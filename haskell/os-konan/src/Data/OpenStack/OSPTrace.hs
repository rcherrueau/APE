{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.OpenStack.OSPTrace (
  -- Parses an OpenStack OSProfiler JSON trace. See,
  -- https://docs.openstack.org/osprofiler/latest/
  HTTP(..)
, HTTPReq(..)
, DBReq(..)
, PythonReq(..)
, TraceInfo(..)
, TraceType(..)
, OSPTrace
, decodeTrace
, eitherDecodeTrace
, Data.OpenStack.OSPTrace.parse
, save
, load
  ) where

import GHC.Generics (Generic)
import Control.Applicative ((<|>))
import Control.Monad ((<=<))
-- import Data.Time (UTCTime)
import qualified Data.List as L (isSuffixOf)
import qualified Data.Tree as T (Tree(..))
import qualified Data.ByteString as BS (readFile, writeFile)
import qualified Data.ByteString.Lazy as BSL (ByteString, readFile)

import Data.Aeson as JSON
import Data.Aeson.Internal as JSON (JSONPath, iparse, formatError)
import Data.Aeson.Types as JSON (Parser, parse, listParser, typeMismatch)
import Data.Aeson.Parser as JSON (decodeWith, eitherDecodeWith, json)
import qualified Language.SQL.SimpleSQL.Parser as SQL (parseQueryExpr)
import qualified Language.SQL.SimpleSQL.Syntax as SQL (QueryExpr, Dialect(MySQL))
import Data.Store as Store (Store, encode, decode, PeekException)

import Data.OpenStack.Sql()
import Data.OpenStack.Utils


-- ADTs

-- | rfc2616
-- See https://ietf.org/rfc/rfc2616
data HTTP = Options | Get | Head | Post | Put | Delete | Trace | Connect
          deriving (Generic, Show, Eq)

data HTTPReq = HTTPReq
  { path   :: String
  , method :: HTTP
  , query  :: String
  } deriving (Generic, Show, Eq)

data DBReq = DBReq
  { stmt   :: Maybe SQL.QueryExpr
  -- , params :: Value -- TODO: Store JSON.Value then uncomment
  } deriving (Generic, Show, Eq)

data PythonReq = PythonReq
  { function :: String
  , args     :: String
  , kwargs   :: String
  } deriving (Generic, Show, Eq)

data TraceInfo a = TraceInfo
  { project  :: String
  , service  :: String
  , host     :: String
  , start    :: String
  , stop     :: Maybe String
  , req      :: a
  } deriving (Generic, Show, Eq)


-- | An OSProfiler Trace represented as a Haskell value.
data TraceType = Wsgi (TraceInfo HTTPReq)
               | DB (TraceInfo DBReq)
               | RPC (TraceInfo PythonReq)
               | ComputeApi (TraceInfo PythonReq)
               | NovaImage (TraceInfo PythonReq)
               | NovaVirt (TraceInfo PythonReq)
               | NeutronApi (TraceInfo PythonReq)
               | Root
               deriving (Generic, Show, Eq)

type OSPTrace = T.Tree TraceType


-- Utils

-- | 'Parser' for the json top 'OSPTrace'.
parserTopTrace :: Value -> Parser OSPTrace
parserTopTrace v = T.Node Root <$> parseChildren v
  where
    parseTrace :: Value -> Parser OSPTrace
    parseTrace v' = T.Node <$> parseJSON v' <*> parseChildren v'

    parseChildren :: Value -> Parser [OSPTrace]
    parseChildren (Object o') = (listParser parseTrace <=< (.: "children")) o'
    parseChildren v'           = typeMismatch "[a]" v'

class (FromJSON a, Show a, Eq a) => ReqPath a where
  reqPath :: Value -> Parser a

-- ReqPath instances
instance ReqPath HTTPReq where
  reqPath = (.:+ [ "meta.raw_payload.wsgi-start", "info", "request" ])

instance ReqPath DBReq where
  reqPath v
    =   v .:+ [ "meta.raw_payload.db-start", "info", "db" ]
    <|> v .:+ [ "meta.raw_payload.neutron.db-start", "info", "db" ]

instance ReqPath PythonReq where
  reqPath v =
    let rpc        = v .:+ [ "meta.raw_payload.rpc-start",         "info", "function" ]
        computeApi = v .:+ [ "meta.raw_payload.compute_api-start", "info", "function" ]
        novaImage  = v .:+ [ "meta.raw_payload.nova_image-start",  "info", "function" ]
        novaVirt   = v .:+ [ "meta.raw_payload.vif_driver-start",  "info", "function" ]
        neutronApi = v .:+ [ "meta.raw_payload.neutron_api-start", "info", "function" ]
    in rpc <|> computeApi <|> novaImage <|> novaVirt <|> neutronApi

-- FronJSON instances
instance FromJSON HTTP where
  parseJSON (String s) = case s of
    "OPTIONS" -> pure Options
    "GET"     -> pure Get
    "HEAD"    -> pure Head
    "POST"    -> pure Post
    "PUT"     -> pure Put
    "DELETE"  -> pure Delete
    "TRACE"   -> pure Trace
    "CONNECT" -> pure Connect
    _         -> fail $ show s ++ " is not an HTTP verb"
  parseJSON v          = typeMismatch "HTTP Verb" v

instance FromJSON HTTPReq where
  parseJSON (Object o) = HTTPReq <$>
        o .: "path"
    <*> o .: "method"
    <*> o .: "query"
  parseJSON v          = typeMismatch "HTTPReq" v

instance FromJSON DBReq where
  parseJSON (Object o) = do
    sql    <- (parseSQL <=< (.: "statement")) o
    -- params <- o .: "params"  -- TODO: Store JSON.Value then uncomment
    -- pure (DBReq sql params)
    pure (DBReq sql)
    where
      parseSQL :: String -> JSON.Parser (Maybe SQL.QueryExpr)
      parseSQL = pure . rightToMaybe . (SQL.parseQueryExpr SQL.MySQL "" Nothing)
  parseJSON v          = typeMismatch "DBReq" v

instance FromJSON PythonReq where
  parseJSON (Object o) = PythonReq <$>
        o .: "name"
    <*> o .: "args"
    <*> o .: "kwargs"
  parseJSON v          = typeMismatch "PythonReq" v

instance (ReqPath a, FromJSON a) => FromJSON (TraceInfo a) where
  parseJSON v@(Object o) = TraceInfo <$>
        o .: "project"
    <*> o .: "service"
    <*> o .: "host"
    <*> ((.: "timestamp") <=< (.:*-  "-start")) o
    <*> (maybe (pure Nothing) (.: "timestamp") <=< (.:*-? "-stop"))  o
    <*> reqPath v
  parseJSON v            = typeMismatch "TraceInfo" v

instance FromJSON TraceType where
  parseJSON (Object o)
    =   traceType o "wsgi"        *> (Wsgi       <$> o .: "info")
    <|> traceType o "db"          *> (DB         <$> o .: "info")
    <|> traceType o "rpc"         *> (RPC        <$> o .: "info")
    <|> traceType o "compute_api" *> (ComputeApi <$> o .: "info")
    <|> traceType o "nova_image"  *> (NovaImage  <$> o .: "info")
    <|> traceType o "vif_driver"  *> (NovaVirt   <$> o .: "info")
    <|> traceType o "neutron_api" *> (NeutronApi <$> o .: "info")
    where
      traceType :: Object -> String -> Parser String
      traceType o n = do
        o' <- o  .: "info" :: Parser Object
        n' <- o' .: "name" :: Parser String
        if n `L.isSuffixOf` n'
          then pure n'
          else fail $ show n' ++ " is not a valid TraceType"
  parseJSON v          = typeMismatch "TraceType" v

instance JSON.ToJSON HTTP
instance JSON.ToJSON HTTPReq
instance JSON.ToJSON DBReq
instance JSON.ToJSON PythonReq
instance (JSON.ToJSON a) => JSON.ToJSON (TraceInfo a)
instance JSON.ToJSON TraceType

instance Store HTTP
instance Store HTTPReq
instance Store DBReq
instance Store PythonReq
instance (Store a) => Store (TraceInfo a)
instance Store TraceType
instance (Store a) => Store (T.Tree a)


-- API

-- | Decodes data into an `OSPTrace`.
decodeTrace :: BSL.ByteString -> Maybe OSPTrace
decodeTrace = decodeWith json (JSON.parse parserTopTrace)

-- | Either decodes data into an `OSPTrace` or returns an error if
-- something gets wrong.
eitherDecodeTrace :: BSL.ByteString -> Either String OSPTrace
eitherDecodeTrace = eitherFormatError . eitherDecodeWith json (iparse parserTopTrace)
  where
    eitherFormatError :: Either (JSONPath, String) a -> Either String a
    eitherFormatError = either (Left . uncurry formatError) Right

-- | Parse an os-profiler JSON file into an `OSPTrace`
parse :: String -> IO (Either String OSPTrace)
parse = pure . eitherDecodeTrace <=< BSL.readFile

-- | Saves an `OSPTrace` on the disk for later use.
save :: OSPTrace  -- ^ OSPTrace to save
        -> String    -- ^ Binary output file path
        -> IO ()
save t fo = BS.writeFile fo $ Store.encode t

-- | Loads a previously saved `OSPTrace` file.
load :: String        -- ^ Binary Input file
        -> IO (Either PeekException OSPTrace)
load = (pure . Store.decode) <=< BS.readFile
