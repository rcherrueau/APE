{-# LANGUAGE OverloadedStrings #-}

module Data.OSPDiff where

import Prelude

import Control.Applicative (empty, (<|>))
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.HashMap.Lazy as H (lookup)


-- Utils
(.:*) :: (FromJSON a) => Value -> [Text] -> Parser a
(.:*) v = parseJSON <=< foldM ((maybe empty return .) . lookupE) v
  where
    lookupE :: Value -> Text -> Maybe Value
    lookupE (Object v') key = H.lookup key v'
    lookupE _           _   = Nothing


data HTTP = Post | Get | Update | Delete deriving (Show, Eq)

data HTTPReq = HTTPReq
  { path   :: String
  , method :: HTTP
  , query  :: String
  } deriving (Show, Eq)

data DBReq = DBReq
  { stmt   :: String
  , params :: Value
  } deriving (Show, Eq)

data PythonReq = PythonReq
  { name   :: String
  , args   :: String
  , kwargs :: String
  } deriving (Show, Eq)

data (Show a, Eq a) => TraceInfo a = TraceInfo
  { project  :: String
  , service  :: String
  , req      :: a
  } deriving (Show, Eq)

data Trace = Wsgi (TraceInfo HTTPReq) [Trace]
           | DB (TraceInfo DBReq) [Trace]
           | RPC (TraceInfo PythonReq) [Trace]
           | ComputeApi (TraceInfo PythonReq) [Trace]
           | NovaImage (TraceInfo PythonReq) [Trace]
           | NovaVirt (TraceInfo PythonReq) [Trace]
           | NeutronApi (TraceInfo PythonReq) [Trace]
             deriving (Show, Eq)


instance FromJSON HTTP where
  parseJSON (String s) = case s of
    "POST"   -> pure Post
    "GET"    -> pure Get
    "UPDATE" -> pure Update
    "DELETE" -> pure Delete
    _        -> empty
  parseJSON _          = empty

instance FromJSON HTTPReq where
  parseJSON (Object o) = HTTPReq <$>
        o .: "path"
    <*> o .: "method"
    <*> o .: "query"
  parseJSON _          = empty

instance FromJSON DBReq where
  parseJSON (Object o) = DBReq <$>
        o .: "statement"
    <*> o .: "params"
  parseJSON _          = empty

instance FromJSON PythonReq where
  parseJSON (Object o) = PythonReq <$>
        o .: "name"
    <*> o .: "args"
    <*> o .: "kwargs"
  parseJSON _          = empty

instance (ReqPath a, FromJSON a) => FromJSON (TraceInfo a) where
  parseJSON v@(Object o) = TraceInfo <$>
        o .: "project"
    <*> o .: "service"
    <*> reqPath v
  parseJSON _            = empty

instance FromJSON Trace where
  parseJSON (Object o) = do
    c  <- o  .: "children" -- :: Parser [Trace]
    o' <- o  .: "info"     -- :: Parser Object
    n  <- o' .: "name"     :: Parser String
    case n of
      "wsgi" -> do
        i <- parseJSON (Object o')
        pure $ Wsgi i c
      "db"   -> do
        i <- parseJSON (Object o')
        pure $ DB i c
      "rpc"  -> do
        i <- parseJSON (Object o')
        pure $ RPC i c
      "compute_api"  -> do
        i <- parseJSON (Object o')
        pure $ ComputeApi i c
      "nova_image"  -> do
        i <- parseJSON (Object o')
        pure $ NovaImage i c
      "vif_driver"  -> do
        i <- parseJSON (Object o')
        pure $ NovaVirt i c
      "neutron_api"  -> do
        i <- parseJSON (Object o')
        pure $ NeutronApi i c
      _      -> empty
  parseJSON _          = empty


class (FromJSON a, Show a, Eq a) => ReqPath a where
  reqPath :: Value -> Parser a

instance ReqPath HTTPReq where
  reqPath = flip (.:*) [ "meta.raw_payload.wsgi-start", "info", "request" ]

instance ReqPath DBReq where
  reqPath = flip (.:*) [ "meta.raw_payload.db-start", "info", "db" ]

instance ReqPath PythonReq where
  reqPath v =
    let rpc        = v .:* [ "meta.raw_payload.rpc-start", "info", "function" ]
        computeApi = v .:* [ "meta.raw_payload.compute_api-start", "info", "function" ]
        novaImage  = v .:* [ "meta.raw_payload.nova_image-start", "info", "function" ]
        novaVirt   = v .:* [ "meta.raw_payload.vif_driver-start", "info", "function" ]
        neutronApi = v .:* [ "meta.raw_payload.neutron_api-start", "info", "function" ]
    in rpc <|> computeApi <|> novaImage <|> novaVirt <|> neutronApi
