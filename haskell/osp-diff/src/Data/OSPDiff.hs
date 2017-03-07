{-# LANGUAGE OverloadedStrings #-}

module Data.OSPDiff where

import Prelude

import Control.Applicative (empty)
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.HashMap.Lazy as H (HashMap, lookup)

-- http://stackoverflow.com/a/18003411
lookupE :: Value -> Text -> Either String Value
lookupE (Object v) key = case H.lookup key v of
  Nothing -> Left $ "key " ++ show key ++ " not present"
  Just v' -> Right v'
lookupE _          _   = Left "not an object"

(.:*) :: (FromJSON a) => Value -> [Text] -> Parser a
(.:*) v = parseJSON <=< foldM ((either fail return .) . lookupE) v

data HTTP = Post | Get | Update | Delete deriving (Show, Eq)

instance FromJSON HTTP where
  parseJSON (String v) = case v of
    "POST"   -> pure Post
    "GET"    -> pure Get
    "UPDATE" -> pure Update
    "DELETE" -> pure Delete
    _        -> empty
  parseJSON _          = empty

data HTTPReq = HTTPReq
  { path   :: String
  , method :: HTTP
  , query  :: String
  } deriving (Show, Eq)

instance FromJSON HTTPReq where
  parseJSON (Object v) = HTTPReq <$>
        v .: "path"
    <*> v .: "method"
    <*> v .: "query"
  parseJSON _          = empty

data DBReq = DBReq
  { stmt   :: String
  , params :: HashMap String Value
  } deriving (Show, Eq)

instance FromJSON DBReq where
  parseJSON (Object v) = DBReq <$>
        v .: "statement"
    <*> v .: "params"
  parseJSON _          = empty

class (FromJSON a, Show a, Eq a) => ReqPath a where
  reqPath :: Value -> Parser a

instance ReqPath HTTPReq where
  reqPath = flip (.:*) [ "meta.raw_payload.wsgi-start", "info", "request" ]

instance ReqPath DBReq where
  reqPath = flip (.:*) [ "meta.raw_payload.db-start", "info", "db" ]

data (Show a, Eq a) => TraceInfo a = TraceInfo
  { project  :: String
  , service  :: String
  , req      :: a
  } deriving (Show, Eq)

instance (ReqPath a, FromJSON a) => FromJSON (TraceInfo a) where
  parseJSON o@(Object v) = TraceInfo <$>
        v .: "project"
    <*> v .: "service"
    <*> reqPath o
  parseJSON _            = empty

data Trace = Wsgi (TraceInfo HTTPReq) [Trace]
           | DB (TraceInfo DBReq) [Trace]
           deriving (Show, Eq)

instance FromJSON Trace where
  parseJSON (Object v) = do
    c  <- parseJSON =<< v  .: "children" -- :: Parser [Trace]
    v' <- parseJSON =<< v  .: "info"     -- :: Parser Object
    n  <- parseJSON =<< v' .: "name"     :: Parser String
    case n of
      "wsgi" -> do
        i <- parseJSON (Object v')
        pure $ Wsgi i c
      "db"   -> do
        i <- parseJSON (Object v')
        pure $ DB i c
      _      -> empty
  parseJSON _          = empty
