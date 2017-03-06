{-# LANGUAGE OverloadedStrings #-}

module Data.OSPDiff where

import Prelude

import Control.Applicative (empty)
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Aeson
import Data.Aeson.Parser
import Data.Aeson.Types (Parser, parse)
import Data.Text (Text)
import Data.HashMap.Lazy as H (HashMap, lookup)

-- http://stackoverflow.com/a/18003411
lookupE :: Value -> Text -> Either String Value
lookupE (Object v) key = case H.lookup key v of
  Nothing -> Left $ "key " ++ show key ++ " not present"
  Just v' -> Right v'
lookupE _          _   = Left "not an object"

(.:*) :: (FromJSON a) => Value -> [Text] -> Parser a
(.:*) hm = parseJSON <=< foldM ((either fail return .) . lookupE) hm

data TraceType = T_DB | T_WSGI

traceType :: Object -> TraceType
traceType v = case H.lookup "info" v of
  Just (Object v') -> case H.lookup "name" v' of
    Just (String n) -> case n of
      "wsgi" -> T_WSGI
      "db"   -> T_DB

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
  , params :: HashMap String String -- [(Key, Value)]
  } deriving (Show, Eq)

instance FromJSON DBReq where
  parseJSON (Object v) = DBReq <$>
        v .: "statement"
    <*> v .: "params"
  parseJSON _          = empty

data (Show a, Eq a) => TraceInfo a = TraceInfo
  { project  :: String
  , service  :: String
  , req      :: a
  } deriving (Show, Eq)

parseJSONTInfo :: (Show a, Eq a, FromJSON a) => TraceType -> Value -> Parser (TraceInfo a)
parseJSONTInfo t o@(Object v) = TraceInfo <$>
        v .: "project"
    <*> v .: "service"
    <*> o .:* (case t of
                 T_WSGI -> [ "meta.raw_payload.wsgi-start", "info", "request" ]
                 T_DB   -> [ "meta.raw_payload.db-start", "info", "db" ])
parseJSONTInfo _ _          = empty

data Trace = Wsgi (TraceInfo HTTPReq) [Trace]
           | DB (TraceInfo DBReq) [Trace]
           deriving (Show, Eq)

instance FromJSON Trace where
  parseJSON (Object v) = do
    c <- parseJSON =<< v .: "children"
    -- TODO: make it fail rather than return an (Object v)
    let v' = fromMaybe (Object v) (H.lookup "info" v)
    case traceType v of
      T_WSGI -> do
        i <- parseJSONTInfo (traceType v) v'
        pure $ Wsgi i c
      T_DB   -> do
        i <- parseJSONTInfo (traceType v) v'
        pure $ DB i c
  parseJSON _          = empty

-- testHTTPReq = "{\"path\": \"/v3\", \"scheme\": \"http\", \"method\": \"GET\", \"query\": \"\"}"

-- main :: IO ()
-- main = do
--   print (decode testHTTPReq :: Maybe HTTPReq)
--   print (decode testDBReq1 :: Maybe DBReq)
--   print (decode testDBReq2 :: Maybe DBReq)
--   print (decodeWith json' (parse $ parseJSONTInfo T_WSGI) testTInfoHttpReq :: Maybe (TraceInfo HTTPReq))
--   print (decodeWith json' (parse $ parseJSONTInfo T_DB) testTInfoDBReq :: Maybe (TraceInfo DBReq))
--   json <- BS.readFile "flavor-list-real.json"
--   print (decode json :: Maybe [Trace])
--   putStrLn "lala"
