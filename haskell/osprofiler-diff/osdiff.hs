{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import Prelude

import Control.Applicative (empty)
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.HashMap.Lazy as H (lookup)

-- http://stackoverflow.com/a/18003411
lookupE :: Value -> Text -> Either String Value
lookupE (Object v) key = case H.lookup key v of
  Nothing -> Left $ "key " ++ show key ++ " not present"
  Just v' -> Right v'
lookupE _          _   = Left "not an object"

(.:*) :: (FromJSON a) => Object -> [Text] -> Parser a
-- TODO:
-- (.:*) hm = parseJSON <=< foldM ((either fail return .) . lookupE) hm
(.:*) v = undefined

data TraceType = T_DB | T_WSGI

traceType :: Object -> TraceType
traceType v = case H.lookup "info" v of
  Just (Object v') -> case H.lookup "name" v' of
    Just (String n) -> case n of
      "wsgi" -> T_WSGI
      "db"   -> T_DB

data HTTP = Post | Get | Update | Delete
            deriving (Show, Eq)

instance FromJSON HTTP where
  parseJSON (String v) = case v of
    "POST"   -> pure Post
    "GET"    -> pure Get
    "UPDATE" -> pure Update
    "DELETE" -> pure Delete
    _        -> empty
  parseJSON _          = empty

data HTTPReq = HTTPReq
  { path   :: Text
  , method :: HTTP
  , query  :: Text
  } deriving Show

instance FromJSON HTTPReq where
  parseJSON (Object v) = HTTPReq <$>
        v .: "path"
    <*> v .: "method"
    <*> v .: "query"
  parseJSON _          = empty

data DBReq = DBReq
  { stmt   :: Text
  , params :: [(Text, Text)] -- [(Key, Value)]
  } deriving Show

instance FromJSON DBReq where
  parseJSON (Object v) = DBReq <$>
        v .: "stmt"
    <*> v .: "params"
  parseJSON _          = empty

data TraceInfo a = TraceInfo
  { project  :: String
  , service  :: String
  , req      :: a
  } deriving Show

parseJSONTInfo :: FromJSON a => TraceType -> Value -> Parser (TraceInfo a)
parseJSONTInfo t (Object v) = TraceInfo <$>
        v .: "project"
    <*> v .: "service"
    <*> v .:* (case t of
                 T_WSGI -> [ "meta.raw_payload.wsgi-start", "info", "request" ]
                 T_DB   -> [ "meta.raw_payload.db-start", "info", "db" ])
parseJSONTInfo _ _          = empty

data Trace = Wsgi (TraceInfo HTTPReq) [Trace]
           | DB (TraceInfo DBReq) [Trace]
           deriving Show

instance FromJSON Trace where
  parseJSON (Object v) = do
    c <- parseJSON =<< v .: "children"
    case traceType v of
      T_WSGI -> do
        i <- parseJSONTInfo (traceType v) (Object v)
        pure $ Wsgi i c
      T_DB   -> do
        i <- parseJSONTInfo (traceType v) (Object v)
        pure $ DB i c
  parseJSON _          = empty

main :: IO ()
main = putStrLn "lala"
