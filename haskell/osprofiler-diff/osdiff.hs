{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import Prelude

import Control.Applicative (empty)
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Aeson
import Data.Aeson.Parser
import Data.Aeson.Types (Parser, parse)
import Data.ByteString.Lazy as BS (readFile)
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
  , params :: HashMap Text Text -- [(Key, Value)]
  } deriving Show

instance FromJSON DBReq where
  parseJSON (Object v) = DBReq <$>
        v .: "statement"
    <*> v .: "params"
  parseJSON _          = empty

data TraceInfo a = TraceInfo
  { project  :: String
  , service  :: String
  , req      :: a
  } deriving Show

parseJSONTInfo :: FromJSON a => TraceType -> Value -> Parser (TraceInfo a)
parseJSONTInfo t o@(Object v) = TraceInfo <$>
        v .: "project"
    <*> v .: "service"
    <*> o .:* (case t of
                 T_WSGI -> [ "meta.raw_payload.wsgi-start", "info", "request" ]
                 T_DB   -> [ "meta.raw_payload.db-start", "info", "db" ])
parseJSONTInfo _ _          = empty

data Trace = Wsgi (TraceInfo HTTPReq) [Trace]
           | DB (TraceInfo DBReq) [Trace]
           deriving Show

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

testHTTPReq = "{\"path\": \"/v3\", \"scheme\": \"http\", \"method\": \"GET\", \"query\": \"\"}"

testDBReq1 = "{\"params\": {}, \"statement\": \"SELECT 1\"}"
testDBReq2 = "{\"params\": {\"project_id_1\": \"b59f058989c24cd28aad3fc1357df339\", \"user_id_1\": \"b8c739fdb5d04d35ae9055393077553f\", \"issued_before_1\": \"2017-03-03T14:14:01.000000\", \"audit_id_1\": \"yVVzGy1XRoiHIj-C7GZRBQ\"}, \"statement\": \"SELECT revocation_event.id AS revocation_event_id, revocation_event.domain_id AS revocation_event_domain_id, revocation_event.project_id AS revocation_event_project_id, revocation_event.user_id AS revocation_event_user_id, revocation_event.role_id AS revocation_event_role_id, revocation_event.trust_id AS revocation_event_trust_id, revocation_event.consumer_id AS revocation_event_consumer_id, revocation_event.access_token_id AS revocation_event_access_token_id, revocation_event.issued_before AS revocation_event_issued_before, revocation_event.expires_at AS revocation_event_expires_at, revocation_event.revoked_at AS revocation_event_revoked_at, revocation_event.audit_id AS revocation_event_audit_id, revocation_event.audit_chain_id AS revocation_event_audit_chain_id \\nFROM revocation_event \\nWHERE revocation_event.issued_before >= %(issued_before_1)s AND (revocation_event.user_id IS NULL OR revocation_event.user_id = %(user_id_1)s) AND (revocation_event.project_id IS NULL OR revocation_event.project_id = %(project_id_1)s) AND (revocation_event.audit_id IS NULL OR revocation_event.audit_id = %(audit_id_1)s)\"}"

testTInfoHttpReq = "{\"exception\": \"None\", \"name\": \"wsgi\", \"service\": \"main\", \"started\": 0, \"meta.raw_payload.wsgi-stop\": {\"info\": {\"project\": null, \"host\": \"contrib-jessie\", \"service\": null}, \"name\": \"wsgi-stop\", \"service\": \"main\", \"timestamp\": \"2017-03-03T14:14:01.013634\", \"trace_id\": \"0b7b497f-eca7-4a1a-8e07-1c731fb88d16\", \"project\": \"keystone\", \"parent_id\": \"88ab1f1c-a9cf-437f-837e-0c14bf986708\", \"base_id\": \"88ab1f1c-a9cf-437f-837e-0c14bf986708\"}, \"finished\": 5, \"project\": \"keystone\", \"host\": \"contrib-jessie\", \"meta.raw_payload.wsgi-start\": {\"info\": {\"project\": null, \"host\": \"contrib-jessie\", \"request\": {\"path\": \"/v3\", \"scheme\": \"http\", \"method\": \"GET\", \"query\": \"\"}, \"service\": null}, \"name\": \"wsgi-start\", \"service\": \"main\", \"timestamp\": \"2017-03-03T14:14:01.008331\", \"trace_id\": \"0b7b497f-eca7-4a1a-8e07-1c731fb88d16\", \"project\": \"keystone\", \"parent_id\": \"88ab1f1c-a9cf-437f-837e-0c14bf986708\", \"base_id\": \"88ab1f1c-a9cf-437f-837e-0c14bf986708\"}}"

testTInfoDBReq = "{\"meta.raw_payload.db-start\": {\"info\": {\"project\": null, \"host\": \"contrib-jessie\", \"db\": {\"params\": {}, \"statement\": \"SELECT 1\"}, \"service\": null}, \"name\": \"db-start\", \"service\": \"main\", \"timestamp\": \"2017-03-03T14:14:01.063490\", \"trace_id\": \"ebc3e79e-74d7-4ea9-8374-ea5f368db916\", \"project\": \"keystone\", \"parent_id\": \"84e863c2-66e0-471b-987f-194e6cf53e97\", \"base_id\": \"88ab1f1c-a9cf-437f-837e-0c14bf986708\"}, \"name\": \"db\", \"service\": \"main\", \"started\": 55, \"finished\": 58, \"project\": \"keystone\", \"meta.raw_payload.db-stop\": {\"info\": {\"project\": null, \"host\": \"contrib-jessie\", \"service\": null}, \"name\": \"db-stop\", \"service\": \"main\", \"timestamp\": \"2017-03-03T14:14:01.067126\", \"trace_id\": \"ebc3e79e-74d7-4ea9-8374-ea5f368db916\", \"project\": \"keystone\", \"parent_id\": \"84e863c2-66e0-471b-987f-194e6cf53e97\", \"base_id\": \"88ab1f1c-a9cf-437f-837e-0c14bf986708\"}, \"host\": \"contrib-jessie\", \"exception\": \"None\"}"

main :: IO ()
main = do
  print (decode testHTTPReq :: Maybe HTTPReq)
  print (decode testDBReq1 :: Maybe DBReq)
  print (decode testDBReq2 :: Maybe DBReq)
  print (decodeWith json' (parse $ parseJSONTInfo T_WSGI) testTInfoHttpReq :: Maybe (TraceInfo HTTPReq))
  print (decodeWith json' (parse $ parseJSONTInfo T_DB) testTInfoDBReq :: Maybe (TraceInfo DBReq))
  json <- BS.readFile "flavor-list-real.json"
  print (decode json :: Maybe [Trace])
  putStrLn "lala"
