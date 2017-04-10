module Main (main) where

import Prelude

import qualified Data.ByteString.Lazy as BS (pack,readFile)
import qualified Data.ByteString.Internal as BI (c2w)
import qualified Data.ByteString.Lazy.Internal as BLI (ByteString)

import qualified Data.HashMap.Lazy as H (empty, fromList)

import Test.HUnit
import System.Exit (exitSuccess, exitFailure)

import Data.OSPUtils.Trace
import Data.Aeson (encode, decode, FromJSON, Value(..))
import Data.Text (pack)


-- Utils
mkStrValue :: String -> Value
mkStrValue = String . pack

assertOSP :: (Show a, Eq a, FromJSON a) => String -> a -> Assertion
assertOSP json res = assertEqual json (Just res) (decode (packJSON json))
  where
    packJSON :: String -> BLI.ByteString
    packJSON s = BS.pack $ map BI.c2w s

assertParsable :: String -> Assertion
assertParsable file = do
  json <- BS.readFile file
  let res  = eitherDecodeTrace json :: Either String Trace
  let msg  = "For " ++ file ++ either (" -- " ++)
                                      (const " -- OK") res
  let theTest = either (const False) (const True) res
  assertBool msg theTest


-- HTTP Code Parsing Tests
testsHTTP :: Test
testsHTTP = TestLabel "HTTP Code Parsing" $
  TestList
  [ TestCase $ assertOSP "\"POST\""   Post
  , TestCase $ assertOSP "\"GET\""    Get
  , TestCase $ assertOSP "\"UPDATE\"" Update
  , TestCase $ assertOSP "\"DELETE\"" Delete
  ]


-- HTTP Request Parsing Tests
httpreq :: String
httpreq = unwords [ "{\"path\": \"/v3\","
                  , " \"scheme\": \"http\","
                  , " \"method\": \"GET\","
                  , " \"query\": \"\"}" ]

httpreqquery :: String
httpreqquery = unwords [ "{\"path\": \"/v2/images\","
                       , " \"scheme\": \"http\","
                       , " \"method\": \"POST\", "
                       , " \"query\": \"limit=20\"}"]

testsHTTPReq :: Test
testsHTTPReq = TestLabel "HTTP Request Parsing" $ TestList
  [ TestCase $ assertOSP httpreq
                         (HTTPReq "/v3" Get "")
  , TestCase $ assertOSP httpreqquery
                         (HTTPReq "/v2/images" Post "limit=20")
  ]


-- DB Request Parsing Tests
dbreq :: String
dbreq = "{\"params\": {}, \"statement\": \"SELECT 1\"}"

dbreqparams :: String
dbreqparams = unwords [ "{\"params\": "
                      ,    "{\"project_id_1\": \"b59f058989c24cd28aad3fc1357df339\","
                      ,    " \"user_id_1\": \"b8c739fdb5d04d35ae9055393077553f\","
                      ,    " \"issued_before_1\": \"2017-03-03T14:14:01.000000\","
                      ,    " \"audit_id_1\": \"yVVzGy1XRoiHIj-C7GZRBQ\"},"
                      , " \"statement\": \"SELECT 1\"}"]

testsDBReq :: Test
testsDBReq = TestLabel "DB Request Parsing" $ TestList
  [ TestCase $ assertOSP dbreq (DBReq "SELECT 1" (Object H.empty))
  , TestCase $ assertOSP dbreqparams
      (DBReq "SELECT 1"
        (Object $ H.fromList [ (pack "project_id_1", mkStrValue "b59f058989c24cd28aad3fc1357df339")
                             , (pack "user_id_1", mkStrValue "b8c739fdb5d04d35ae9055393077553f")
                             , (pack "issued_before_1", mkStrValue "2017-03-03T14:14:01.000000")
                             , (pack "audit_id_1", mkStrValue "yVVzGy1XRoiHIj-C7GZRBQ")]))
  ]


-- Python Request Parsing Test
cptPythonReq :: String
cptPythonReq = "{\"args\": \"(<nova.compute.api.API object at 0x7feed4bca690>, <nova.context.RequestContext object at 0x7feed394cad0>, u'1fbf7f34-0aff-41b2-9611-4aaba19cca3a')\", \"name\": \"nova.compute.api.API.get\", \"kwargs\": \"{'expected_attrs': ['flavor', 'info_cache', 'metadata', 'numa_topology', 'pci_devices']}\"}"

testsPythonReq :: Test
testsPythonReq = TestLabel "Python Request Parsing" $ TestList
  [ TestCase $ assertOSP cptPythonReq (PythonReq "nova.compute.api.API.get" "(<nova.compute.api.API object at 0x7feed4bca690>, <nova.context.RequestContext object at 0x7feed394cad0>, u'1fbf7f34-0aff-41b2-9611-4aaba19cca3a')" "{'expected_attrs': ['flavor', 'info_cache', 'metadata', 'numa_topology', 'pci_devices']}")
  ]


-- Trace Info Parsing Test
mkTinfohttp :: String -> String
mkTinfohttp s = unwords [ "{\"exception\": \"None\","
                         , "\"name\": \"wsgi\", "
                         , "\"service\": \"main\", "
                         , "\"started\": 0, "
                         , "\"meta.raw_payload.wsgi-stop\": "
                         ,   "{\"info\": "
                         ,     "{\"project\": null, \"host\": \"contrib-jessie\", \"service\": null}, "
                         ,   " \"name\": \"wsgi-stop\", "
                         ,   " \"service\": \"main\", "
                         ,   " \"timestamp\": \"2017-03-03T14:14:01.013634\", "
                         ,   " \"trace_id\": \"0b7b497f-eca7-4a1a-8e07-1c731fb88d16\", "
                         ,   " \"project\": \"keystone\", "
                         ,   " \"parent_id\": \"88ab1f1c-a9cf-437f-837e-0c14bf986708\", "
                         ,   " \"base_id\": \"88ab1f1c-a9cf-437f-837e-0c14bf986708\"}, "
                         , "\"finished\": 5, "
                         , "\"project\": \"keystone\", "
                         , "\"host\": \"contrib-jessie\", "
                         , "\"meta.raw_payload.wsgi-start\": "
                         ,   "{\"info\": "
                         ,     "{\"project\": null, "
                         ,      "\"host\": \"contrib-jessie\", "
                         ,      "\"request\": " , s , ", "
                         ,      "\"service\": null}, "
                         ,   " \"name\": \"wsgi-start\", "
                         ,   " \"service\": \"main\", "
                         ,   " \"timestamp\": \"2017-03-03T14:14:01.008331\", "
                         ,   " \"trace_id\": \"0b7b497f-eca7-4a1a-8e07-1c731fb88d16\", "
                         ,   " \"project\": \"keystone\", "
                         ,   " \"parent_id\": \"88ab1f1c-a9cf-437f-837e-0c14bf986708\", "
                         ,   " \"base_id\": \"88ab1f1c-a9cf-437f-837e-0c14bf986708\"}}" ]

mkTinfodb :: String -> String
mkTinfodb s = unwords [ "{\"meta.raw_payload.db-start\": "
                      ,   "{\"info\": "
                      ,     "{\"project\": null, "
                      ,     " \"host\": \"contrib-jessie\", "
                      ,     " \"db\": ", s , ", "
                      ,     " \"service\": null}, "
                      ,   " \"name\": \"db-start\", "
                      ,   " \"service\": \"main\", "
                      ,   " \"timestamp\": \"2017-03-03T14:14:01.063490\", "
                      ,   " \"trace_id\": \"ebc3e79e-74d7-4ea9-8374-ea5f368db916\", "
                      ,   " \"project\": \"keystone\", "
                      ,   " \"parent_id\": \"84e863c2-66e0-471b-987f-194e6cf53e97\", "
                      ,   " \"base_id\": \"88ab1f1c-a9cf-437f-837e-0c14bf986708\"}, "
                      , " \"name\": \"db\", "
                      , " \"service\": \"main\", "
                      , " \"started\": 55, "
                      , " \"finished\": 58, "
                      , " \"project\": \"keystone\", "
                      , " \"meta.raw_payload.db-stop\": "
                      ,   "{\"info\": {\"project\": null, "
                      ,     " \"host\": \"contrib-jessie\", "
                      ,     " \"service\": null}, "
                      ,   " \"name\": \"db-stop\", "
                      ,   " \"service\": \"main\", "
                      ,   " \"timestamp\": \"2017-03-03T14:14:01.067126\", "
                      ,   " \"trace_id\": \"ebc3e79e-74d7-4ea9-8374-ea5f368db916\", "
                      ,   " \"project\": \"keystone\", "
                      ,   " \"parent_id\": \"84e863c2-66e0-471b-987f-194e6cf53e97\", "
                      ,   " \"base_id\": \"88ab1f1c-a9cf-437f-837e-0c14bf986708\"}, "
                      , " \"host\": \"contrib-jessie\", "
                      , " \"exception\": \"None\"}" ]

tinfohttpreq :: String
tinfohttpreq = mkTinfohttp httpreq

tinfohttpreqquery :: String
tinfohttpreqquery = mkTinfohttp httpreqquery

tinfodbreq :: String
tinfodbreq = mkTinfodb dbreq

tinfodbreqparams :: String
tinfodbreqparams = mkTinfodb dbreqparams

testsTraceInfo :: Test
testsTraceInfo = TestLabel "TraceInfo Parsing" $ TestList
  [ TestCase $ assertOSP tinfohttpreq
                         (TraceInfo "keystone" "main"
                           "2017-03-03T14:14:01.008331" (Just "2017-03-03T14:14:01.013634")
                           (HTTPReq "/v3" Get ""))
  , TestCase $ assertOSP tinfohttpreqquery
                         (TraceInfo "keystone" "main"
                           "2017-03-03T14:14:01.008331" (Just "2017-03-03T14:14:01.013634")
                           (HTTPReq "/v2/images" Post "limit=20"))
  , TestCase $ assertOSP tinfodbreq
                         (TraceInfo "keystone" "main"
                           "2017-03-03T14:14:01.063490" (Just "2017-03-03T14:14:01.067126")
                           (DBReq "SELECT 1" (Object H.empty)))
  , TestCase $ assertOSP tinfodbreqparams
                         (TraceInfo "keystone" "main"
                           "2017-03-03T14:14:01.063490" (Just "2017-03-03T14:14:01.067126")
                           (DBReq "SELECT 1"
        (Object $ H.fromList [ (pack "project_id_1", mkStrValue "b59f058989c24cd28aad3fc1357df339")
                             , (pack "user_id_1", mkStrValue "b8c739fdb5d04d35ae9055393077553f")
                             , (pack "issued_before_1", mkStrValue "2017-03-03T14:14:01.000000")
                             , (pack "audit_id_1", mkStrValue "yVVzGy1XRoiHIj-C7GZRBQ") ])))
  ]


-- Trace Parsing Test
testsTraces :: Test
testsTraces = TestLabel "Trace Parsing" $ TestList $
  map (TestCase . assertParsable)
  [ "test/rsc/flavor-list-fake.json"
  , "test/rsc/flavor-list-real.json"
  , "test/rsc/hypervisor-list-fake.json"
  , "test/rsc/hypervisor-list-real.json"
  , "test/rsc/image-list-fake.json"
  , "test/rsc/image-list-real.json"
  , "test/rsc/server-create-fake.json"
  , "test/rsc/server-create-real.json"
  ]


-- Test Main
testsAll :: Test
testsAll = TestList [ testsHTTP, testsHTTPReq, testsDBReq,
                      testsPythonReq, testsTraceInfo, testsTraces ]

main :: IO ()
main = do
  c <- runTestTT testsAll
  if errors c + failures c == 0
    then exitSuccess
    else exitFailure
