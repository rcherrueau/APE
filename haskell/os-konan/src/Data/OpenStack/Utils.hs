module Data.OpenStack.Utils where

import Data.Either (either)

import Data.Aeson as JSON
import Data.Aeson.Types as JSON

import Control.Monad
import Control.Applicative ((<|>))
import qualified Data.HashMap.Lazy as H
import qualified Data.List as L

import Data.Text (Text, isSuffixOf)


-- JSON Combinators

-- | Retrieve the value associated with the path of key of an 'Value'.
-- The result is 'empty' if the path '[Text]' is not present or the
-- value cannot be converted to the desired type.
--
-- Example usage to get the value of "k" {a: {path: {to: k: {}}}}:
-- >>> o .+: ["a", "path", "to", "k"]
(.+:) :: (FromJSON a) => JSON.Value -> [Text] -> JSON.Parser a
(.+:) v t = parseJSON <=< foldM ((maybe err pure .) . lookupE) v $ t
  where
    err :: JSON.Parser a
    err = fail $ "No key path for " ++ show t ++ " in " ++ show v

    lookupE :: JSON.Value -> Text -> Maybe JSON.Value
    lookupE (Object v') key = H.lookup key v'
    lookupE _           _   = Nothing

-- | Retrieve the value associated with the ended key of an 'Object'.
-- The result is 'empty' if there is no key ended by 'Text' or the
-- value cannot be converted to the desired type.
--
-- Example usage to get the value of "{wordWithSuffix: {}}":
-- >>> o *.: "Suffix"
(*.:) :: (FromJSON a) => JSON.Object -> Text -> JSON.Parser a
(*.:) o t = parseJSON <=< ((maybe err pure .) . lookupRE) o $ t
  where
    err :: JSON.Parser a
    err = fail $ "No key ended by " ++ show t ++ " in Object"

    lookupRE :: JSON.Object -> Text -> Maybe JSON.Value
    lookupRE o' suffix = do k <- L.find (isSuffixOf suffix) (H.keys o')
                            H.lookup k o'

-- | Retrieve the value associated with the ended key of an 'Object'.
-- The result is 'Nothing' if there is no key ended by 'Text' or
-- 'empty' if the value cannot be converted to the desired type.
--
-- Example usage to get the value of "word-with-suffix":
-- >>> o *.:? "-suffix"
(*.:?) :: (FromJSON a) => JSON.Object -> Text -> JSON.Parser (Maybe a)
(*.:?) o s = (pure . Just <=< (*.: s)) o <|> pure Nothing


-- Others

rightToMaybe :: Either l r -> Maybe r
rightToMaybe = either (const Nothing) Just
