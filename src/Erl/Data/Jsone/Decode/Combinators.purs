module Erl.Data.Jsone.Decode.Combinators
  ( getField
  , (.?)
  , getFieldOptional
  , (.??)
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (findMap)
import Data.Maybe (Maybe(..), maybe)
import Erl.Data.Jsone (JObject(..))
import Erl.Data.Jsone.Decode.Class (class DecodeJson, decodeJson)
import Erl.Data.Tuple (Tuple2, uncurry2)

getField :: forall a. DecodeJson a => JObject -> String -> Either String a
getField (JObject o) s =
  maybe
    (Left $ "Expected field " <> show s)
    decodeJson
    (findMap (match s) o)

infix 7 getField as .?

getFieldOptional :: forall a. DecodeJson a => JObject -> String -> Either String (Maybe a)
getFieldOptional (JObject o) s =
  maybe
    (pure Nothing)
    decode
    (findMap (match s) o)
  where
    decode json = Just <$> decodeJson json

infix 7 getFieldOptional as .??

match :: forall a b. (Eq a) => a -> Tuple2 a b -> Maybe b
match s = uncurry2 $ (if _ then Just else const Nothing) <<< (==) s
