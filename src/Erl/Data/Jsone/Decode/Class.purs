module Erl.Data.Jsone.Decode.Class
  ( class DecodeJson
  , decodeJson
  ) where

import Prelude
import Data.Array (fromFoldable, null, uncons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (findMap)
import Data.Int (fromNumber)
import Data.Maybe (maybe, Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Erl.Data.Binary (Binary, bin, stringFromBin)
import Erl.Data.Jsone (JArray, JObject(..), Json, foldJsonBoolean, foldJsonNull, foldJsonNumber, foldJsonString, isNull, toArray, toObject, toString)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple2, Tuple3, tuple2, tuple3, uncurry2)

class DecodeJson a where
  decodeJson :: Json -> Either String a

instance decodeJsonMaybe :: DecodeJson a => DecodeJson (Maybe a) where
  decodeJson j
    | isNull j = pure Nothing
    | otherwise = Just <$> decodeJson j

instance decodeJsonTuple :: (DecodeJson a, DecodeJson b) => DecodeJson (Tuple a b) where
  decodeJson j = decodeJson j >>= f
    where
    f l = case uncons l of
      Just { head: a, tail } -> case uncons tail of
        Just { head: b, tail: tail' } | null tail' -> Tuple <$> decodeJson a <*> decodeJson b
        _ -> Left "Couldn't decode Tuple"
      _ -> Left "Couldn't decode Tuple"

instance decodeJsonTuple2 :: (DecodeJson a, DecodeJson b) => DecodeJson (Tuple2 a b) where
  decodeJson j = decodeJson j >>= f
    where
    f l = case uncons l of
      Just { head: a, tail } -> case uncons tail of
        Just { head: b, tail: tail' } | null tail' -> tuple2 <$> decodeJson a <*> decodeJson b
        _ -> Left "Couldn't decode Tuple"
      _ -> Left "Couldn't decode Tuple"


instance decodeJsonTuple3 :: (DecodeJson a, DecodeJson b, DecodeJson c) => DecodeJson (Tuple3 a b c) where
  decodeJson j = decodeJson j >>= f
    where
    f l = case uncons l of
      Just { head: a, tail } -> case uncons tail of
        Just { head: b, tail: tail' } -> case uncons tail' of
          Just { head: c, tail: tail'' } | null tail'' -> tuple3 <$> decodeJson a <*> decodeJson b <*> decodeJson c
          _ -> Left "Couldn't decode Tuple"
        _ -> Left "Couldn't decode Tuple"
      _ -> Left "Couldn't decode Tuple"

instance decodeJsonEither :: (DecodeJson a, DecodeJson b) => DecodeJson (Either a b) where
  decodeJson json =
    lmap ("Couldn't decode Either: " <> _) $
      decodeJObject json >>= \obj -> do
        tag <- maybe (Left "Expected field 'tag'") Right $ find "tag" obj
        val <- maybe (Left "Expected field 'value'") Right $ find "value" obj
        case stringFromBin <$> toString tag of
          Just "Right" -> Right <$> decodeJson val
          Just "Left" -> Left <$> decodeJson val
          _ -> Left "'tag' field was not \"Left\" or \"Right\""
    where
      find :: String -> JObject -> Maybe Json
      find k (JObject obj) = findMap (match $ bin k) obj

      match :: forall d e. (Eq d) => d -> Tuple2 d e -> Maybe e
      match s = uncurry2 $ (if _ then Just else const Nothing) <<< (==) s

instance decodeJsonNull :: DecodeJson Unit where
  decodeJson = foldJsonNull (Left "Value is not a null") (const $ Right unit)

instance decodeJsonBoolean :: DecodeJson Boolean where
  decodeJson = foldJsonBoolean (Left "Value is not a Boolean") Right

instance decodeJsonNumber :: DecodeJson Number where
  decodeJson = foldJsonNumber (Left "Value is not a Number") Right

instance decodeJsonInt :: DecodeJson Int where
  decodeJson
    = maybe (Left "Value is not an integer") Right
    <<< fromNumber
    <=< decodeJson

instance decodeJsonString :: DecodeJson String where
  decodeJson = foldJsonString (Left "Value is not a String") (Right <<< stringFromBin)

instance decodeJsonBinary :: DecodeJson Binary where
  decodeJson = foldJsonString (Left "Value is not a String") Right

instance decodeJsonJson :: DecodeJson Json where
  decodeJson = Right

instance decodeJsonJObject :: DecodeJson JObject where
  decodeJson = decodeJObject

instance decodeArray :: DecodeJson a => DecodeJson (Array a) where
  decodeJson
    = lmap ("Couldn't decode Array: " <> _)
    <<< ((fromFoldable <$> _ ) <$> traverse decodeJson <=< decodeJArray)

instance decodeList :: DecodeJson a => DecodeJson (List a) where
  decodeJson
    = lmap ("Couldn't decode List: " <> _)
    <<< (traverse decodeJson <=< decodeJArray)

decodeJArray :: Json -> Either String JArray
decodeJArray = maybe (Left "Value is not an Array") Right <<< toArray

decodeJObject :: Json -> Either String JObject
decodeJObject = maybe (Left "Value is not an Object") Right <<< toObject
