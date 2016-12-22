module Erl.Data.Jsone.Encode.Class where

import Prelude
import Data.Either (Either, either)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Erl.Data.Binary (Binary, bin)
import Erl.Data.Jsone (Json, JObject(..), fromArray, fromBoolean, fromNumber, fromObject, fromString, jsonNull)
import Erl.Data.List (List, fromFoldable, nil, (:))
import Erl.Data.Tuple (Tuple2, Tuple3, Tuple4, Tuple5, tuple2, uncurry2, uncurry3, uncurry4, uncurry5)

class EncodeJson a where
  encodeJson :: a -> Json

instance encodeJsonMaybe :: EncodeJson a => EncodeJson (Maybe a) where
  encodeJson Nothing  = jsonNull
  encodeJson (Just a) = encodeJson a

instance encodeJsonTuple :: (EncodeJson a, EncodeJson b) => EncodeJson (Tuple a b) where
  encodeJson (Tuple a b) = encodeJson [encodeJson a, encodeJson b]

instance encodeJsonTuple2 :: (EncodeJson a, EncodeJson b) => EncodeJson (Tuple2 a b) where
  encodeJson = uncurry2 \a b -> encodeJson [encodeJson a, encodeJson b]

instance encodeJsonTuple3 :: (EncodeJson a, EncodeJson b, EncodeJson c) => EncodeJson (Tuple3 a b c) where
  encodeJson = uncurry3 \a b c -> encodeJson [encodeJson a, encodeJson b, encodeJson c]

instance encodeJsonTuple4 :: (EncodeJson a, EncodeJson b, EncodeJson c, EncodeJson d) => EncodeJson (Tuple4 a b c d) where
  encodeJson = uncurry4 \a b c d -> encodeJson [encodeJson a, encodeJson b, encodeJson c, encodeJson d]

instance encodeJsonTuple5 :: (EncodeJson a, EncodeJson b, EncodeJson c, EncodeJson d, EncodeJson e) => EncodeJson (Tuple5 a b c d e) where
  encodeJson = uncurry5 \a b c d e -> encodeJson [encodeJson a, encodeJson b, encodeJson c, encodeJson d, encodeJson e]

instance encodeJsonEither :: (EncodeJson a, EncodeJson b) => EncodeJson (Either a b) where
  encodeJson = either (obj "Left") (obj "Right")
    where
    obj :: forall c. EncodeJson c => String -> c -> Json
    obj tag x =
      fromObject $ JObject $
        tuple2 (bin "tag") (fromString $ bin tag) : tuple2 (bin "value") (encodeJson x) : nil

instance encodeJsonUnit :: EncodeJson Unit where
  encodeJson = const jsonNull

instance encodeJsonJBoolean :: EncodeJson Boolean where
  encodeJson = fromBoolean

instance encodeJsonJNumber :: EncodeJson Number where
  encodeJson = fromNumber

instance encodeJsonInt :: EncodeJson Int where
  encodeJson = fromNumber <<< toNumber

instance encodeJsonJString :: EncodeJson String where
  encodeJson = fromString <<< bin

instance encodeJsonBinary :: EncodeJson Binary where
  encodeJson = fromString

instance encodeJsonJson :: EncodeJson Json where
  encodeJson = id

instance encodeJsonArray :: EncodeJson a => EncodeJson (Array a) where
  encodeJson json = fromArray (encodeJson <$> fromFoldable json)

instance encodeJsonList :: EncodeJson a => EncodeJson (List a) where
  encodeJson = fromArray <<< map encodeJson
