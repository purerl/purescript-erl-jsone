-- | Provides operators for a DSL to construct `Json` values:
-- |
-- | ``` purescript
-- | myJson
-- |  = "key1" := value1
-- |  ~> "key2" := value2
-- |  ~> jsonEmptyObject
-- | ```
module Erl.Data.Jsone.Encode.Combinators where

import Prelude
import Data.Newtype (unwrap)
import Erl.Data.Jsone (JAssoc, Json, JObject(..), foldJsonObject, jsonSingletonObject, fromObject)
import Erl.Data.Jsone.Encode.Class (class EncodeJson, encodeJson)
import Erl.Data.List ((:))
import Erl.Data.Tuple (tuple2, uncurry2)

-- | Creates a `JAssoc` entry, representing a key/value pair for an object.
infix 7 assoc as :=

-- | The named implementation of the `(:=)` operator.
assoc :: forall a. EncodeJson a => String -> a -> JAssoc
assoc k = tuple2 k <<< encodeJson

-- | Extends a Json object with a `JAssoc` property.
infixr 6 extend as ~>

-- | The named implementation of the `(~>)` operator.
extend :: forall a. EncodeJson a => JAssoc -> a -> Json
extend = uncurry2 \k v ->
  foldJsonObject
    (jsonSingletonObject k v)
    (unwrap >>> (tuple2 k v : _) >>> JObject >>> fromObject)
    <<< encodeJson
