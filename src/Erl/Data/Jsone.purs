module Erl.Data.Jsone (
  Json,
  JNull,
  JBoolean,
  JNumber,
  JString,
  JArray,
  JObject(..),
  JAssoc,
  fromNull,
  fromBoolean,
  fromNumber,
  fromString,
  fromArray,
  fromObject,
  toNull,
  toBoolean,
  toNumber,
  toString,
  toArray,
  toObject,
  foldJson,
  foldJsonNull,
  foldJsonBoolean,
  foldJsonNumber,
  foldJsonString,
  foldJsonObject,
  jsonNull,
  isNull,
  jsonEmptyObject,
  jsonSingletonObject
  ) where

import Prelude
import Data.Newtype (class Newtype)
import Erl.Data.List
import Erl.Data.Tuple
import Data.Maybe

foreign import data Json :: Type

foreign import data JNull :: Type

foreign import coerce_ :: forall a b. a -> b

type JBoolean = Boolean
type JNumber = Number
type JString = String
type JArray = List Json

newtype JObject = JObject (List (Tuple2 String Json))
derive instance newtypeJObject :: Newtype JObject _

type JAssoc = Tuple2 String Json

fromNull :: JNull -> Json
fromNull = coerce_

fromBoolean  :: JBoolean -> Json
fromBoolean = coerce_

fromNumber :: JNumber -> Json
fromNumber = coerce_

fromString  :: JString -> Json
fromString = coerce_

fromArray :: JArray -> Json
fromArray = coerce_

foreign import fromObject_ :: JObject -> Json

fromObject  :: JObject -> Json
fromObject = fromObject_

foreign import toNull_ :: forall a. a -> (JNull -> a) -> Json -> a

toNull :: Json -> Maybe JNull
toNull = toNull_ Nothing Just

foreign import toBoolean_ :: forall a. a -> (JBoolean -> a) -> Json -> a

toBoolean :: Json -> Maybe JBoolean
toBoolean = toBoolean_ Nothing Just

foreign import toNumber_ :: forall a. a -> (JNumber -> a) -> Json -> a

toNumber :: Json -> Maybe JNumber
toNumber = toNumber_ Nothing Just

foreign import toString_ :: forall a. a -> (JString -> a) -> Json -> a

toString :: Json -> Maybe JString
toString = toString_ Nothing Just

foreign import toArray_ :: forall a. a -> (JArray -> a) -> Json -> a

toArray :: Json -> Maybe JArray
toArray = toArray_ Nothing Just

foreign import toObject_ :: forall a. a -> (JObject -> a) -> Json -> a

toObject :: Json -> Maybe JObject
toObject = toObject_ Nothing Just

foreign import foldJson
  :: forall a
   . (JNull -> a)
  -> (JBoolean -> a)
  -> (JNumber -> a)
  -> (JString -> a)
  -> (JArray -> a)
  -> (JObject -> a)
  -> Json -> a

foldJsonNull :: forall a. a -> (JNull -> a) -> Json -> a
foldJsonNull d f j = foldJson f (const d) (const d) (const d) (const d) (const d) j

foldJsonBoolean :: forall a. a -> (JBoolean -> a) -> Json -> a
foldJsonBoolean d f j = foldJson (const d) f (const d) (const d) (const d) (const d) j

foldJsonNumber :: forall a. a -> (JNumber -> a) -> Json -> a
foldJsonNumber d f j = foldJson (const d) (const d) f (const d) (const d) (const d) j

foldJsonString :: forall a. a -> (JString -> a) -> Json -> a
foldJsonString d f j = foldJson (const d) (const d) (const d) f (const d) (const d) j

foldJsonArray :: forall a. a -> (JArray -> a) -> Json -> a
foldJsonArray d f j = foldJson (const d) (const d) (const d) (const d) f (const d) j

foldJsonObject :: forall a. a -> (JObject -> a) -> Json -> a
foldJsonObject d f j = foldJson (const d) (const d) (const d) (const d) (const d) f j

foreign import jsonNull :: Json

foreign import isNull :: Json -> Boolean

jsonEmptyObject :: Json
jsonEmptyObject = fromObject (JObject nil)

jsonSingletonObject :: String -> Json -> Json
jsonSingletonObject k v = fromObject $ JObject $ tuple2 k v : nil
