module Test.Main where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Data.Either (Either(Right))
import Data.Maybe (Maybe(..))
import Erl.Data.Jsone (foldJson, foldJsonBoolean, foldJsonNull, foldJsonNumber, foldJsonString, fromArray, fromBoolean, fromNumber, fromString, jsonEmptyObject, jsonNull, toString)
import Erl.Data.Jsone.Decode.Class (decodeJson)
import Erl.Data.Jsone.Decode.Combinators ((.?))
import Erl.Data.Jsone.Encode.Combinators ((:=), (~>))
import Erl.Data.Jsone.Parser (jsonParser)
import Erl.Data.Jsone.Printer (printJson, prettyPrintJson)
import Erl.Data.List (nil, (:))
import Test.Assert (assert)

foreign import data DEBUG :: Effect
foreign import debugMsg :: forall eff. String -> Eff (debug :: DEBUG | eff) Unit
foreign import debugVal :: forall eff a. a -> Eff (debug :: DEBUG | eff) Unit

main = do
  debugMsg "Running PureScript tests!"

  debugVal $ printJson $ fromString $ "hello world"
  assert $ "\"hello world\"" == (printJson $ fromString $ "hello world")
  debugVal $ printJson $ fromBoolean $ true
  assert $ "true" == (printJson $ fromBoolean true)
  assert $ "false" == (printJson $ fromBoolean false)
  debugVal $ printJson $ fromArray (fromNumber 1.0 : fromNumber 2.0 : fromNumber 3.0 : nil)
  let fortyTwo = printJson $ fromArray $ (fromNumber 42.0 : nil)
  debugVal fortyTwo
  assert $ """[4.20000000000000000000e+01]""" == fortyTwo
  let obj = "foo" := 42.0
            ~> "bar" := "baz"
            ~> "x" := ("nestedKey" := true ~> jsonEmptyObject)
            ~> "y" := false
            ~> jsonEmptyObject
  assert $ """{"foo":4.20000000000000000000e+01,"bar":"baz","x":{"nestedKey":true},"y":false}""" == printJson obj
  debugMsg $ prettyPrintJson obj
  assert $ """{
  "foo": 4.20000000000000000000e+01,
  "bar": "baz",
  "x": {
    "nestedKey": true
  },
  "y": false
}""" == prettyPrintJson obj

  assert $ Right "baz" == (decodeJson obj >>= (_ .? "bar"))
  assert $ Right false == (decodeJson obj >>= (_ .? "y"))
  assert $ Right true == (decodeJson obj >>= (_ .? "x") >>= (_ .? "nestedKey"))

  let f :: forall a. a -> Boolean
      f = const false
      z :: forall a. a -> Number
      z = const 0.0
      s :: forall a. a -> String
      s = const ""

  assert $ foldJson (const true) f f f f f $ jsonNull
  assert $ foldJsonNull false (const true) jsonNull
  assert $ foldJson f id f f f f $ fromBoolean true
  assert $ foldJsonBoolean false (const true) $ fromBoolean true
  assert $ (42.0 == _) $ foldJson z z id z z z $ fromNumber 42.0
  assert $ (42.0 == _) $ foldJsonNumber 0.0 id $ fromNumber 42.0
  assert $ ("Hello" == _) $ foldJson s s s id s s $ fromString "Hello"
  assert $ ("Hello" == _) $ foldJsonString "Hello" id (fromString "Hello")

  assert $ Right (Just "roundtrip") == (toString <$> (jsonParser $ printJson $ fromString $ "roundtrip"))
