module Test.Main where

import Prelude

import Data.Either (Either(Right))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Data.Jsone (foldJson, foldJsonBoolean, foldJsonNull, foldJsonNumber, foldJsonString, fromArray, fromBoolean, fromNumber, fromString, jsonEmptyObject, jsonNull, toString)
import Erl.Data.Jsone.Decode.Class (decodeJson)
import Erl.Data.Jsone.Decode.Combinators ((.?))
import Erl.Data.Jsone.Encode.Combinators ((:=), (~>))
import Erl.Data.Jsone.Parser (jsonParser)
import Erl.Data.Jsone.Printer (printJson, prettyPrintJson)
import Erl.Data.List (nil, (:))
import Test.Assert (assert)

foreign import debugMsg :: String -> Effect Unit
foreign import debugVal :: forall a. a -> Effect Unit

main :: Effect Unit
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
  assert $ foldJson f identity f f f f $ fromBoolean true
  assert $ foldJsonBoolean false (const true) $ fromBoolean true
  assert $ (42.0 == _) $ foldJson z z identity z z z $ fromNumber 42.0
  assert $ (42.0 == _) $ foldJsonNumber 0.0 identity $ fromNumber 42.0
  assert $ ("Hello" == _) $ foldJson s s s identity s s $ fromString "Hello"
  assert $ ("Hello" == _) $ foldJsonString "Hello" identity (fromString "Hello")

  assert $ Right (Just "roundtrip") == (toString <$> (jsonParser $ printJson $ fromString $ "roundtrip"))
