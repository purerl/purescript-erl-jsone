module Test.Main where

import Prelude
import Control.Monad.Eff.Console (CONSOLE, log)
import Erl.Data.Jsone
import Erl.Data.Jsone.Printer (printJson, prettyPrintJson)
import Erl.Data.Jsone.Parser (jsonParser)
import Erl.Data.Jsone.Encode.Combinators

import Erl.Data.Jsone.Decode.Combinators
import Erl.Data.Jsone.Decode.Class
import Erl.Data.List (nil, (:), List)
import Erl.Data.Tuple
import Data.Either (either, Either(..))
import Data.Maybe
import Control.Monad.Eff (Eff)
import Test.Assert

foreign import data DEBUG :: !
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
