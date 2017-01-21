module Erl.Data.Jsone.Parser (jsonParser) where

import Prelude
import Erl.Data.Jsone (Json)
import Data.Either (Either(..))

foreign import jsonParserImpl :: forall a. (String -> a) -> (Json -> a) -> String -> Either String Json

jsonParser :: String -> Either String Json
jsonParser = jsonParserImpl Left Right
