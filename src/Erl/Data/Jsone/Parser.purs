module Erl.Data.Jsone.Parser (jsonParser) where

import Prelude
import Erl.Data.Binary (Binary)
import Erl.Data.Jsone (Json)
import Data.Either (Either(..))

foreign import jsonParserImpl :: forall a. (String -> a) -> (Json -> a) -> Binary -> Either String Json

jsonParser :: Binary -> Either String Json
jsonParser = jsonParserImpl Left Right
