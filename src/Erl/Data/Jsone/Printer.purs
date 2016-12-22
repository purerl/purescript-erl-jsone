module Erl.Data.Jsone.Printer (printJson, prettyPrintJson) where

import Erl.Data.Binary
import Erl.Data.Jsone (Json)

foreign import encode_ :: Json -> Int -> Int -> Binary

printJson :: Json -> Binary
printJson j = encode_ j 0 0

prettyPrintJson :: Json -> Binary
prettyPrintJson j = encode_ j 1 2
