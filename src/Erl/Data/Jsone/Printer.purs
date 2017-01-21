module Erl.Data.Jsone.Printer (printJson, prettyPrintJson) where

import Erl.Data.Jsone (Json)

foreign import encode_ :: Json -> Int -> Int -> String

printJson :: Json -> String
printJson j = encode_ j 0 0

prettyPrintJson :: Json -> String
prettyPrintJson j = encode_ j 1 2
