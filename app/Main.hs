module Main where

import SimpleJSON
import PrettyJSON
import Prettify(compact, pretty, ident)

main :: IO ()
main = putStrLn . pretty 10 . ident 4 . renderJValue $ JObject [("f", JNumber 1), ("q", JBool True)]
