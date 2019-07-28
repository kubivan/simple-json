module Main where

import SimpleJSON
import PrettyJSON
import Prettify(compact, pretty)

main :: IO ()
main = putStrLn . pretty 3 . renderJValue $ JObject [("f", JNumber 1), ("q", JBool True)]
