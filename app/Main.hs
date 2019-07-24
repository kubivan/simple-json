module Main where

import SimpleJSON
import PrettyJSON
import Prettify(compact)

main :: IO ()
main = putStrLn . compact . renderJValue $ JObject [("f", JNumber 1), ("q", JBool True)]
