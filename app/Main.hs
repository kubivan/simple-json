module Main where

import SimpleJSON

main :: IO ()
main = putStrLn . show . isNull . JNumber $ 3
