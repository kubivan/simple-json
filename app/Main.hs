module Main where

import SimpleJSON
import PrettyJSON
import Prettify(compact, pretty, ident)

result :: JValue
result = JObject [
  ("query", JString "awkward squad haskell"),
  ("estimatedCount", JNumber 3920),
  ("moreResults", JBool True),
  ("results", JArray [
     JObject [
      ("title", JString "Simon Peyton Jones: papers"),
      ("snippet", JString "Tackling the awkward ..."),
      ("url", JString "http://.../marktoberdorf/")
     ]])
  ]

main :: IO ()
--main = putStrLn . pretty 10 . ident 4 . renderJValue $ JObject [("f", JNumber 1), ("q", JBool True)]
main = do
    putStrLn "================SOURCE=============="
    putStrLn . show $ result
    putStrLn "================RENDERED=============="
    putStrLn . show . renderJValue $ result
    putStrLn "================PRETTY=============="
    putStrLn . pretty 10 . renderJValue $ result
    putStrLn "================PRETTY&IDENT=============="
    putStrLn . pretty 10 . ident 4 . renderJValue $ result
