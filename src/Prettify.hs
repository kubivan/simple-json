module Prettify where

import Prelude hiding ((<>))
import Debug.Trace(trace)

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving(Show)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = text (show d)

--line break
line :: Doc 
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc 
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of 
                Empty        -> transform ds
                Char c       -> c : transform ds
                Text s       -> s ++ transform ds
                Line         -> '\n' : transform ds
                a `Concat` b -> transform (a:b:ds)
                _ `Union` b  -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) = 
               case d of
                 Empty    -> best col ds
                 Char c   -> c : best (col + 1) ds
                 Text s   -> s ++ best (col + length s) ds
                 Line     -> '\n' : best 0 ds
                 a `Concat` b -> best col (a:b:ds) 
                 a `Union` b -> nicest col (best col (a:ds))
                                           (best col (b:ds))
          best _ _ = ""
          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col

ident :: Int -> Doc -> Doc
ident width x = transform 0 [x]
    where transform _ [] = Empty
          transform level (d:ds) = 
              case d of 
                Char '{' -> identElem level d <> Line <> transform (level+1) ds
                Char '}' -> identElem level d <> transform (level-1) ds
                Char ':' -> d <> transform level ds
                Char ',' -> d <> transform level (Line:ds)
                Char _ -> d <> transform level ds
                l `Concat` r -> transform level (l:r:ds)
                l `Union` r -> transform level (l:ds)
                Line -> Line <> transform level ds
                Text _ -> identElem level d <> transform level ds
                otherwise -> error ("pattern missed!" ++ show d)
          identElem level x = x <>Text (replicate (level*width) ' ')

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs

