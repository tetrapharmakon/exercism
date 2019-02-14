module Anagram (anagramsFor) where

import Data.List
import Data.Char

shout :: String -> String
shout = map toUpper

isAnagramOf :: String -> String -> Bool
isAnagramOf a b = sort (shout a) == sort (shout b) && shout a /= shout b

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter (isAnagramOf xs)