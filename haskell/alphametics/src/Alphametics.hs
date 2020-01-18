module Alphametics
  ( solve
  ) where

import Data.List

alph :: String
alph = ['A' .. 'Z']

addends :: String -> [String]
addends = filter (/= "+") . takeWhile (/= "==") . words

result :: String -> String
result = last . dropWhile (/= "==") . words

solve :: String -> Maybe [(Char, Int)]
solve puzzle = error "You need to implement this function."
