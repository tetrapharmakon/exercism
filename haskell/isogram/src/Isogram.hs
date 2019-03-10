module Isogram (isIsogram) where

import Data.List
import Data.Char

sg :: Ord a => [a] -> [[a]]
sg = group . sort

filterByLength :: Ord a => (Int -> Bool) -> [a] -> [[a]]
filterByLength p = filter (p . length) . sg

repeatedBy :: Ord a => (Int -> Bool) -> [a] -> [a]
repeatedBy p = map head . filterByLength p

repeated :: Ord a => [a] -> [a]
repeated = repeatedBy (>1)

isIsogram :: String -> Bool
isIsogram s = repeated (map toLower rs) == ""
  where rs = filter isAlpha s