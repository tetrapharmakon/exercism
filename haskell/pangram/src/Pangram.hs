module Pangram (isPangram) where

import Data.Char
import Data.List

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

alphabet :: String
alphabet = ['a'..'z']

isPangram :: String -> Bool
isPangram text = sort (rmdups ltext) == alphabet
  where ltext = filter (`elem` alphabet) $ map toLower text