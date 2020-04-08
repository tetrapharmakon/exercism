module Brackets (arePaired) where

import Data.Maybe

arePaired :: String -> Bool
arePaired = areThey ""

areThey :: String -> String -> Bool
areThey s [] = null s
areThey s (x:xs)
  | x `notElem` "([{}])" = areThey s xs
  | x `elem` "([{" = areThey (expected x:s) xs
  | x `elem` ")]}" && (not . null) s = (x == head s) && areThey (tail s) xs
  | otherwise = False

expected :: Char -> Char
expected c = fromJust $ lookup c $ zip "{[(" "}])"
