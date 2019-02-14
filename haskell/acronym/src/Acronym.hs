module Acronym (abbreviate) where

import Data.Char

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

thisForThat :: Char -> Char
thisForThat x 
  | x == '-' = ' ' 
  | otherwise = x

shout :: String -> String
shout = map toUpper

uppers :: String -> String
uppers = filter isUpper

f :: String -> String
f x = if x == shout x then [head x] else uppers x

abbreviate :: String -> String
abbreviate xs = concatMap (f . capitalize) $ words $ map thisForThat xs