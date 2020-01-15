module Acronym where

import Control.Monad
import Data.Char

capitalize :: String -> String
capitalize = ap ((:) . toUpper . head) tail

thisForThat :: String -> String
thisForThat = map q
  where
    q x
      | x == '-' = ' '
      | otherwise = x

shout :: String -> String
shout = map toUpper

uppers :: String -> String
uppers = filter isUpper

f :: String -> String
f x =
  if x == shout x
    then [head x]
    else uppers x

abbreviate :: String -> String
abbreviate = (f . capitalize =<<) . words . thisForThat
