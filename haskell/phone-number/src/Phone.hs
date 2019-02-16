module Phone (number) where

import Data.Char

isValid :: String -> Bool
isValid xs = undefined

stripped :: String -> String
stripped xs = tail $ filter (isDigit) xs

number :: String -> Maybe String
number xs 
  | isValid xs = Just (stripped xs)
  | otherwise  = Nothing
