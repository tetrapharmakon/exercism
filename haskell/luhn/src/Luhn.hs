module Luhn (isValid) where

import Data.Char

stripSpaces :: String -> String
stripSpaces = filter (\x -> isDigit x && x /= ' ')

evens :: [a] -> [a]
evens [] = []
evens [x] = []
evens (_:y:xs) = y : evens xs

odds :: [a] -> [a]
odds [] = []
odds [x] = [x]
odds (x:_:xs) = x : odds xs

digits :: String -> [Int]
digits = map f
  where f x = read [x] :: Int

shuffle :: [a] -> [a] -> [a]
shuffle [] ys = ys
shuffle xs [] = xs
shuffle (x:xs) (y:ys) = x : y : shuffle xs ys

luhnDouble :: Int -> Int
luhnDouble n = if 2 * n > 9 then 2 * n - 9 else 2 * n

isValid :: String -> Bool
isValid n 
  | stripSpaces n == "0" = False
  | otherwise =
  mod (sum $ shuffle (odds rn) (map luhnDouble (evens rn))) 10 == 0
    where
      rn = digits $ reverse $ stripSpaces n 