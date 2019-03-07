module OCR (convert) where

import Data.List hiding (group)

f :: Char -> Integer
f '_' = 1
f '|' = 1
f _   = 0

g :: [Integer] -> Char
g [2,5,7,0] = '0'
g [0,1,1,0] = '1'
g [2,3,6,0] = '2'
g [2,3,3,0] = '3'
g [0,7,1,0] = '4'
g [2,6,3,0] = '5'
g [2,6,7,0] = '6'
g [2,1,1,0] = '7'
g [2,7,7,0] = '8'
g [2,7,3,0] = '9'
g _ = '?'

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative or zero n"

numbers :: [[Int]]
numbers = [ [2,5,7,0] --0
          , [0,1,1,0] --1
          , [2,3,6,0] --2
          , [2,3,3,0] --3
          , [0,7,1,0] --4
          , [2,6,3,0] --5
          , [2,6,7,0] --6
          , [2,1,1,0] --7
          , [2,7,7,0] --8
          , [2,7,3,0] --9
          ]

convert :: String -> String
convert xs = map g (hh (lines xs))

fromBin :: [Integer] -> Integer
fromBin xs = sum [(xs !! (l-i)) * 2^i | i <- [0..l]]
  where l = length xs - 1

hh :: [String] -> [[Integer]]
hh h = map (map (fromBin . map f)) (transpose $ map (group 3) h)