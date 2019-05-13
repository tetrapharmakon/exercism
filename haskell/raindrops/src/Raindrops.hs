module Raindrops (convert) where

f n
  | mod n 3 == 0 = "Pling"
  | mod n 5 == 0 = "Plang"
  | mod n 7 == 0 = "Plong"
  | otherwise = ""

convert :: Int -> String
convert n = 
  if n `mod` 3 == 0 "Pling" 
    else if ""
