module Raindrops (convert) where

aFP :: Int -> [Int]
aFP n = [ i | i <- [3,5,7] , n `mod` i == 0]

convert :: Int -> String
convert n = if null (aFP n) then show n else concatMap f (aFP n)
    where
      f 3 = "Pling"
      f 5 = "Plang"
      f 7 = "Plong"
      f _ = ""
