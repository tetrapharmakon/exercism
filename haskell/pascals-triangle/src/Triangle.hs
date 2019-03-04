module Triangle (rows) where

bini :: Int -> Int -> Integer
bini _ 0 = toInteger 1
bini n 1 = toInteger n
bini n k 
  | k > n = 0
  | otherwise = bini (n-1) k + bini (n-1) (k-1)

rows :: Int -> [[Integer]]
rows 0 = []
rows n = [[bini h k | k <- [0..h]] | h <- [0..n-1]]
