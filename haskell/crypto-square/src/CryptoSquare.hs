module CryptoSquare (encode) where

import Data.Char
import Data.List.Split

test :: Char -> Bool
test x  = isLetter x && (x /=' ')

intSquareRoot :: Int -> Int
intSquareRoot n = try n where
  try i   | i*i > n   = try (i - 1) 
          | i*i <= n  = i

encode :: String -> [String]
encode xs = chunksOf n $ filter test xs
  where 
    n = minimum [l - sql^2, l - sql * (sql + 1), l - sql * (sql - 1)]
    l = length xs
    sql = intSquareRoot l
