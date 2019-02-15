module CryptoSquare (encode) where

import Data.Char
import Data.List
import Data.List.Split

whisper :: String -> String
whisper = map toLower

test :: Char -> Bool
test x  = isAlphaNum x && (x /=' ')

intSquareRoot :: Int -> Int
intSquareRoot n = try n where
  try i   | i*i > n   = try (i - 1) 
          | i*i <= n  = i

padLeft :: [a] -> Int -> a -> [a]
padLeft xs 0 _ = xs
padLeft xs n x = xs ++ map (const x) [1..n]

padLast :: [String] -> [String]
padLast xss = init xss ++ [padLeft (last xss) n ' ']
  where n = length (head xss) - length (last xss)

encode :: String -> String
encode [] = []
encode xs = 
  unwords $ transpose $ padLast $ chunksOf sql $ filter test (whisper xs)
    where 
      l = length xs
      sql = if l <= k * (k+1) then k else k+1
      k = intSquareRoot l
      s = chunksOf sql $ filter test xs