module RotationalCipher (rotate) where

import Data.Char

alphaBet :: String
alphaBet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

ord' :: Char -> Int
ord' c 
  | isLower c = ord c - 71
  | otherwise = ord c - 65

chr' :: Int -> Char
chr' n 
  | n `elem` [0..25] = alphaBet !! n
  | n `elem` [26..51] = alphabet !! (n-26)
  | otherwise = undefined

rotate :: Int -> String -> String
rotate 0 = id
rotate n = map (q n)
  where
    q n c
      | isAlpha c = chr' (kaiserCypher n c)
      | otherwise = c

kaiserCypher :: Int -> Char -> Int
kaiserCypher n x
  | isUpper x = mod (ord' x + (n `mod` 26)) 26
  | isLower x = 26 + mod (ord' x + (n `mod` 26)) 26      
  | otherwise = undefined
