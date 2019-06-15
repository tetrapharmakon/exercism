module Cipher
  ( caesarDecode
  , caesarEncode
  , caesarEncodeRandom
  ) where

import Data.Char
import System.Random

alphabet :: String
alphabet = ['a' .. 'z']

ord' :: Char -> Int
ord' c = ord c - 97

chr' :: Int -> Char
chr' n = chr (n + 97)

-- if #xs = N, 
-- then (rot (N-n)) . (rot n) 
--     = (rot n) . (rot (N-n)) 
--     = id
rot :: Int -> [a] -> [a]
rot n xs = drop n xs ++ take n xs

kaiser :: Char -> Char -> Char
kaiser key c = rot (ord' key) alphabet !! ord' c

caesarDecode :: String -> String -> String
caesarDecode [x] text = map (kaiser dk) text
  where
    dk = chr' (26 - ord' x)
caesarDecode key text =
  if simpleKey
    then map (kaiser dHeadKey) text
    else zipWith kaiser dk text
  where
    dk = dualKey key
    dHeadKey = chr' (26 - ord' (head key))
    dualKey = map (\c -> chr' (26 - ord' c))
    simpleKey = all (\x -> x == head key) key

caesarEncode :: String -> String -> String
caesarEncode [x] text = map (kaiser x) text
caesarEncode key text =
  if simpleKey
    then map (kaiser (head key)) text
    else zipWith kaiser key text
  where
    simpleKey = all (\x -> x == head key) key

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  g <- getStdGen
  let randKey = take (length text) (randomRs ('a', 'z') g)
  return (randKey, caesarEncode randKey text)
