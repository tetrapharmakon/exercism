module Atbash
  ( decode
  , encode
  ) where

import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe

alphabet :: String
alphabet = ['a' .. 'z']

alphaBet :: String
alphaBet = ['A' .. 'Z']

mapCiphers :: M.Map Char Char
mapCiphers = M.fromList $ zip alphabet (reverse alphabet)

decode :: String -> String
decode text = map decEncFunction $ filter (/= ' ') text

encode :: String -> String
encode text = unwords $ chunksOf 5 $ map decEncFunction ltext
  where
    ltext = map helperino $ filter (`notElem` " ,.;:") text

helperino c =
  if c `elem` alphaBet
    then toLower c
    else c

decEncFunction :: Char -> Char
decEncFunction x =
  if x `elem` alphabet
    then fromJust $ M.lookup x mapCiphers
    else x
