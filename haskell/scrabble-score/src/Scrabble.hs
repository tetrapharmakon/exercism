module Scrabble where

import           Data.Char
import           Data.Map
import           Data.Maybe

fromFibers :: Ord b => Map a [b] -> Map b a
fromFibers elts = fromList $ concatMap f (assocs elts)
  where
    f (_ , [])    = []
    f (n, x : xs) = (x, n) : f (n, xs)

scrabRules :: [(Integer, String)]
scrabRules = [ (1, "AEIOULNRST")
             , (2, "DG")
             , (3, "BCMP")
             , (4, "FHVWY")
             , (5, "K")
             , (8, "JX")
             , (10, "QZ")
             ]

scoreLetter :: Char -> Integer
scoreLetter letter
  | uletter `elem` ['A'..'Z'] = fromJust $ fromFibers (fromList scrabRules) !? uletter
  | otherwise = 0
  where uletter = toUpper letter

scoreWord :: String -> Integer
scoreWord word = sum $ Prelude.map (scoreLetter . toUpper) word
