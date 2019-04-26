module DNA (toRNA) where

import Data.List
import Data.Maybe

valid :: Char -> Bool
valid c = c `elem` "ACGT"

toRNA :: String -> Either Char String
toRNA xs 
  | all valid xs = Right $ map f xs
  | otherwise = Left $ fromJust $ find (not . valid) xs
  where
    f 'A' = 'U'
    f 'C' = 'G'
    f 'G' = 'C'
    f 'T' = 'A'
    f _ = undefined