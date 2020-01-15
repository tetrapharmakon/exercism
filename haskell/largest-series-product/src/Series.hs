module Series
  ( Error(..)
  , largestProduct
  ) where

import Data.Char

data Error
  = InvalidSpan
  | InvalidDigit Char
  deriving (Show, Eq)

sublists :: Int -> [a] -> [[a]]
sublists n xs = take (length xs - n + 1) $ sublists' n xs
  where
    sublists' _ [] = [[]]
    sublists' m qs@(_:rest) = take m qs : sublists' m rest

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | not validSpan = Left InvalidSpan
  | all isDigit digits =
    (toInteger . maximum <$> map product) . sublists size <$> digs
  | otherwise = Left $ InvalidDigit $ head chars
  where
    digs = Right $ map (\x -> read [x] :: Int) digits
    chars = filter (not . isDigit) digits
    validSpan = size <= length digits && size >= 0
