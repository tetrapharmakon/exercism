module Phone (number) where

import Data.Char

allDigits :: Maybe String -> Bool
allDigits (Just xs) = all isDigit xs
allDigits Nothing = False

isNANP :: Maybe String -> Bool
isNANP (Just xs) = head xs `elem` ['2'..'9'] && (xs !! 3) `elem` ['2'..'9']
isNANP Nothing = False 

isValid :: String -> Bool
isValid xs = allDigits (Just (stripped xs)) && isNANP (Just (stripped xs))

stripped :: String -> String
stripped xs 
  | head xs == '1' = xs
  | otherwise = filter (`notElem` "()-+. ") xs

number :: String -> Maybe String
number xs 
  | length (stripped xs) /= 10 = Nothing
  | take 3 xs == "+1 " = Just (stripped $ drop 3 xs)
  | take 2 xs == "1 " = Just (stripped $ drop 2 xs)
  | head xs == '1' = Just (tail $ stripped xs)
  | isValid xs = Just (stripped xs)
  | otherwise = Nothing