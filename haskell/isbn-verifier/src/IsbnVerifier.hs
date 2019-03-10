module IsbnVerifier (isbn) where

import Text.ParserCombinators.Parsec
import Data.Char
import Data.Either

cell = many (digit <|> char 'X')
slot = sepBy cell (char '-')

parseISBN :: String -> Either ParseError [String]
parseISBN = parse slot "error"

allDigits :: String -> Bool
allDigits = all isDigit

safeConcat :: Either ParseError [String] -> String
safeConcat (Right xs) = concat xs
safeConcat (Left _) = "error"

test :: [Integer] -> Bool
test xs = sum (zipWith (*) nums xs) `mod` 11 == 0
  where nums = reverse [1..10]

validate :: Either ParseError [String] -> Bool
validate (Left _) = False
validate (Right xs) = and [ map length xs == [1, 3, 5, 1]
                          , allDigits $ init $ safeConcat (Right xs)
                          , head (last xs) `elem` "0123456789X" 
                          , test wxs]
                          where wxs = map f $ safeConcat (Right xs)

f :: Char -> Integer
f 'X' = 10
f x
  | isDigit x = (read . (:[])) x :: Integer
  | otherwise = undefined

isbn :: String -> Bool
isbn input 
  | '-' `elem` input = validate $ parseISBN input
  | otherwise = length input == 10 && allDigits input && test winput
  where winput = map f input