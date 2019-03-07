module Sandbox where

import Text.ParserCombinators.Parsec
import Data.Char

slot = sepBy cell (char '-')
cell = many (digit <|> char 'X')

allDigits :: String -> Bool
allDigits = all isDigit

parseISBN :: String -> Either ParseError [String]
parseISBN = parse slot "(unknown)"

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

a :: [String]
a = [ "3-598-21508-8"
    , "3-598-21508-9"
    , "3-598-21507-X"
    , "3-598-21507-A"
    , "3-598-P1581-X"
    , "3-598-2X507-9"
    , "3598215088"
    , "359821507"
    , "3598215078X"
    , "00"
    , "3-598-21507"
    , "3-598-21515-X"
    , ""
    , "134456729"
    , "3132P34035"
    , "98245726788"]

b :: [Bool]
b = [ True
    , False
    , True
    , False
    , False
    , False
    , True
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False
    , False]