module Sandbox where

import Text.ParserCombinators.Parsec
import Data.Char

slot = sepBy cell (char '-')
cell = many (digit <|> char 'X')

parsePhone :: String -> Either ParseError [String]
parsePhone = parse slot "(unknown)"

safeConcat :: Either ParseError [String] -> String
safeConcat (Right xs) = concat xs
safeConcat (Left _) = "error"