module Doi where

import Text.ParserCombinators.Parsec

doiCode :: Parser [String]
doiCode = prefix `sepBy` (char '.' <|> char '/')

-- prefix = string "10."
-- doiURL = optional $ string "http://dx.doi.org/"

prefix :: Parser String
prefix = many (alphaNum <|> char '/')

parseDOI :: String -> Either ParseError [String]
parseDOI = parse doiCode "(unknown)"

safeConcat :: Either ParseError [String] -> String
safeConcat (Right xs) = concat xs
safeConcat (Left _) = "error"

doi :: String -> Maybe [String]
doi code = undefined