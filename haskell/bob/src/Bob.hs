module Bob (responseFor) where

import Data.Char

isQuestion :: String -> Bool
isQuestion s = last s == '?'

isYelled :: String -> Bool
isYelled s 
  | fs == ""  = False
  | otherwise = all isUpper fs
  where
    fs = filter isAlpha s

isYQuest :: String -> Bool
isYQuest s = isQuestion s && isYelled s

responseFor :: String -> String
responseFor xs 
  | rxs == ""      = "Fine. Be that way!"
  | isYQuest rxs   = "Calm down, I know what I'm doing!"
  | isQuestion rxs = "Sure."
  | isYelled rxs   = "Whoa, chill out!"
  | otherwise      = "Whatever."
  where
    rxs = filter (not . isSpace) xs