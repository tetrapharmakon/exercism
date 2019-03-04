module Bob (responseFor) where

import Data.Char

isQuestion :: String -> Bool
isQuestion s = last s == '?'

isYell :: String -> Bool
isYell xs = all isUpper rxs
  where rxs = init $ filter (/=' ') xs

isYellQuest :: String -> Bool
isYellQuest xs = isQuestion xs && isYell xs

responseFor :: String -> String
responseFor xs
  | isYellQuest xs = "Calm down, I know what I'm doing!"
  | isQuestion xs = "Sure."
  | isYell (init xs) = "Whoa, chill out!"
  | "" == xs = "Fine. Be that way!"
  | otherwise = "Whatever."