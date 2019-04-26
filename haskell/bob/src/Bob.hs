{-# LANGUAGE OverloadedStrings #-}

module Bob where

import Data.Char
import qualified Data.Text as T

isQuestion :: T.Text -> Bool
isQuestion s = T.last s == '?'

isYelled :: T.Text -> Bool
isYelled s 
  | T.null fs  = False
  | otherwise = T.all isUpper fs
  where
    fs = T.filter isAlpha s

isYQuest :: T.Text -> Bool
isYQuest s = isQuestion s && isYelled s

responseFor :: T.Text -> T.Text
responseFor xs 
  | T.null rxs     = "Fine. Be that way!"
  | isYQuest rxs   = "Calm down, I know what I'm doing!"
  | isQuestion rxs = "Sure."
  | isYelled rxs   = "Whoa, chill out!"
  | otherwise      = "Whatever."
  where
    rxs = T.filter (not . isSpace) xs