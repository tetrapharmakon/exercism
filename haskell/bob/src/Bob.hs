module Bob where

{-# LANGUAGE OverloadedStrings #-}

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
  | T.null rxs     = T.pack "Fine. Be that way!"
  | isYQuest rxs   = T.pack "Calm down, I know what I'm doing!"
  | isQuestion rxs = T.pack "Sure."
  | isYelled rxs   = T.pack "Whoa, chill out!"
  | otherwise      = T.pack "Whatever."
  where
    rxs = T.filter (not . isSpace) xs