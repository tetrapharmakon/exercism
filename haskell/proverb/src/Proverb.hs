module Proverb(recite) where

helper :: [a] -> [[a]]
helper [x] = [[x]]
helper [x,y] = [[x], [x,y]]
helper xs = helper (init xs) ++ [fatTail xs]
  where
    fatTail zs = [last (init zs), last zs]

verse :: [String] -> String
verse [x] = concat ["And all for the want of a ", x, "."]
verse ss = concat ["For want of a ", head ss, " the ", head (tail ss), " was lost.\n"]

recite :: [String] -> String
recite [] = ""
recite xss = concatMap verse as ++ verse a
  where
    (a:as) = helper xss