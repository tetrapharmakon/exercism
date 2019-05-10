module Transpose (tRanspose) where

import           Data.List

padSomeSpace :: Int -> String -> String
padSomeSpace n s = take n $ s ++ map (const ' ') [1..]

padMatrix :: [String] -> [String]
padMatrix [] = []
padMatrix ss = map (padSomeSpace n) (init ss) ++ [last ss]
  where
    n = maximum (map length ss)

tRanspose :: [String] -> [String]
tRanspose ls = transpose ls
