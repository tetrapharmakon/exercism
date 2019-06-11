module Series
  ( slices
  ) where

import Data.Vector hiding (length, map)

slices :: Int -> String -> [[Int]]
slices n xs =
  map (helperino . toList) [slice i n $ fromList xs | i <- [0 .. (l - n)]]
  where
    l = length xs
    helperino = map (\x -> read [x] :: Int)
