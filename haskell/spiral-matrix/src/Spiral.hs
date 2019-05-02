module Spiral (spiral) where

import           Data.List

spiral' :: Integer -> Integer -> Integer -> [[Integer]]
spiral' 0 _ _ = []
spiral' h w s = [s .. s+w-1] : rot90 (spiral' w (h-1) (s+w))
  where rot90 = map reverse . transpose

spiral :: Integer -> [[Integer]]
spiral size = spiral' size size 1
