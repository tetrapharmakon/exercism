module Triangle (TriangleType(..), triangleType) where

import Data.List

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Eq a, Ord a, Num a) => a -> a -> a -> TriangleType
triangleType a b c
  | notLegal     = Illegal
  | lNubABC == 1 = Equilateral
  | lNubABC == 2 = Isosceles
  | lNubABC == 3 = Scalene
  | otherwise = undefined
  where
    lNubABC = length (nub [a,b,c])
    notLegal = not ((a < b + c) && (b < a + c) && (c < a + b))