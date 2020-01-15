module Yacht
  ( yacht
  , Category(..)
  ) where

import Data.List
import Data.Maybe

data Category
  = Ones
  | Twos
  | Threes
  | Fours
  | Fives
  | Sixes
  | FullHouse
  | FourOfAKind
  | LittleStraight
  | BigStraight
  | Choice
  | Yacht

howManys :: Eq a => a -> [a] -> Int
howManys n xs = length $ filter (== n) xs

mapLengthGroupSort :: Ord a => [a] -> [Int]
mapLengthGroupSort xs = map length $ group $ sort xs

yacht :: Category -> [Int] -> Int
yacht Ones dice = howManys 1 dice
yacht Twos dice = 2 * howManys 2 dice
yacht Threes dice = 3 * howManys 3 dice
yacht Fours dice = 4 * howManys 4 dice
yacht Fives dice = 5 * howManys 5 dice
yacht Sixes dice = 6 * howManys 6 dice
yacht FullHouse dice =
  if elem 3 $ mapLengthGroupSort dice
    then sum dice
    else 0
yacht FourOfAKind dice
  | isNothing (helperino dice) = 0
  | any (>= 4) $ mapLengthGroupSort dice = 4 * fromJust (helperino dice)
  | otherwise = 0
yacht LittleStraight dice =
  if [1 .. 5] == sort dice
    then 30
    else 0
yacht BigStraight dice =
  if [2 .. 6] == sort dice
    then 30
    else 0
yacht Choice dice = sum dice
yacht Yacht dice =
  if allEqual dice
    then 50
    else 0

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual (x:y:xs) = (x == y) && allEqual xs

-- helperino checks whether four elements in a tuple are equal and tells you which
helperino :: (Eq a, Ord a) => [a] -> Maybe a
helperino xs =
  let l = length xs
   in if l /= 5 -- if there are less than six elts, then ragequit
        then Nothing
        else if any (>= 4) $ mapLengthGroupSort xs -- if there are 4 equal elements
               then Just $
                    head $
                    concat $ filter (\x -> length x >= 4) $ group $ sort xs
               else Nothing
