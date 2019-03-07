module Matrix (saddlePoints) where

import Data.Array

isLess :: Ord a => a -> [a] -> Bool
isLess x = all (x <=)

isMore :: Ord a => a -> [a] -> Bool
isMore x = all (x >=)

isSaddle :: Ord a => a -> [a] -> [a] -> Bool
isSaddle x xs ys = isMore x xs && isLess x ys

ithRow :: Int -> Array i e -> [e]
ithRow i mat = [ mat!(i,j) | j <- [0..l] ]
  where l = snd $ snd $ bounds mat

ithColumn :: Array i e -> [e]

saddlePoints :: Array i e -> [i]
saddlePoints matrix = error "You need to implement this function."