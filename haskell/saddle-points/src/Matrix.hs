module Matrix where

import Data.Array
import Data.Ix

isLess :: (Ix a, Ord a, Ord b) => ((a, a),b) -> [((a, a),b)] -> Bool
isLess x xs = all (snd x <=) $ map snd xs

isMore :: (Ix a, Ord a, Ord b) => ((a, a),b) -> [((a, a),b)] -> Bool
isMore x xs = all (snd x >=) $ map snd xs

test :: (Ix i, Ord i) => i -> Array i e -> Bool
test i mat = undefined

saddlePoints :: Array i e -> [i]
saddlePoints mat = map fst [assocs mat !! i | i <- [0..l], test i mat]
  where
    l = length (assocs mat)

ithRow :: (Ix a, Ix b1) => a -> Array (a, b1) b2 -> [((a, b1), b2)]
ithRow i mat = filter q $ assocs mat
  where
    q x = fst (fst x) == i

ithCol :: (Ix a1, Ix a2) => a2 -> Array (a1, a2) b -> [((a1, a2), b)]
ithCol i mat = filter q $ assocs mat
  where
    q x = snd (fst x) == i