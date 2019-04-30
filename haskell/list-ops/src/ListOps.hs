module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
-- foldl' = foldr . flip
foldl' f z = acc
  where
    acc [] = z
    acc (x:xs) = acc xs `f` x

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z = acc 
  where
    acc [] = z
    acc (x:xs) = x `f` acc xs

length :: [a] -> Int
length = foldr (\x -> (+) 1) 0

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [x | x <- xs , p x ]

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat = foldr (++) []
