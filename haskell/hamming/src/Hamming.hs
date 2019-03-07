module Hamming (distance) where

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

hamDist :: String -> String -> Int
hamDist xs ys = count True $ zipWith (/=) xs ys

distance :: String -> String -> Maybe Int
distance xs ys 
  | length xs /= length ys = Nothing
  | otherwise = Just (hamDist xs ys)