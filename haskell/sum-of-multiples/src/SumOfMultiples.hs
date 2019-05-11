module SumOfMultiples (sumOfMultiples) where

divides :: [Integer] -> Integer -> Bool
divides ms n = any (\x -> n `mod` x == 0) $ filter (/= 0) ms

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples fac k = sum [ n | n <- [1..k-1], divides fac n ]
