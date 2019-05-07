module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum sum = [(a,b,c) | a <- [1..2*q]
                               , b <- [a..2*q]
                               , c <- [b..2*q]
                               , a + b + c == sum
                               , a^2 + b^2 == c^2
                               ]
                                 where
                                  q = quot sum 3
