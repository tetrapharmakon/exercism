module Palindromes
  ( largestPalindrome
  , smallestPalindrome
  ) where

import Data.List

factors :: Integral a => a -> [a]
factors n = [i | i <- [1 .. n], mod n i == 0]

isPaly :: Integer -> Bool
isPaly n = palyndrome (digits n)
  where
    palyndrome xs = xs == reverse xs
    digits num = map (\x -> read [x] :: Integer) (show num)

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor =
  if null palys
    then Nothing
    else Just (maxPal, helperino (f maxPal minFactor maxFactor))
  where
    prods =
      nub [i * j | i <- [minFactor .. maxFactor], j <- [minFactor .. maxFactor]]
    palys = filter isPaly prods
    maxPal = maximum palys
    f n low upp = filter (bothElem low upp) [(i, div n i) | i <- factors n]
    bothElem a b (x, y) = a <= x && x <= y && y <= b

smallestPalindrome ::
     Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor =
  if null palys
    then Nothing
    else Just (minPal, helperino (f minPal minFactor maxFactor))
  where
    prods =
      nub [i * j | i <- [minFactor .. maxFactor], j <- [minFactor .. maxFactor]]
    palys = filter isPaly prods
    minPal = minimum palys
    f n low upp = filter (bothElem low upp) [(i, div n i) | i <- factors n]
    bothElem a b (x, y) = a <= x && x <= y && y <= b

helperino :: [a] -> [a]
helperino xs = take (div l 2 + 1) xs
  where
    l = length xs
