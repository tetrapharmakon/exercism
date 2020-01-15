module Sieve
  ( primesUpTo
  ) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
-- import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))
primesUpTo :: Integer -> [Integer]
primesUpTo n =
  concatMap fst $ takeWhile (/= ([], [])) $ iterate (f . snd) $ f [2 .. n]
  where
    f :: [Integer] -> ([Integer], [Integer])
    f [] = ([], [])
    f (x:xs) = ([x], filter (\a -> mod a x /= 0) xs)
