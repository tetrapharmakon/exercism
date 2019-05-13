module ETL (transform) where

import           Data.Char
import           Data.Map

transform :: Map Int String -> Map Char Int
transform legacyData = fromList $ concatMap (f toLower) (assocs legacyData)

f :: (b -> b) -> (a, [b]) -> [(b, a)]
f _ (_ , [])    = []
f g (n, x : xs) = (g x, n) : f g (n, xs)
