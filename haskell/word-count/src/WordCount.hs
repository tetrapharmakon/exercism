module WordCount (wordCount) where

import Data.List
import Data.Char
import Data.List.Split

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (== x) xs) : pack (dropWhile (== x) xs)
 

rleCompress :: Eq a => [a] -> [(a, Int)]
rleCompress [] = []
rleCompress y@(x:xs) =
  (x, length $ head $ pack y) : rleCompress (dropWhile (== x) xs)
 
rleReduce :: (Eq a, Ord a) => [(a, Int)] -> [(a, Int)]
rleReduce [] = []
rleReduce a = filter appears $ 
  map reducer $ 
  groupBy alike $ 
  sortOn fst a
  where
    appears x = snd x /= 0
    alike x y = fst x == fst y
    reducer w = (fst $ head w, sum $ map snd w)

removeTicks :: String -> String
removeTicks ('\'':xs) 
  | last xs == '\'' = init xs
  | otherwise = xs
removeTicks as = as

wordCount :: String -> [(String, Int)]
wordCount xs = rleReduce $ rleCompress parole
  where 
    parole = filter (/="") $ 
      concatMap (splitOneOf ", :;.!&@$%^\n\t\r" . removeTicks) $ 
      words $ map toLower xs
