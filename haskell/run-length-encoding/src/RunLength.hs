module RunLength where

import Data.List
import Data.Char
import Data.List.Split

rleCompress :: Eq a => [a] -> [(a, Int)]
rleCompress t = zip (map head tt) (map length tt) 
  where tt = group t

rleReduce :: (Eq a, Ord a) => [(a, Int)] -> [(a, Int)]
rleReduce [] = []
rleReduce a = filter appears $ map red $ groupBy alike $ sortOn fst a
  where
    appears x = snd x /= 0
    alike x y = fst x == fst y
    red w = (fst $ head w, sum $ map snd w)

g :: String -> [String]
g [] = []
g (x:y) = undefined

append :: a -> [a] -> [a]
append a [] = [a]
append a [x] = [x,a]
append a (x:xs) = x : append a xs

-- decode :: String -> String
decode encodedText = concatMap g $ chunksOf 2 encodedText

encode :: String -> String
encode text = filter (/='1') $ concatMap f $ rleReduce $ rleCompress text
  where f (x,n) = append x (show n)