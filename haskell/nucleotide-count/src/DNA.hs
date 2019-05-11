module DNA (nucleotideCounts, Nucleotide(..)) where

import           Data.List
import           Data.Map  hiding (filter, map)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read)

nucleos :: [(Nucleotide, Int)]
nucleos = zip [A,C,G,T] [0,0,0,0]

niceNucleotide :: String -> Bool
niceNucleotide [] = True
niceNucleotide ns = all (`elem` "ACGT") ns

nucleoCompress :: String -> [(Char, Int)]
nucleoCompress t = zip (map head tt) (map length tt)
  where tt = group $ sort t

rleReduce :: (Eq a, Ord a) => [(a, Int)] -> [(a, Int)]
rleReduce [] = []
rleReduce a = map red $ groupBy alike $ sortOn fst a
  where
    alike x y = fst x == fst y
    red w = (fst $ head w, sum $ map snd w)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs
  | not $ niceNucleotide xs = Left "erore!1!!"
  | otherwise = Right $ fromList readxs
  where
    readxs = rleReduce $ map f (nucleoCompress xs) ++ nucleos
    f (x,y) = ( read [x] :: Nucleotide , y )

