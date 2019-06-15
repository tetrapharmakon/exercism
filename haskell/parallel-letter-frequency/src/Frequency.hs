{-# LANGUAGE OverloadedStrings #-}

module Frequency
  ( frequency
  ) where

import Control.Parallel.Strategies (parListChunk, rseq, using)
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as M (Map, empty, fromList, unionsWith)
import Data.Text (Text, pack, unpack)

rleCompress :: Eq a => [a] -> [(a, Int)]
rleCompress t = zip (map head tt) (map Prelude.length tt)
  where
    tt = group t

rleReduce :: (Eq a, Ord a) => [(a, Int)] -> [(a, Int)]
rleReduce [] = []
rleReduce a = map red $ groupBy alike $ sortOn fst a
  where
    alike x y = fst x == fst y
    red w = (fst $ head w, sum $ map snd w)

frequency :: Int -> [Text] -> M.Map Char Int
frequency _ [] = M.empty
frequency nWorkers texts = M.unionsWith (+) freqs
  where
    utexts = map toLower $ filter isLetter $ concatMap Data.Text.unpack texts
    chunks = chunksOf nWorkers utexts
    strategy = parListChunk chunks rseq
    freqs = M.fromList (rleReduce $ rleCompress utexts) `using` strategy
