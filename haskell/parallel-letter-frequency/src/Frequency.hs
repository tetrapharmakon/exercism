{-# LANGUAGE OverloadedStrings #-}

module Frequency
  ( frequency
  ) where

import Control.Parallel.Strategies hiding (parMap)
import Data.Char
import Data.List
import qualified Data.Map as M (Map, empty, fromList)
import qualified Data.Text as T (Text, pack, unpack)

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

parMap :: (a -> b) -> [a] -> Eval [b]
parMap _ [] = return []
parMap f (a:as) = do
  b <- rpar (f a)
  bs <- parMap f as
  return (b : bs)

frequency :: Int -> [T.Text] -> M.Map Char Int
frequency _ [] = M.empty
frequency _ texts = M.fromList (rleReduce $ rleCompress utexts)
  where
    utexts =
      runEval $ parMap toLower $ filter isLetter $ concatMap T.unpack texts
