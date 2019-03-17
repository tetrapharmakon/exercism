module Diamond (diamond) where

import Data.Maybe

mirrorList :: [a] -> [a]
mirrorList xs = xs ++ reverse (init xs)

padLR :: Int -> String -> String
padLR n xs = nspaces ++ xs ++ nspaces
  where nspaces = map (const ' ') [1..n]

dilate :: Int -> Char -> String
dilate n xs = [xs] ++ nspaces ++ [xs]
  where nspaces = map (const ' ') [1..n]

diamond :: Char -> Maybe [String]
diamond c 
  | c `notElem` ['A'..'Z'] = Nothing
  | otherwise              =  Just $ mirrorList nicelySpaced
  where
    s = ['A'..c]
    n = length s
    nicelySpaced = padLR (n-1) "A" : [ padLR (n-k-1) (dilate (2*k-1) (s !! k)) 
                                     | k <- [1..n-1]]