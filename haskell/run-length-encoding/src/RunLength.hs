module RunLength where

import Data.Char
import Data.List

rleCompress :: Eq a => [a] -> [(Int, a)]
rleCompress t = zip (map length tt) (map head tt)
  where
    tt = group t

lettersAndSpace :: String
lettersAndSpace = ' ' : ['A' .. 'Z'] ++ ['a' .. 'z']

allLetters :: String -> Bool
allLetters = all (`elem` lettersAndSpace)

intersperseDigits :: String -> String
intersperseDigits encodedText =
  if isAlpha (head encodedText)
    then f ('1' : encodedText)
    else f encodedText
  where
    f = concatMap fu . splitOnDigit
    fu [] = []
    fu [x] = [x]
    fu xs =
      if allLetters xs
        then intersperse '1' xs
        else xs

decode :: String -> String
decode [] = []
decode text =
  concatMap (g . f) $
  uncurry zip $ evenOddSplit $ splitOnDigit $ intersperseDigits text
  where
    f (a, b) = (read a :: Integer, head b)
    g (n, y) = map (const y) [1 .. n]

encode :: String -> String
encode = concat . filter (/= "1") . splitOnDigit . concatMap f . rleCompress
  where
    f (n, x) = show n ++ [x]

groupOn :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupOn _ [] = []
groupOn proj (x:xs) = (x', x : ys) : groupOn proj zs
  where
    x' = proj x
    (ys, zs) = span ((== x') . proj) xs

evenOddSplit :: [a] -> ([a], [a])
evenOddSplit [] = ([], [])
evenOddSplit (x:xs) = (x : o, e)
  where
    (e, o) = evenOddSplit xs

splitOnDigit :: String -> [String]
splitOnDigit = map snd . groupOn (`notElem` lettersAndSpace)
