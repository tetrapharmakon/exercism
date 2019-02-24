module Queens (boardString, canAttack) where

import Data.List

line :: String
line = "_ _ _ _ _ _ _ _"

emptyBoard :: String
emptyBoard = unlines $ map (const line) [0..7]

unstripBoard :: [String] -> [String]
unstripBoard = map (intersperse ' ')

whiteOnBoard :: Int -> Int -> String
whiteOnBoard u v = 
  unlines $ unstripBoard [[f u v m n | m <- [0..7]] | n <- [0..7]]
    where 
      f i j m n = if j==m && i==n then 'W' else '_'

blackOnBoard :: Int -> Int -> String
blackOnBoard u v = 
  unlines $ unstripBoard [[f u v m n | m <- [0..7]] | n <- [0..7]]
    where 
      f i j m n = if i==m && j==n then 'B' else '_'
    

boardString :: Maybe (Int,Int) -> Maybe (Int,Int) -> String
boardString Nothing Nothing = emptyBoard 
boardString (Just (p,q)) Nothing = whiteOnBoard p q
boardString Nothing (Just (p,q)) = blackOnBoard p q
boardString (Just (p,q)) (Just (r,s)) = 
  unlines $ unstripBoard [[f p q r s m n | m <- [0..7]] | n <- [0..7]]
    where 
      f p q r s m n 
        | q==m && p==n = 'W' 
        | r==m && s==n = 'B' 
        | otherwise = '_'

canAttack :: (Int,Int) -> (Int,Int) -> Bool
canAttack queenA@(x,y) queenB@(p,q) 
  | x == p = True
  | y == q = True
  | y - q == x - p = True
  | y - q == p - x = True
  | otherwise = False