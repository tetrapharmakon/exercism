module Minesweeper (annotate) where

type Board = [String]

-- nameless helpers:
-- f takes the correct subset of [n-1,n,n+1] in edge cases
-- g counts how many mines are around an empty cell
-- and return a mine/space otherwise

f :: Int -> Int -> [Int]
f n m = filter (\x -> x>=0 && x<=m) [n-1,n,n+1]

g :: Int -> Int -> Board -> Char
g i j b
  | b !! i !! j == '*' = '*'
  | countMines i j b == 0 = ' '
  | otherwise = head (show (countMines i j b))

contour :: Int -> Int -> Board -> String
contour i j arr 
  | n == 0 = [head (arr !! u) | u <- f i m]
  | m == 0 = [head arr !! v | v <- f j n]
  | otherwise = concat [[arr !! u !! v | u <- f i m] | v <- f j n]
  where 
    n = length (head arr) - 1 -- #cols
    m = length arr - 1 -- #rows

count :: Eq a => a -> [a] -> Int    
count x = length . filter (x==)

countMines :: Int -> Int -> Board -> Int
countMines i j arr = count '*' (contour i j arr)

annotate :: Board -> Board
annotate [] = []
annotate [[]] = [""]
annotate board 
  | m == 0    = [[ g 0 j board | j <- [0..n]]]
  | n == 0    = map (:[]) (head [[ g i 0 board | i <- [0..m]]])
  | otherwise = [[ g i j board | i <- [0..m]] | j <- [0..n]]
  where 
    m = length board -1
    n = length (head board) -1
