module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Data.Vector (Vector)

data Matrix a = [Vector a] deriving (Eq, Show)

cols :: Matrix a -> Int
cols matrix = 

column :: Int -> Matrix a -> Vector a
column x matrix = 

flatten :: Matrix a -> Vector a
flatten matrix = error "You need to implement this function."

fromList :: [[a]] -> Matrix a
fromList xss = error "You need to implement this function."

fromString :: Read a => String -> Matrix a
fromString xs = error "You need to implement this function."

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape dimensions matrix = error "You need to implement this function."

row :: Int -> Matrix a -> Vector a
row x matrix = error "You need to implement this function."

rows :: Matrix a -> Int
rows matrix = length matrix

shape :: Matrix a -> (Int, Int)
shape matrix = error "You need to implement this function."

transpose :: Matrix a -> Matrix a
transpose matrix = error "You need to implement this function."
