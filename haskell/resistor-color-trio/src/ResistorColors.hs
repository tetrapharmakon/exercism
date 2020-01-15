module ResistorColors
  ( Color(..)
  , Resistor(..)
  , label
  , ohms
  ) where

import Data.List.Split

data Color
  = Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor =
  Resistor
    { bands :: (Color, Color, Color)
    }
  deriving (Show)

label :: Resistor -> String
label res
  | howMany0 == 1 = show (cutOhms res) ++ " kiloohms"
  | howMany0 == 2 = show (cutOhms res) ++ " megaohms"
  | howMany0 == 3 = show (cutOhms res) ++ " gigaohms"
  | otherwise = show (ohms res) ++ " ohms"
  where
    howMany0 = length $ takeWhile (== [0, 0, 0]) magnitude
    magnitude = chunksOf 3 $ digs res

digs :: Resistor -> [Int]
digs res =
  let ba = bands res
   in reverse $ map colToInt [f ba, s ba] ++ replicate (colToInt (t ba)) 0

ohms :: Resistor -> Int
ohms resistor = digsToNum (digs resistor)

cutOhms :: Resistor -> Int
cutOhms res = digsToNum gna
  where
    gna = concat $ dropWhile (== [0, 0, 0]) magnitude
    magnitude = chunksOf 3 $ digs res

digsToNum :: [Int] -> Int
digsToNum xs = sum $ zipWith (*) xs [fromInteger (10 ^ k) | k <- [0 ..]]

colToInt :: Color -> Int
colToInt Black = 0
colToInt Brown = 1
colToInt Red = 2
colToInt Orange = 3
colToInt Yellow = 4
colToInt Green = 5
colToInt Blue = 6
colToInt Violet = 7
colToInt Grey = 8
colToInt White = 9

f :: (a, b, c) -> a
f (a, _, _) = a

s :: (a, b, c) -> b
s (_, b, _) = b

t :: (a, b, c) -> c
t (_, _, c) = c
