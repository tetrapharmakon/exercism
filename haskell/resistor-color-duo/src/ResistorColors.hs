module ResistorColors (Color(..), value) where

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Eq, Show, Read)

value :: [Color] -> Int
value cs = (read $ concatMap show mr) :: Int
  where
    reader c
      | c == Black   = 0
      | c == Brown   = 1
      | c == Red     = 2
      | c == Orange  = 3
      | c == Yellow  = 4
      | c == Green   = 5
      | c == Blue    = 6
      | c == Violet  = 7
      | c == Grey    = 8
      | c == White   = 9
      | otherwise    = undefined
    mr = map reader cs