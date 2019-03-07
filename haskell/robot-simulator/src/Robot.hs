module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot { bear :: Bearing
                   , pos :: (Integer, Integer)
                   } deriving (Eq,Show)

bearing :: Robot -> Bearing
bearing = bear

coordinates :: Robot -> (Integer, Integer)
coordinates = pos

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

turnLeft :: Robot -> Robot
turnLeft (Robot North p) = Robot West p
turnLeft (Robot South p) = Robot East p
turnLeft (Robot East  p) = Robot North p
turnLeft (Robot West  p) = Robot South p

turnRight :: Robot -> Robot
turnRight (Robot North p) = Robot East p
turnRight (Robot South p) = Robot West p
turnRight (Robot East  p) = Robot South p
turnRight (Robot West  p) = Robot North p

add :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
add (a,b) (c,d) = (a+c, b+d)

advance :: Robot -> Robot
advance rob
  | b == North  = Robot North (p `add` (0,1))
  | b == East   = Robot East (p `add` (1,0))
  | b == South  = Robot South (p `add` (0,-1))
  | otherwise   = Robot West (p `add` (-1,0))
  where
    b = bearing rob
    p = pos rob

f :: Char -> (Robot -> Robot)
f 'L' = turnLeft 
f 'R' = turnRight
f 'A' = advance
f _ = undefined

move :: Robot -> String -> Robot
move robot instructions = composeAll (map f (reverse instructions)) robot

composeAll :: [Robot -> Robot] -> (Robot -> Robot)
composeAll = foldl (.) id