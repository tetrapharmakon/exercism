module Garden
  ( Plant(..)
  , garden
  , lookupPlants
  ) where

import Data.List
import Data.List.Split
import qualified Data.Map as M hiding (map)
import Data.Maybe

stringToGarden = map f
  where
    f 'C' = Clover
    f 'G' = Grass
    f 'R' = Radishes
    f 'V' = Violets
    f _ = undefined

data Plant
  = Clover
  | Grass
  | Radishes
  | Violets
  deriving (Eq, Show)

type Garden = M.Map String [Plant]

garden :: [String] -> String -> Garden
garden students = M.fromList . zip students . p
  where
    p =
      map (stringToGarden . concat) .
      chunksOf 2 . transpose . splitWhen (== '\n')

lookupPlants :: String -> Garden -> [Plant]
lookupPlants = oddNEvens . fromJust . Data.List.lookup

oddNEvens xs = odds xs ++ evens xs
  where
    odds [] = []
    odds [x] = [x]
    odds (x:y:xs) = x : odds xs
    evens [] = []
    evens [x] = []
    evens (x:y:xs) = y : evens xs
