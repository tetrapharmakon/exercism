module Allergies
  ( Allergen(..)
  , allergies
  , isAllergicTo
  ) where

data Allergen
  = Eggs
  | Peanuts
  | Shellfish
  | Strawberries
  | Tomatoes
  | Chocolate
  | Pollen
  | Cats
  deriving (Eq, Show)

aller :: [Allergen]
aller =
  [Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats]

{- 
  1. take the integer
  2. generate its base 2 ciphers
  3. zip with list of allergens IN ORDER
  4. filter all (1, Allergen) and discard all (0, Allergen) 
-}
toBase2 :: Int -> [Int]
toBase2 0 = []
toBase2 1 = [1]
toBase2 n = toBase2 (div n 2) ++ [mod n 2]

allergies :: Int -> [Allergen]
allergies score =
  let ns = reverse (toBase2 score)
   in map fst $ filter (\x -> snd x == 1) $ zip aller ns

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = allergen `elem` allergies score
