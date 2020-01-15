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

values :: [Int]
values = [1, 2, 4, 8, 16, 32, 64, 128]

allergies :: Int -> [Allergen]
allergies score = error "You need to implement this function."

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = error "You need to implement this function."
