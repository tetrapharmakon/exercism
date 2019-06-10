module DND
  ( Character(..)
  , ability
  , modifier
  , character
  ) where

import Data.List
import Test.QuickCheck

data Character =
  Character
    { name :: String
    , strength :: Int
    , dexterity :: Int
    , constitution :: Int
    , intelligence :: Int
    , wisdom :: Int
    , charisma :: Int
    , hitpoints :: Int
    }
  deriving (Show, Eq)

instance Arbitrary Character where
  arbitrary = do
    name <- listOf (elements ['a' .. 'z'])
    str <- ability
    dex <- ability
    con <- ability
    int <- ability
    wis <- ability
    cha <- ability
    let hp = makeHP con
    return (Character name str dex con int wis cha hp)

modifier :: Int -> Int
modifier n = div (n - 10) 2

makeHP :: Int -> Int
makeHP n = 10 + modifier n

ability :: Gen Int
ability = sum . tail . sort <$> vectorOf 4 (choose (1, 6))

character :: Gen Character
character = arbitrary
