{-# LANGUAGE TemplateHaskell #-}

module Person where

import           Control.Lens
import           Data.Time.Calendar


data Name = Name { _foreNames :: String
                 , _surName   :: String
                 } deriving Show
makeLenses ''Name

data Address = Address { _street      :: String
                       , _houseNumber :: Int
                       , _place       :: String
                       , _country     :: String
                       } deriving Show
makeLenses ''Address

data Born = Born { _bornAt :: Address
                 , _bornOn :: Day
                 } deriving Show
makeLenses ''Born

data Person = Person { _name    :: Name
                     , _born    :: Born
                     , _address :: Address
                     } deriving Show
makeLenses ''Person

bornStreet :: Born -> String
bornStreet = view (bornAt . street)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet = set (address . street)

setBirthMonth :: Int -> Person -> Person
setBirthMonth month = over (born . bornOn) (changeMonth month)

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person =
  over (born . bornAt . street) f $
  over (address . street) f person

changeMonth :: Int -> Day -> Day
changeMonth month day = fromGregorian yearOfDay month dayOfDay
  where
    yearOfDay = fst' (toGregorian day)
    dayOfDay  = trd' (toGregorian day)
    fst' (a,_,_) = a
    trd' (_,_,c) = c
