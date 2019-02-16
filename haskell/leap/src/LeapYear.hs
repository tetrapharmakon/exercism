module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = x || (y && not z)
  where
    x = year `rem` 400 == 0
    y = year `rem` 4 == 0
    z = year `rem` 100 == 0