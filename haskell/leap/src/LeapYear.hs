module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = (x && not y) || (z && y)
  where
    x = year `rem` 4 == 0
    y = year `rem` 100 == 0
    z = quot year 100 `rem` 4 == 0