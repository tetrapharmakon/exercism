module Meetup where

import Data.List
import Data.Maybe
import Data.Time.Calendar

data Weekday
  = Mo
  | Tu
  | We
  | Th
  | Fr
  | Sa
  | Su
  deriving (Eq, Show, Read)

data Schedule
  = First
  | Second
  | Third
  | Fourth
  | Last
  | Teenth

instance Enum Weekday where
  toEnum i =
    case mod i 7 of
      0 -> Su
      1 -> Mo
      2 -> Tu
      3 -> We
      4 -> Th
      5 -> Fr
      _ -> Sa
  fromEnum Mo = 1
  fromEnum Tu = 2
  fromEnum We = 3
  fromEnum Th = 4
  fromEnum Fr = 5
  fromEnum Sa = 6
  fromEnum Su = 7
  enumFromTo wd1 wd2
    | wd1 == wd2 = [wd1]
  enumFromTo wd1 wd2 = wd1 : enumFromTo (succ wd1) wd2
  enumFromThenTo wd1 wd2 wd3
    | wd2 == wd3 = [wd1, wd2]
  enumFromThenTo wd1 wd2 wd3 =
    wd1 : enumFromThenTo wd2 (toEnum $ (2 * fromEnum wd2) - fromEnum wd1) wd3

dayOfWeek :: Day -> Weekday
dayOfWeek (ModifiedJulianDay d) = toEnum $ fromInteger $ d + 3

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay Teenth weekday year month = fromGregorian year month giorno
  where
    giorno = fst $ fromJust $ find isWeekday $ daysInRange 13 19
    isWeekday x = snd x == weekday
    daysInRange m n = zip [m .. n] $ map (findWeekday year month) [m .. n]
meetupDay sched weekday year month = fromGregorian year month giorno
  where
    giorno =
      case sched of
        First -> fst $ head daysies
        Second -> fst $ daysies !! 1
        Third -> fst $ daysies !! 2
        Fourth -> fst $ daysies !! 3
        _ -> fst $ last daysies
    daysies = filter isWeekday $ daysInRange 1 31
    isWeekday x = snd x == weekday
    daysInRange m n = zip [m .. n] $ map (findWeekday year month) [m .. n]

findWeekday :: Integer -> Int -> Int -> Weekday
findWeekday year month day = dayOfWeek (fromGregorian year month day)
