module TwelveDays (recite) where

import Data.List

recite :: Int -> Int -> [String]
recite start stop 
 | start == stop = [f start ]
 | otherwise     = [f i | i <- [start..stop]] --error "You need to implement this function."

things :: [String]
things = [ "twelve Drummers Drumming,"
         , "eleven Pipers Piping,"
         , "ten Lords-a-Leaping,"
         , "nine Ladies Dancing,"
         , "eight Maids-a-Milking,"
         , "seven Swans-a-Swimming,"
         , "six Geese-a-Laying,"
         , "five Gold Rings,"
         , "four Calling Birds,"
         , "three French Hens,"
         , "two Turtle Doves,"
         , "and a Partridge in a Pear Tree."]

nums :: [String]
nums = [ "first"
       , "second"
       , "third"
       , "fourth"
       , "fifth"
       , "sixth"
       , "seventh"
       , "eighth"
       , "ninth"
       , "tenth"
       , "eleventh"
       , "twelfth"]

f :: Int -> String
f 1 = "On the first day of Christmas my true love gave to me: a Partridge in a Pear Tree."
f n = "On the " ++ nums !! (n-1) ++ " day of Christmas my true love gave to me: "  ++ unwords (reverse $ take n (reverse things))