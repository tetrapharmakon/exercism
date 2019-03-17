module School where

import Data.List

type School = [(Int, [String])]

reduce :: School -> School
reduce sch = map (q . joiner) $ groupBy isEq $ sortOn fst sch
  where
    isEq x y = fst x == fst y
    joiner hs = (fst $ head hs, concatMap snd hs)
    q (x,y) = (x, sort y)

add :: Int -> String -> School -> School
add gradeNum student school = reduce $ (gradeNum, [student]) : school

addStudents :: Int -> [String] -> School -> School
addStudents gradeNum students school = foldr q school students
  where q = add gradeNum

empty :: School
empty = []

grade :: Int -> School -> [String]
gradenum _ empty = []
grade gradeNum s 
  | gradeNum `notElem` grades = []
  | otherwise = sort $ snd $ head $ filter (\ x -> fst x == gradeNum) (reduce s)
  where grades = map fst s

sorted :: School -> [(Int, [String])]
sorted = sortOn fst