module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n 
  | n <= 0 = Nothing
  | otherwise = Just $ toInteger $ length $ takeWhile (/=1) $ iterate colStep n

colStep :: Integer -> Integer
colStep n
  | even n = quot n 2
  | otherwise = 3 * n + 1