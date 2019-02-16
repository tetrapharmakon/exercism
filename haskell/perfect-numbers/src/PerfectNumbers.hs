module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient 
                    | Perfect 
                    | Abundant 
                    deriving (Eq, Show)

-- not the cheapest way to compute factors...
-- but I guess you don't want me to use Data.Primes

factors :: Int -> [Int]
factors n = init $ filter (\x -> mod n x == 0) [1..n]

aliquot :: Int -> Int
aliquot n = sum (factors n)

perfection :: Int -> Int
perfection n = n - aliquot n

classify :: Int -> Maybe Classification
classify n 
  | n <= 0 = Nothing
  | otherwise = 
    if      perfection n < 0 then Just Abundant 
    else if perfection n > 0 then Just Deficient
    else                          Just Perfect
