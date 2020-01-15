module Base where

data Error a
  = InvalidInputBase
  | InvalidOutputBase
  | InvalidDigit a
  deriving (Show, Eq)

rebase :: Integer -> Integer -> Integer -> Either (Error Integer) Integer
rebase num from to
  | any (> from) (digits num) = Left $ InvalidDigit num
  | from <= 0 = Left InvalidInputBase
  | to <= 0 = Left InvalidOutputBase
  | otherwise = Right $ (num `fromBase` from) `inBase` to

-- converts in base 10 the number n__b
-- e.g : 421 `fromBase` 5 == 111
fromBase :: Integer -> Integer -> Integer
fromBase 0 _ = 0
fromBase n b = foldl (f b) n0 ns
  where
    n0 = last (digits n)
    ns = tail $ reverse $ digits n
    f b m p = m * b + p

digits :: Integer -> [Integer]
digits n = reverse $ map (\x -> read [x] :: Integer) $ show n

-- converts in base b the number n__10
-- e.g. : 421 `inBase` 5 = 3141
inBase :: Integer -> Integer -> Integer
inBase 0 _ = 0
inBase n b = read (map (head . show) s) :: Integer
  where
    s = reverse $ map fst $ takeWhile (/= (0, 0)) $ iterate q (f n)
    f m = radix m b
    q = f . snd
    radix m n = (m `mod` n, m `div` n)
