module Say
  ( inEnglish
  ) where

build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
  where
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n = l `c` splitter (drop i l) c n

inEnglish :: Integer -> Maybe String
inEnglish n = error "You need to implement this function."

-- palabras :: Integer -> Maybe String
palabras n
  | length (show n) > 12 = Nothing
  | otherwise =
    case length gnogni of
      1 -> undefined
      2 -> undefined
      3 -> undefined
      4 -> undefined
      _ -> undefined
  where
    digs = undefined
    gnogni = reverse $ map reverse $ chunksOf 3 (reverse digs)
    sayTens = 
