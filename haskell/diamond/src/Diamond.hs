module Diamond (diamond) where

letters :: String
letters = ['A'..'Z']

pad :: Int -> Char -> String
pad n xs = nspaces ++ [xs] ++ nspaces
  where nspaces = map (const ' ') [1..n]

dilate :: Int -> Char -> String
dilate n xs = [xs] ++ nspaces ++ [xs]
  where nspaces = map (const ' ') [1..n]

diamond :: Char -> Maybe [String]
diamond = error "You need to implement this function"
