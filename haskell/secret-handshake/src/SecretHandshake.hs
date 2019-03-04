module SecretHandshake (handshake) where

binDigits :: Int -> [Int]
binDigits 0 = []
binDigits x =  binDigits (x `div` 2) ++ [x `mod` 2]

g :: (Int,Int) -> String
g (0, _) = undefined
g (1, 0) = "wink"
g (1, 1) = "double blink"
g (1, 2) = "close your eyes"
g (1, 3) = "jump"
g (1, 4) = "R"
g (_, _) = undefined

handshake :: Int -> [String]
handshake n 
  | n == 0 = []
  | "R" == last mapGFs = reverse (init mapGFs)
  | otherwise = map g fs
    where 
      s  = binDigits n
      fs = filter (\x -> fst x /= 0) $ zip (reverse s) [0..]
      mapGFs = map g fs