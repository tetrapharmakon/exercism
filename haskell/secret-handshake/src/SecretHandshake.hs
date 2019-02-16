module SecretHandshake (handshake) where

binDigits :: Int -> [Int]
binDigits n = [mod n 2^i | i <- takeWhile (\x -> 2^x <= n) [0..]]

g :: (Int,Int) -> String
g (0,_) = undefined
g (1,n) 
  | n == 0 = "wink"
  | n == 1 = "double blink"
  | n == 2 = "close your eyes"
  | n == 3 = "jump"
  | otherwise = undefined
g(_,_) = undefined

handshake :: Int -> [String]
handshake n = map g $ filter (\x -> fst x /= 0) $ zip (reverse s) [0..]
  where s = binDigits n
