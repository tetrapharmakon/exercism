module ArmstrongNumbers where

digs :: Integral x => x -> [x]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

armstrong :: Integral a => a -> Bool
armstrong n = n == sum (zipWith (^) (digs n) (map (const len) [1 ..]))
  where
    len = toInteger $ length (digs n)
