module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock { hour :: Int , mins :: Int } deriving (Show)

instance Eq Clock where
  Clock h1 m1 == Clock h2 m2 = (rh1 - rh2) `mod` 24 == 0 && (rm1 - rm2) `mod` 60 == 0
    where 
      rh1 = hour (reduce (Clock h1 m1))
      rh2 = hour (reduce (Clock h2 m2))
      rm1 = mins (reduce (Clock h1 m1))
      rm2 = mins (reduce (Clock h2 m2))

reduce :: Clock -> Clock
reduce c = Clock rhour rmins
  where 
    rmins = mins c `mod` 60
    rhour = mod (hour c + div (mins c) 60) 24

sumClocks :: Clock -> Clock -> Clock
sumClocks c d = reduce $ Clock (hour rc + hour rd) (mins rc + mins rd)
  where 
    rc = reduce c
    rd = reduce d

hourFormat :: Int -> String
hourFormat n = if n < 10 then '0' : show n else show n

fromHourMin :: Int -> Int -> Clock
fromHourMin = Clock

toString :: Clock -> String
toString c = hourFormat (hour rc) ++ ":" ++ hourFormat (mins rc)
  where rc = reduce c

addDelta :: Int -> Int -> Clock -> Clock
addDelta h m c = reduce $ sumClocks c (Clock h m)