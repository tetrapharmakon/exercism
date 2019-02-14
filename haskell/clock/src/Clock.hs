{-# LANGUAGE TypeOperators #-}

module Clock (addDelta, fromHourMin, toString) where

newtype Clock = Clock (Int, Int) deriving (Eq,Show)

getH :: Clock -> Int
getH (Clock (a,_)) = a

getM :: Clock -> Int
getM (Clock (_,b)) = b

sumClocks :: Clock -> Clock -> Clock
sumClocks c d = reduce $ Clock (getH c + getH d, getM c + getM d)

hourFormat :: Int -> String
hourFormat n = if n < 10 then '0' : show n else show n

reduce :: Clock -> Clock
reduce c = Clock (mod (getH c + quot (getM c) 60) 24, mins)
  where mins = getM c `mod` 60

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minu = Clock (hour, minu)

toString :: Clock -> String
toString clock = hourFormat (getH rclock) ++ ":" ++ hourFormat (getM rclock)
  where rclock = reduce clock

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour minu clock = reduce $ sumClocks clock (fromHourMin hour minu)