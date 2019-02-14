module HelloWorld (hello) where

hello :: String
hello = "Hello world"

solve :: IO ()
solve = putStrLn hello