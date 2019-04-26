module Beer (song) where

import Data.List

pourBeer :: Int -> String
pourBeer 0 = "No more bottles of beer on the wall, no more bottles of beer.\n\
             \Go to the store and buy some more, 99 bottles of beer on the wall."
pourBeer 1 = "1 bottle of beer on the wall, 1 bottle of beer.\n\
             \Take it down and pass it around, no more bottles of beer on the wall.\n\n"
pourBeer n =  show n ++ " bottles of beer on the wall, " ++ show n ++ " bottles of beer.\n\
              \Take one down and pass it around, " ++ show (n-1) ++ " bottles of beer on the wall.\n\
              \\n"

sing = putStrLn $ intercalate "" ([pourBeer n | n <- reverse [2..99]] ++ [pourBeer 1, pourBeer 0])