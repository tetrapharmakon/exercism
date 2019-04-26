module WordProblem (answer) where


wat :: String
wat = "What"

is :: String
is = "is"

by :: String
by ="by"

f :: String -> String
f "plus" = "+" 
f "minus" = "-"
f "multiplied" = "x"
f "divided" = "/"
f xs = xs

-- answer :: String -> Maybe Integer
answer problem = map f $ filter (`notElem` [wat,is, by, "?"]) $ words sproblem
  where
    sproblem = filter (`notElem` "?;:,.!") problem
