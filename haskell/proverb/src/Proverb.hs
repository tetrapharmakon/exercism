module Proverb(recite) where

things = ["nail", "shoe", "horse", "rider", "message", "battle", "kingdom"]

recite :: [String] -> String
recite [] = ""
recite [_] = "And all for the want of a nail."
recite xss = unwords ["For want of a", head rxss, "the", head (tail rxss), "was lost.\n"] ++ recite (tail rxss)
  where rxss = reverse xss