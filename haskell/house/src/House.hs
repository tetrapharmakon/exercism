module House (rhyme) where

things = reverse [ "the horse and the hound and the horn\nthat belonged to"
         , "the farmer sowing his corn\nthat kept"
         , "the rooster that crowed in the morn\nthat woke"
         , "the priest all shaven and shorn\nthat married"
         , "the man all tattered and torn\nthat kissed"
         , "the maiden all forlorn\nthat milked"
         , "the cow with the crumpled horn\nthat tossed"
         , "the dog\nthat worried"
         , "the cat\nthat killed"
         , "the rat\nthat ate"
         , "the malt\nthat lay in"
         ]

rhyme :: String
-- rhyme [] = "This is the house that Jack built."
rhyme = concat ["This is ", something, " the house that Jack built."]
  where
    something = head things

lastButOne :: a -> [a] -> [a]
lastButOne x [] = [x]
lastButOne x [y] = [x,y]
lastButOne x (y:ys) = y : lastButOne x ys