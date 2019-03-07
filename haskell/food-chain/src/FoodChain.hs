module FoodChain (song) where

animals :: [String]
animals = ["fly"
          , "spider"
          , "bird"
          , "cat"
          , "dog"
          , "goat"
          , "cow"]

f :: String -> String
f "fly" = "I don't know why she swallowed the fly. Perhaps she'll die."
f "spider" = "It wriggled and jiggled and tickled inside her"
f "bird" = "How absurd to swallow a bird!"
f "cat" = "Imagine that, to swallow a cat!"
f "dog" = "What a hog, to swallow a dog!"
f "goat" = "Just opened her throat and swallowed a goat!"
f "cow" = "I don't know how she swallowed a cow!"
f _ = undefined

song' :: Int -> String
song' n = "I know an old lady who swallowed a " 
  ++ head lst 
  ++ "\n" 
  ++ f (head lst) 
  ++ "\nShe swallowed the " 
  ++ head lst 
  ++ " to catch the " 
  ++ head (tail lst) 
  ++ "\n"
  ++ f (head animals)
    where lst = drop n (reverse animals)


song :: String
song =
    "I know an old lady who swallowed a fly.\n\
    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
    \\n\
    \I know an old lady who swallowed a spider.\n\
    \It wriggled and jiggled and tickled inside her.\n\
    \She swallowed the spider to catch the fly.\n\
    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
    \\n\
    \I know an old lady who swallowed a bird.\n\
    \How absurd to swallow a bird!\n\
    \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
    \She swallowed the spider to catch the fly.\n\
    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
    \\n\
    \I know an old lady who swallowed a cat.\n\
    \Imagine that, to swallow a cat!\n\
    \She swallowed the cat to catch the bird.\n\
    \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
    \She swallowed the spider to catch the fly.\n\
    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
    \\n\
    \I know an old lady who swallowed a dog.\n\
    \What a hog, to swallow a dog!\n\
    \She swallowed the dog to catch the cat.\n\
    \She swallowed the cat to catch the bird.\n\
    \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
    \She swallowed the spider to catch the fly.\n\
    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
    \\n\
    \I know an old lady who swallowed a goat.\n\
    \Just opened her throat and swallowed a goat!\n\
    \She swallowed the goat to catch the dog.\n\
    \She swallowed the dog to catch the cat.\n\
    \She swallowed the cat to catch the bird.\n\
    \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
    \She swallowed the spider to catch the fly.\n\
    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
    \\n\
    \I know an old lady who swallowed a cow.\n\
    \I don't know how she swallowed a cow!\n\
    \She swallowed the cow to catch the goat.\n\
    \She swallowed the goat to catch the dog.\n\
    \She swallowed the dog to catch the cat.\n\
    \She swallowed the cat to catch the bird.\n\
    \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
    \She swallowed the spider to catch the fly.\n\
    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
    \\n\
    \I know an old lady who swallowed a horse.\n\
    \She's dead, of course!\n"
