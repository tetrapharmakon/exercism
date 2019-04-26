module LinkedList where

data LinkedList a = Seq a (LinkedList a) | Null deriving (Eq, Show)

fromList :: [a] -> LinkedList a
fromList [] = Null
fromList [a] = Seq a Null
fromList (x:xs) = Seq x (fromList xs)

isNil :: LinkedList a -> Bool
isNil Null = True
isNil _    = False

new :: a -> LinkedList a -> LinkedList a
new x = Seq x

nil :: LinkedList a
nil = Null

datum :: LinkedList a -> a
datum l = case l of
        Seq a _ -> a
        Null -> error "empty list"

next :: LinkedList a -> LinkedList a
next l = case l of
        Seq _ a -> a
        Null -> Null

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList Null = Null
reverseLinkedList (Seq a x) =  g x (Seq a Null)
  where
    g Null x = x
    g (Seq a xs) e = g xs (Seq a e)

toList :: LinkedList a -> [a]
toList Null = []
toList lls = datum lls : toList (next lls)
