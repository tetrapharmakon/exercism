module BST
  ( BST
  , bstLeft
  , bstRight
  , bstValue
  , empty
  , fromList
  , insert
  , singleton
  , toList
  ) where

data BST a
  = Empty
  | Branch a (BST a) (BST a)
  deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft (Branch _ l _) = Just l
bstLeft _ = Nothing

bstRight :: BST a -> Maybe (BST a)
bstRight (Branch _ _ r) = Just r
bstRight _ = Nothing

bstValue :: BST a -> Maybe a
bstValue (Branch a _ _) = Just a
bstValue Empty = Nothing

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList [] = Empty
fromList (x:ys) =
  Branch x (fromList $ filter (< x) ys) (fromList $ filter (>= x) ys)

insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (Branch y l r)
  | x > y = Branch y l (insert x r)
  | otherwise = Branch y (insert x l) r

singleton :: a -> BST a
singleton x = Branch x Empty Empty

toList :: BST a -> [a]
toList Empty = []
toList (Branch a l r) = toList l ++ (a : toList r)
