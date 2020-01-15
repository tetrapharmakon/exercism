module Deque
  ( Deque
  , mkDeque
  , pop
  , push
  , shift
  , unshift
  ) where

import Control.Applicative

data Deque a
  = Lefties a (Deque a)
  | Middle a (Deque a) (Deque a)
  | Righties a (Deque a)
  | Null
  deriving (Eq, Show)

mkDeque :: IO (Deque a)
mkDeque = IO <$> Null

pop :: Deque a -> IO (Maybe a)
pop deque = error "You need to implement this function."

push :: Deque a -> a -> IO ()
push deque x = error "You need to implement this function."

unshift :: Deque a -> a -> IO ()
unshift deque x = error "You need to implement this function."

shift :: Deque a -> IO (Maybe a)
shift deque = error "You need to implement this function."
-- front :: Deque a -> a
-- front (Lefties a (Deque a)) = undefined
-- back :: Deque a -> a
-- back deque = undefined
