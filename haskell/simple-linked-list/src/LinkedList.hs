module LinkedList
  ( LinkedList
  , datum
  , fromList
  , isNil
  , new
  , next
  , nil
  , reverseLinkedList
  , toList
  ) where

data LinkedList a
  = Nil
  | Cell a
         (LinkedList a)
  deriving (Eq, Show)

datum :: LinkedList a -> a
datum Nil        = error "datum on empty list"
datum (Cell n _) = n

fromList :: [a] -> LinkedList a
fromList = foldr Cell Nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _   = False

new :: a -> LinkedList a -> LinkedList a
new = Cell

next :: LinkedList a -> LinkedList a
next Nil         = error "next on empty list"
next (Cell _ ll) = ll

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = go Nil
  where
    go acc Nil         = acc
    go acc (Cell n ll) = go (Cell n acc) ll

toList :: LinkedList a -> [a]
toList Nil         = []
toList (Cell n ll) = n : toList ll
