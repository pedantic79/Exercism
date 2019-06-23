{-# LANGUAGE FlexibleContexts #-}
module Deque (Deque, mkDeque, pop, push, shift, unshift) where
import Data.IORef

data DoubleList a = Nil | Node (DoubleList a) a (DoubleList a) deriving Show
type Deque a = IORef (DoubleList a)

mkDeque :: IO (Deque a)
mkDeque = newIORef Nil

pop :: Deque a -> IO (Maybe a)
pop deque = do
  (value, dl) <- pop' <$> readIORef deque
  writeIORef deque dl
  pure value

pop' :: DoubleList a -> (Maybe a, DoubleList a)
pop' Nil            = (Nothing, Nil)
pop' (Node l x Nil) = (Just x, l)
pop' (Node l x r)   = Node l x <$> pop' r

push :: Deque a -> a -> IO ()
push deque = modifyIORef' deque . push'

push' :: a -> DoubleList a -> DoubleList a
push' x Nil = Node Nil x Nil
push' x deq = Node deq x Nil

unshift :: Deque a -> a -> IO ()
unshift deque = modifyIORef' deque . unshift'
  where
    unshift' :: a -> DoubleList a -> DoubleList a
    unshift' x Nil = Node Nil x Nil
    unshift' x deq = Node Nil x deq

shift :: Deque a -> IO (Maybe a)
shift deque = do
  (value, dl) <- shift' <$> readIORef deque
  writeIORef deque dl
  pure value
  where
    shift' :: DoubleList a -> (Maybe a, DoubleList a)
    shift' Nil            = (Nothing, Nil)
    shift' (Node Nil x r) = (Just x, r)
    shift' (Node l x r)   = (\a -> Node a x r) <$> shift' l
