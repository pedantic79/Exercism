module Accumulate (accumulate) where

accumulate :: (a -> b) -> [a] -> [b]
accumulate f = foldr ((:) . f) []

{-
stream-fusion makes foldr faster than manually iterating through recursion

accumulate f (x:xs) = f x : accumulate f xs
accumulate f []     = []
-}
