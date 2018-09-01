module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard = keep . (not .)

keep :: (a -> Bool) -> [a] -> [a]
keep p = foldr (\v acc -> if p v then v:acc else acc) []
