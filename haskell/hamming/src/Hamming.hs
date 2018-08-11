module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance (x:xs) (y:ys) = (+) <$> bool2int (x /= y) <*> distance xs ys
distance []     []     = Just 0
distance _       _      = Nothing

bool2int :: Bool -> Maybe Int
bool2int True = Just 1
bool2int False = Just 0
