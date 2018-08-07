module Hamming (distance) where

zipMaybe :: [a] -> [b] -> [Maybe (a, b)]
zipMaybe (a:as) (b:bs) = Just (a, b) : zipMaybe as bs
zipMaybe []     (_:_)  = Nothing : []
zipMaybe (_:_)  []     = Nothing : []
zipMaybe []     []     = []

distance :: String -> String -> Maybe Int
distance xs ys = fmap (sum . fmap eq) . sequence $ zipMaybe xs ys
    where
        eq (x, y) = if (x == y) then 0 else 1
