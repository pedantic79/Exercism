module Hamming (distance) where

zipMaybe :: [a] -> [b] -> [Maybe (a, b)]
zipMaybe (a:as) (b:bs) = Just (a, b) : zipMaybe as bs
zipMaybe []     (_:_)  = [Nothing]
zipMaybe (_:_)  []     = [Nothing]
zipMaybe []     []     = []

distance :: String -> String -> Maybe Int
distance xs ys = sum . fmap ne <$> sequence (zipMaybe xs ys)
    where
        ne = bool2int . uncurry (/=)

bool2int :: Bool -> Int
bool2int True = 1
bool2int False = 0
