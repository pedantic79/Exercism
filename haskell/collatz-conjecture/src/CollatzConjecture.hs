module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz = collatz' 0
    where
        collatz' depth n
            | n < 1     = Nothing
            | n == 1    = Just depth
            | even n    = collatz' (depth + 1) (n `div` 2)
            | otherwise = collatz' (depth + 1) (3*n+1)
