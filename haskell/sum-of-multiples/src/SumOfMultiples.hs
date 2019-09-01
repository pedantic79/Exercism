module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples = (sum .) . multiples

multiples :: [Integer] -> Integer -> [Integer]
multiples fact limit = [x| x <- [1..limit-1], any (multipleOf x) fact']
  where
    fact' = filter (/=0) fact
    multipleOf = ((0 ==) .) . mod
