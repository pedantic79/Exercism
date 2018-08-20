module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples = (sum .) . multiples

multiples :: [Integer] -> Integer -> [Integer]
multiples fact limit = [x| x <- [1..limit-1], any ((0 ==) . (x `mod`)) fact]
