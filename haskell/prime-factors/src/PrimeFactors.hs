module PrimeFactors
  ( primeFactors
  ) where

import           Control.Monad (join)

primeFactors :: Integer -> [Integer]
primeFactors = factors prime
  where
    factors [] _       = undefined
    factors _ 1        = []
    factors pps@(p:ps) m
      | m `mod` p == 0 = p:factors pps (m `div` p)
      | otherwise      = factors ps m

prime :: [Integer]
prime = 2:3:[ x | k <- [1 ..]
                , r <- [-1, 1]
                , let x = 6 * k + r
                , all ((0 /=) . (x `mod`)) . takeWhile ((<= x) . join (*)) $ prime
                ]
