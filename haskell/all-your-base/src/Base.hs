module Base (Error(..), rebase) where

import Data.List (foldl')

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase digits
  | inputBase  < 2 = Left InvalidInputBase
  | outputBase < 2 = Left InvalidOutputBase
  | otherwise      = toDigits outputBase <$> toBase10 inputBase digits

toDigits :: Integral a => a -> a -> [a]
toDigits base = go []
  where
    go acc n
      | n > 0     = go (n `mod` base:acc) (n `div` base)
      | otherwise = acc

toBase10 :: Integral a => a -> [a] -> Either (Error a) a
toBase10 base = foldl' go (Right 0)
  where
    go acc digit
      | digit < 0 || digit >= base = Left $ InvalidDigit digit
      | otherwise                  = (+ digit) . (* base) <$> acc
