module Series (Error(..), largestProduct) where

import Data.Char (ord)
import Data.List (tails)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits = do
  d <- string2Digits digits
  w <- window size d
  return . maximum . map product $ w

string2Digits :: String -> Either Error [Integer]
string2Digits = mapM digitToInt

window :: Int -> [a] -> Either Error [[a]]
window size digits
  | size < 0 || size > length digits = Left InvalidSpan
  | otherwise = Right . filter (\x -> length x == size) . map (take size) . tails $ digits

digitToInt :: Char -> Either Error Integer
digitToInt c
  | dec `elem` [0..9] = Right $ fromIntegral dec
  | otherwise         = Left $ InvalidDigit c
  where
    dec = ord c - ord '0'
