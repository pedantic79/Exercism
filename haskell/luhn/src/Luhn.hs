module Luhn (isValid) where

import Data.Char (digitToInt, isDigit, isSpace)
import Data.Maybe (fromMaybe)

isValid :: String -> Bool
isValid = maybe False (check . foldr luhn (0, 0)) . traverse filtLuhn .
            filter (not . isSpace)

-- Convert Char to Int, returning Nothing if it's not valid
filtLuhn :: Char -> Maybe Int
filtLuhn =  fmap digitToInt . filtMaybe isDigit

-- Run the predicate on n, if true return Just n, else Nothing
filtMaybe :: (a -> Bool) -> a -> Maybe a
filtMaybe pred n
    | pred n    = Just n
    | otherwise = Nothing

check :: (Int, Int) -> Bool
check (nth, total)
    | nth < 2   = False
    | otherwise = total `mod` 10 == 0

luhn :: Int -> (Int, Int) -> (Int, Int)
luhn n (nth, total)
    | nth `mod` 2 == 0 = (nth + 1, total + n)
    | n > 4            = (nth + 1, total + 2 * n - 9)
    | otherwise        = (nth + 1, total + 2 * n)

