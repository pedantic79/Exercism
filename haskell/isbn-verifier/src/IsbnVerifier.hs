module IsbnVerifier (isbn) where

import Data.Char (digitToInt, isDigit)
import Data.Maybe (maybe)

isbn :: String -> Bool
isbn = isValid . foldr isbnCalc (Just 0, 0) . filter (/='-')
    where
        isValid (Just v, 10) = v `mod` 11 == 0
        isValid _            = False

isbnCalc :: Char -> (Maybe Int, Int) -> (Maybe Int, Int)
isbnCalc c (acc, pos) = ((+) <$> acc <*> ((*next) <$> digit), next)
    where
        digit = convertChar pos c
        next  = pos + 1

convertChar :: Int -> Char -> Maybe Int
convertChar pos c
    | pos == 0 && c == 'X' = Just 10
    | isDigit c            = Just . digitToInt $ c
    | otherwise            = Nothing

