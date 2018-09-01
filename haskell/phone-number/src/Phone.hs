module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number = (valid =<<) . cleanup

valid :: String -> Maybe String
valid input
    | length input /= 10       = Nothing
    | head input `elem` "01"   = Nothing
    | (input !! 3) `elem` "01" = Nothing
    | otherwise                = Just input

cleanup :: String -> Maybe String
cleanup input
    | len == 10            = Just digits
    | len == 11 && headOne = Just $ tail digits
    | otherwise            = Nothing
    where
        digits  = filter isDigit input
        len     = length digits
        headOne = head digits == '1'
