module Acronym (abbreviate) where

import Data.Char (isAlpha, isLower, isSpace, toUpper)
import Data.List.Split (splitOneOf, splitWhen)

abbreviate :: String -> String
abbreviate = concatMap (fmap head . filter (/="") . splitWhen isLower . capitalize) . splitOneOf " -" . filter validLetter

capitalize :: String -> String
capitalize (x:xs) = toUpper x:xs

validLetter :: Char -> Bool
validLetter c = isAlpha c || isSpace c || c == '-'
