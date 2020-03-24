module Acronym (abbreviate) where

import Data.Bool (bool)
import Data.Char (isAlpha, isSpace, isUpper, toUpper)
import Data.Foldable (foldMap)
import Data.List.Split (splitOneOf)
import Data.Maybe (listToMaybe)
import Data.Monoid (Any(..), getAny)


abbreviate :: String -> String
abbreviate = concatMap getImportant . splitOneOf " -" . filter validLetter

validLetter :: Char -> Bool
validLetter = getAny . foldMap (Any .) [isAlpha, isSpace, (== '-')]

capitalize :: String -> String
capitalize (x:xs) = toUpper x:xs
capitalize [] = []

getFirst :: String -> String
getFirst = maybe [] (:[]) . listToMaybe

getImportant :: String -> String
getImportant = bool <$> (filter isUpper . capitalize ) <*> getFirst <*> (all isUpper)
