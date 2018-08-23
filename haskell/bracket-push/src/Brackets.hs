{-# LANGUAGE LambdaCase #-}
module Brackets (arePaired) where

import Data.Maybe (isJust)

arePaired :: String -> Bool
arePaired = isJust . foldr processPairs (Just [])

processPairs :: Char -> Maybe String -> Maybe String
processPairs c rest
    | h == Just c          = fmap tail rest
    | c `elem` "[{("       = Nothing
    | Just p <- getMatch c = (:) p <$> rest
    | otherwise            = rest
    where
        h = rest >>= safeHead

getMatch = \case
            ')' -> Just '('
            ']' -> Just '['
            '}' -> Just '{'
            _   -> Nothing

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
