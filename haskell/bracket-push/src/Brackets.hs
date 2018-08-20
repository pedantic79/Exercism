{-# LANGUAGE LambdaCase #-}
module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired = null . foldl processPairs []

processPairs :: String -> Char -> String
processPairs rest c
    | h == Just c          = tail rest
    | Just p <- getMatch c = p:rest
    | c `elem` "])}"       = c:rest
    | otherwise            = rest
    where
        h = safeHead rest

getMatch :: Char -> Maybe Char
getMatch = \case
            '[' -> Just ']'
            '(' -> Just ')'
            '{' -> Just '}'
            _   -> Nothing

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
