{-# LANGUAGE TupleSections #-}

module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

scrabbleScore :: M.Map Char Integer
scrabbleScore = createMap [ ("aeioulnrst", 1)
                          , ("dg", 2)
                          , ("bcmp", 3)
                          , ("fhvwy", 4)
                          , ("k", 5)
                          , ("jx", 8)
                          , ("qz", 10)
                          ]

createMap :: [(String, Integer)] -> M.Map Char Integer
createMap = foldr splitEntry M.empty
    where
        splitEntry (letters, score) acc =
            acc `M.union` M.fromList (fmap (, score) letters)

scoreLetter :: Char -> Integer
scoreLetter = fromMaybe 0 . flip M.lookup scrabbleScore . toLower

scoreWord :: String -> Integer
scoreWord = sum . fmap scoreLetter
