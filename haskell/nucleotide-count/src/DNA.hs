module DNA (nucleotideCounts, Nucleotide(..)) where

import qualified Data.Map as M

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (M.Map Nucleotide Int)
nucleotideCounts = fmap foldNtides . traverse isValid
    where
        foldNtides = foldr (\ntide m -> M.insertWith (+) ntide 1 m) zero
        zero = M.fromList[(A, 0), (T, 0), (G, 0), (C, 0)]

isValid :: Char -> Either String Nucleotide
isValid nucleotide =
    case nucleotide of
        'A' -> Right A
        'C' -> Right C
        'G' -> Right G
        'T' -> Right T
        _   -> Left $ "Invalid nucleotide: " ++ show nucleotide
