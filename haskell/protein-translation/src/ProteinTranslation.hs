module ProteinTranslation(proteins) where

import Data.List.Split (chunksOf)

proteins :: String -> Maybe [String]
proteins = fmap (takeWhile (/="STOP")) . traverse proteinLookup . chunksOf 3

proteinLookup :: String -> Maybe String
proteinLookup p
    | p == "AUG"                            = Just "Methionine"
    | p `elem` ["UUU", "UUC"]               = Just "Phenylalanine"
    | p `elem` ["UUA", "UUG"]               = Just "Leucine"
    | p `elem` ["UCU", "UCC", "UCA", "UCG"] = Just "Serine"
    | p `elem` ["UAU", "UAC"]               = Just "Tyrosine"
    | p `elem` ["UGU", "UGC"]               = Just "Cysteine"
    | p == "UGG"                            = Just "Tryptophan"
    | p `elem` ["UAA", "UAG", "UGA"]        = Just "STOP"
    | otherwise                             = Nothing
