module DNA (nucleotideCounts) where

import qualified Data.Map as M

nucleotideCounts :: String -> Either String (M.Map Char Int)
nucleotideCounts xs = foldNtides <$> xs'
    where
        foldNtides = foldl (\m ch -> M.insertWith (+) ch 1 m) zero
        zero = M.fromList([('A', 0), ('T', 0), ('G', 0), ('C', 0)])
        xs' = mapM isValid xs

isValid :: Char -> Either String Char
isValid x
    | elem x "ACTG" = Right x
    | otherwise     = Left $ "Invalid nucleotide: " ++ show x
