module RunLength (decode, encode) where

import Data.Char
import Data.List

decode :: String -> String
decode = reverse . fst . foldl' split ([], [])

split (acc, count) ch
    | isDigit ch = (acc, ch:count)
    | null count = (ch:acc, [])
    | otherwise  = (replicate (read . reverse $ count) ch ++ acc, [])


encode :: String -> String
encode = concatMap encodeLetter . group

encodeLetter str
    | l == 1    = str
    | otherwise = show l ++ [head str]
    where
        l = length str
