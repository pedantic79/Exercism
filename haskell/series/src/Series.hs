module Series (slices) where

import Data.Char (digitToInt)
import Data.List (tails)

slices :: Int -> String -> [[Int]]
slices n = filter (\x -> length x == n) . map (take n) . tails . map (digitToInt)
