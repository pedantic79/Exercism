module Raindrops (convert) where

convert :: Int -> String
convert n
    | combined == "" = show n
    | otherwise      = combined
    where
        combined = concatMap (divisibleBy n) msg
        msg      = [(3, "Pling"), (5, "Plang"), (7, "Plong")]


divisibleBy :: Int -> (Int, String) -> String
divisibleBy n (d, m)
    | n `mod` d == 0 = m
    | otherwise      = ""
