module SecretHandshake (handshake) where

import Data.Bits

handshake :: Int -> [String]
handshake = process 0 []

process :: Int -> [String] -> Int -> [String]
process num acc n
    | num == 4  = if bitset then acc else reverse acc
    | bitset    = process (num + 1) ((signal !! num):acc) n
    | otherwise = process (num + 1) acc n
    where
        bitset = shiftR n num .&. 1 == 1

signal :: [String]
signal = ["wink", "double blink", "close your eyes", "jump"]
