module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram xs = all (`elem` lc) ['a'..'z']
    where
        lc = map toLower xs
