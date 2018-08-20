module Isogram (isIsogram) where

import Data.List (group, sort)
import Data.Char (isAlpha, toLower)

isIsogram :: String -> Bool
isIsogram = all (==1) . map length . group . sort . map toLower . filter isAlpha
