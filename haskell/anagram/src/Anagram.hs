module Anagram
  ( anagramsFor
  ) where

import           Data.Char (toLower)
import           Data.List ((\\))

lowerCase :: String -> String
lowerCase = map toLower

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter f
  where
    f x =
      let x' = lowerCase x
       in ((x' /= lc) && null (x' \\ lc) && null (lc \\ x'))
    lc = lowerCase xs
