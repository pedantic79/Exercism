module Atbash (decode, encode) where

import Data.Char (toLower, isAlphaNum, isNumber, isSpace)
import Data.List.Split (chunksOf)

import qualified Data.Map.Strict as M

decode :: String -> String
decode = map cipher . filter (not . isSpace)

encode :: String -> String
encode = unwords . chunksOf 5 . map (cipher . toLower) . filter isAlphaNum

cipher :: Char -> Char
cipher c
    | isNumber c = c
    | otherwise  = M.fromList (zip ['a'..'z'] ['z','y'..'a']) M.! c
