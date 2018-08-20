module ETL (transform) where

import Data.Char (toLower)
import Data.Map.Strict (Map, toList, fromList)

transform :: Map a String -> Map Char a
transform = fromList . concatMap trans . toList

trans :: (a, String) -> [(Char, a)]
trans = uncurry (fmap . flip ((,) . toLower))
