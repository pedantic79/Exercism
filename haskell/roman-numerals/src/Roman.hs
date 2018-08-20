module Roman (numerals) where

import Control.Monad (replicateM)
import qualified Data.Map.Strict as M

numerals :: Integer -> Maybe String
numerals n = fst $ foldr getNumerals (Just [], n) (M.keys romanNumerals)

getNumerals :: Integer -> (Maybe String, Integer) -> (Maybe String, Integer)
getNumerals divisor (acc, num)
    | d > 0     = getNumerals divisor ((++) <$> acc <*> repeat, r)
    | otherwise = (acc, num)
    where
        (d, r)  = num `divMod` divisor
        numeral = M.lookup divisor romanNumerals
        repeat  = fmap concat . replicateM (fromIntegral d) $ numeral

romanNumerals :: M.Map Integer String
romanNumerals = M.fromList  [ (1000, "M")
                            , (900, "CM")
                            , (500,  "D")
                            , (400, "CD")
                            , (100,  "C")
                            , (90,  "XC")
                            , (50,   "L")
                            , (40,  "XL")
                            , (10,   "X")
                            , (9,   "IX")
                            , (5,    "V")
                            , (4,   "IV")
                            , (1,    "I")
                            ]
