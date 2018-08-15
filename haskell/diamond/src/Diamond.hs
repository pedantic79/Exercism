module Diamond (diamond) where
import Data.Char (chr,ord,toUpper)

diamond :: Char -> Maybe [String]
diamond ch = Just $ top ++ bottom
    where
        sz     = subtract 64 . ord . toUpper $ ch
        top    = zipWith3 line (0:[1, 3..]) [sz-1, sz-2..0] ['A'..'Z']
        bottom = tail . reverse $ top

line :: Int -> Int -> Char -> String
line middle outer l
    | middle == 0 = spaces outer ++ l:spaces outer
    | otherwise   = spaces outer ++ l:spaces middle ++ l:spaces outer
    where
        spaces = flip replicate ' '
