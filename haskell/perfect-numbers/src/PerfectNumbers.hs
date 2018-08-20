module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
    | n <  1 = Nothing
    | s == n = Just Perfect
    | s >  n = Just Abundant
    | s <  n = Just Deficient
    where
        s = sum . filter (/=n) . factors $ n

factors :: Int -> [Int]
factors n = do
    i     <- [1..r]
    (d,0) <- [divMod n i]
    j     <- i:[ d | d/=i ]
    return j
    where
        r = floor . sqrt . fromIntegral $ n

