module Triangle (TriangleType(..), triangleType) where

import Data.List (sort)

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c = case numEq a b c of
                        Just 0  -> Scalene
                        Just 1  -> Isosceles
                        Just 3  -> Equilateral
                        Nothing -> Illegal

numEq :: (Num a, Ord a) => a -> a -> a -> Maybe Int
numEq a b c
    | s1 + s2 <= s3 = Nothing
    | otherwise     = Just . foldr count 0 $ zip [a,b,c] [b,c,a]
    where
        [s1, s2, s3]   = sort [a,b,c]
        count (x, y) a = if x == y then a + 1 else a

