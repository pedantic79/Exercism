module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong number = number == s
  where
    d = digits number
    l = length d
    s = sum . map (^l) $ d

digits :: Integral a => a -> [a]
digits = go []
  where
    go acc n
      | n > 0     = go (n `mod` 10:acc) (n `div` 10)
      | otherwise = acc
