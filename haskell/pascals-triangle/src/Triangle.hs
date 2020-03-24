module Triangle (rows) where

rows :: Int -> [[Integer]]
rows = flip take (iterate newRow [1])

newRow :: [Integer] -> [Integer]
newRow [] = [1]
newRow xs = mconcat [[1], zipWith (+) xs (tail xs), [1]]
