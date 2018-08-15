module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA = traverse dna2RNA

dna2RNA :: Char -> Maybe Char
dna2RNA 'G' = Just 'C'
dna2RNA 'C' = Just 'G'
dna2RNA 'T' = Just 'A'
dna2RNA 'A' = Just 'U'
dna2RNA _   = Nothing
