module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = traverse dna2RNA

dna2RNA :: Char -> Either Char Char
dna2RNA 'G' = Right 'C'
dna2RNA 'C' = Right 'G'
dna2RNA 'T' = Right 'A'
dna2RNA 'A' = Right 'U'
dna2RNA x   = Left  x
