module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import qualified Data.Map.Strict as M
import Data.List (foldl')

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

type Garden = M.Map String [Plant]

garden :: [String] -> String -> Garden
garden students plants = fst3 $ foldl' addPlants (M.empty, head1 row, head2 row) students
        where
            fst3 (a, _, _) = a
            head1 = head
            head2 = head . tail
            row = map readGarden <$> lines plants
            addPlants (m, r1, r2) name
                | null r1   = (m, r1, r2)
                | otherwise = (M.insert name [head1 r1, head2 r1, head1 r2, head2 r2] m, drop 2 r1, drop 2 r2)

readGarden :: Char -> Plant
readGarden 'C' = Clover
readGarden 'G' = Grass
readGarden 'R' = Radishes
readGarden 'V' = Violets
readGarden g = error $ "Unknown char " ++ show g


lookupPlants :: String -> Garden -> [Plant]
lookupPlants = flip (M.!)
