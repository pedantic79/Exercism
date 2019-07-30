module ResistorColors (Color(..), value) where
import Data.List (foldl')

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Eq, Show, Read, Enum)

value :: [Color] -> Int
value  = foldl' (\a x -> a * 10 + fromEnum x) 0
