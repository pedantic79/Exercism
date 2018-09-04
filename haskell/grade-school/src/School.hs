module School
  ( School
  , add
  , empty
  , grade
  , sorted
  ) where

import           Control.Lens       ((%~))
import           Control.Lens.Each  (each)
import           Control.Lens.Tuple (_2)
import           Data.List          (sort)

import qualified Data.MultiMap      as M

type School = M.MultiMap Int String

add :: Int -> String -> School -> School
add = M.insert

empty :: School
empty = M.empty

grade :: Int -> School -> [String]
grade = (sort .) . M.lookup

sorted :: School -> [(Int, [String])]
sorted = (each . _2 %~ sort) . M.assocs

numberOfStudents :: School -> Int
numberOfStudents = M.size
