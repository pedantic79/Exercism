module School (School, add, empty, grade, sorted) where

import Control.Lens ((%~))
import Control.Lens.At (at)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Monoid (getSum, Sum(..))

import qualified Data.IntMap as M

type School = M.IntMap [String]

add :: Int -> String -> School -> School
add g name = at g %~ Just . sort . (name:) . fromMaybe []
-- add g name = M.insertWith ((sort .) . (++)) g [name]

empty :: School
empty = M.empty

grade :: Int -> School -> [String]
grade = M.findWithDefault []

sorted :: School -> [(Int, [String])]
sorted = M.toAscList

numberOfStudents :: School -> Int
numberOfStudents = getSum . foldMap (Sum . length)
