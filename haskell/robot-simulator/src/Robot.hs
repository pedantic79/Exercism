{-# LANGUAGE TemplateHaskell #-}

module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

import Control.Monad (liftM2)
import Control.Lens (makeLenses, (&), (^.), (%~), view)
import Data.List (foldl')

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show, Enum,  Bounded)

data Robot = Robot { _dir :: Bearing, _x :: Integer, _y :: Integer}

makeLenses ''Robot

bearing :: Robot -> Bearing
bearing = view dir

coordinates :: Robot -> (Integer, Integer)
coordinates = liftM2 (,) (^. x) (^. y)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction (xCoord, yCoord) = Robot direction xCoord yCoord

simulate :: Robot -> String -> Robot
simulate = foldl' operate

operate :: Robot -> Char -> Robot
operate robot 'R' = robot & dir %~ turnRight
operate robot 'L' = robot & dir %~ turnLeft
operate robot 'A' = case bearing robot of
                        North -> robot & y %~ succ
                        South -> robot & y %~ pred
                        East  -> robot & x %~ succ
                        West  -> robot & x %~ pred

turnLeft :: Bearing -> Bearing
turnLeft d
    | d == minBound = maxBound
    | otherwise     = pred d

turnRight :: Bearing -> Bearing
turnRight d
    | d == maxBound = minBound
    | otherwise     = succ d
