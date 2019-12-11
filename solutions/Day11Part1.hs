module Day11Part1 where

import qualified Data.Map.Strict as M
  ( Map
  , empty
  , findWithDefault
  , insert
  )

import IntcodeComputer
  ( St
  , stHalted
  , stOutputs
  , Tape
  , parse
  , resumeRun
  , runWithArgs
  )

data Direction = L | D | U | R
  deriving (Eq, Show)

data Color = Black | White
  deriving (Eq, Show)

type Point = (Int, Int)

type Position = (Point, Direction)

type TileGrid = M.Map Point Color

solve11p1 :: FilePath -> IO ()
solve11p1 fp
  = print
  . length
  . startRobot
  . parse
  =<< readFile fp

-- Returns a list of tiles that were painted
startRobot :: Tape -> TileGrid
startRobot tape = runRobot (runWithArgs tape [0]) M.empty ((0,0), U)

runRobot :: St -> TileGrid -> Position -> TileGrid
runRobot st tiles (point@(x, y), d)
  | stHalted st = tiles
  | otherwise   = runRobot (resumeRun st [nextColorCode]) paint move

  where
    (rCode : cCode : _) = stOutputs st

    paint :: TileGrid
    paint = M.insert point color tiles
      where color = if cCode == 0 then Black else White

    rotate :: Direction
    rotate = case rCode of
      0 -> case d of
        L -> D
        D -> R
        R -> U
        U -> L
      1 -> case d of
        L -> U
        U -> R
        R -> D
        D -> L

    move :: Position
    move = (forward, rotate)
      where
        forward = case rotate of
          L -> (x - 1, y)
          D -> (x, y + 1)
          U -> (x, y - 1)
          R -> (x + 1, y)

    nextColorCode :: Int
    nextColorCode = let toColor c = if c == Black then 0 else 1 in
      toColor $ M.findWithDefault Black (fst move) paint

