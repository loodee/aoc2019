{-# LANGUAGE TupleSections #-}

module Day11Part2 where

import Data.List (sortBy)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as M
  ( Map
  , empty
  , findWithDefault
  , fromList
  , insert
  , keys
  , toList
  , union
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

solve11p2 :: FilePath -> IO ()
solve11p2 fp
  = putStrLn
  . formatIdentifier
  . startRobot
  . parse
  =<< readFile fp

formatIdentifier :: TileGrid -> String
formatIdentifier tiles = unlines $ format grid
  where
    (xs, ys) = unzip $ M.keys tiles
    xSpan = [minimum xs .. maximum xs]
    ySpan = [minimum ys .. maximum ys]

    grid :: TileGrid
    grid = M.union tiles
         . M.fromList
         . map (, Black)
         . concatMap (\x -> map (x,) ySpan)
         $ xSpan

    format :: TileGrid -> [String]
    format = map (map (\(_, c) -> if c == Black then '.' else '#'))
           . chunksOf (length xSpan)
           . sortBy (\((_, y1), _) ((_, y2), _) -> compare y1 y2)
           . M.toList

-- Returns a list of tiles that were painted
startRobot :: Tape -> TileGrid
startRobot tape = runRobot (runWithArgs tape [1]) M.empty ((0,0), U)

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
