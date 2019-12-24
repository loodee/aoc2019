module Day13Part1 where

import Data.List.Split (splitOn)

import IntcodeComputer (readRunWithArgs, stOutputs)

type Pos = (Int, Int)

data Tiles = Tiles
    { tileEmpty  :: [Pos]
    , tileWall   :: [Pos]
    , tileBlock  :: [Pos]
    , tilePaddle :: [Pos]
    , tileBall   :: [Pos]
    }

solve13p1 :: FilePath -> IO ()
solve13p1 fp = do
  st <- readRunWithArgs fp []
  let tiles = parseOutput (reverse $ stOutputs st)
  print $ length (tileBlock tiles)

parseOutput :: [Int] -> Tiles
parseOutput = parse emptyTiles
  where
    emptyTiles = Tiles [] [] [] [] []

    parse :: Tiles -> [Int] -> Tiles
    parse ts [] = ts
    parse ts (x : y : id : xs) = parse (f ts (x, y)) xs
      where
        f = case id of
          0 -> addEmpty
          1 -> addWall
          2 -> addBlock
          3 -> addPaddle
          4 -> addBall

        addEmpty  ts pos = ts { tileEmpty  = pos : tileEmpty ts }
        addWall   ts pos = ts { tileWall   = pos : tileWall ts }
        addBlock  ts pos = ts { tileBlock  = pos : tileBlock ts }
        addPaddle ts pos = ts { tilePaddle = pos : tilePaddle ts }
        addBall   ts pos = ts { tileBall   = pos : tileBall ts }
