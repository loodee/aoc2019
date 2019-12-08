module Day6Part1 where

data Planet = Planet String [Planet]
    deriving (Eq, Show)

type Orbit = (String, String)

solve6p1 :: FilePath -> IO ()
solve6p1 = undefined