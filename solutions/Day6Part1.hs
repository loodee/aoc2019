module Day6Part1 where

import Data.List (partition)
import Data.List.Split (splitOn)

import Data.Tree

type Orbit = (String, String)

solve6p1 :: FilePath -> IO ()
solve6p1 fp = print . count 0 . (`build` com) . parse =<< readFile fp

com :: Tree String
com = Node "COM" []

parse :: String -> [Orbit]
parse = map ((\(x:y:_) -> (x, y)) . splitOn ")") . lines

build :: [Orbit] -> Tree String -> Tree String
build rem (Node name xs) =
  let (orbits, rest) = partition ((name ==) . fst) rem
  in Node name $ map (build rest . (\x -> Node x []) . snd) orbits

count :: Int -> Tree String -> Int
count d (Node _ []) = d
count d (Node n xs) = d + sum (map (count $ d + 1) xs)
