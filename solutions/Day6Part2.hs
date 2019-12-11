module Day6Part2 where

import Data.List (partition)
import Data.List.Split (splitOn)

import Data.Tree

type Orbit = (String, String)

solve6p2 :: FilePath -> IO ()
solve6p2 fp
  = print
  . (\tree -> transfers (search "YOU" tree) (search "SAN" tree))
  . flip build com
  . parse
  =<< readFile fp

com :: Tree String
com = Node "COM" []

parse :: String -> [Orbit]
parse = map ((\(x:y:_) -> (x, y)) . splitOn ")") . lines

build :: [Orbit] -> Tree String -> Tree String
build rem (Node name xs) =
  let (orbits, rest) = partition ((name ==) . fst) rem
  in Node name $ map (build rest . (\x -> Node x []) . snd) orbits

search :: String -> Tree String -> [String]
search s (Node name xs)
  | s == name = [name]
  | null branch = []
  | otherwise = name : branch
  where
    branch = concatMap (search s) xs

transfers :: [String] -> [String] -> Int
transfers (x:xs) (y:ys) =
  if x == y then transfers xs ys else length ys + length xs
