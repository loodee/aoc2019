module Day4Solution where

import Data.List.Split (splitOn)

solve4p1IO :: FilePath -> IO ()
solve4p1IO fp = print . solve4p1 . parse =<< readFile fp

parse :: String -> (Int, Int)
parse = (\xs -> (read . head $ xs, read . head . tail $ xs)) . splitOn "-"

solve4p1 :: (Int, Int) -> Int
solve4p1 (m, n)
    = length
    . filter (\x -> ascends x && hasAdjacent x)
    . map show
    $ [m .. n]

hasAdjacent :: String -> Bool
hasAdjacent [x]    = False
hasAdjacent (x:xs) = x == head xs || hasAdjacent xs

ascends :: String -> Bool
ascends [x]    = True
ascends (x:xs) = x <= head xs && ascends xs
