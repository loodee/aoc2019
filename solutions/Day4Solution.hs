module Day4Solution where

import Data.List (group)
import Data.List.Split (splitOn)

solve4IO :: FilePath -> IO ()
solve4IO fp = print . solve4 . parse =<< readFile fp

parse :: String -> (Int, Int)
parse = (\xs -> (read . head $ xs, read . head . tail $ xs)) . splitOn "-"

solve4 :: (Int, Int) -> Int
solve4 = length
       . filter (\x -> ascends x && hasAdjacent x)
       . map show
       . (\(x, y) -> [x .. y])

hasAdjacent :: String -> Bool
hasAdjacent = any ((2 ==) . length) . group

ascends :: String -> Bool
ascends [x]    = True
ascends (x:xs) = x <= head xs && ascends xs
