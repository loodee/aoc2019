module Day8Part1 where

import Data.Char (intToDigit)
import Data.List (minimumBy)

width  = 25
height = 6

solve8p1 :: FilePath -> IO ()
solve8p1 fp
  = print
  . (\xs -> count '1' xs * count '2' xs)
  . fewest '0'
  . parse
  =<< readFile fp

parse :: String -> [String]
parse [] = []
parse xs = take (width * height) xs : parse (drop (width * height) xs)

count :: Eq a => a -> [a] -> Int
count n = length . filter (== n)

fewest :: Eq a => a -> [[a]] -> [a]
fewest c = minimumBy (\xs ys -> count c xs `compare` count c ys)

