module Day2Solution where

import Data.Char (isNumber)
import Data.List (groupBy)
import Data.Map.Strict (Map, fromList, insert, lookup)
import Data.Maybe (catMaybes, isJust)
import Text.Read (readMaybe)

solve :: FilePath -> IO ()
solve fp = print . compute 0  . fromList . index 0 . parse =<< readFile fp

parse :: String -> [Int]
parse = catMaybes
      . filter isJust
      . map readMaybe
      . groupBy (\x y -> isNumber x == isNumber y)

index :: Int -> [Int] -> [(Int, Int)]
index _ [] = []
index n (x:xs) = (n, x) : index (n + 1) xs

compute :: Int -> Map Int Int -> [Int]
compute i m = case i of
    1 ->  + lookup

    where add = let res = fmap (+) (lookup (i + 1) m) $ (lookup (i + 2) m) in
                    insert 
