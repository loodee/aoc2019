module Day2Solution where

import Data.Char (isNumber)
import Data.List (groupBy)
import Data.Map.Strict as M (Map, fromList, elems, insert, lookup)
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Text.Read (readMaybe)

solve2 :: FilePath -> IO ()
solve2 fp = print . run . buildMap =<< readFile fp

buildMap :: String -> Map Int Int
buildMap = M.fromList . index 0 . parse
    where
        parse :: String -> [Int]
        parse = catMaybes
              . filter isJust
              . map readMaybe
              . groupBy (\x y -> isNumber x == isNumber y)

        index :: Int -> [Int] -> [(Int, Int)]
        index _ [] = []
        index n (x:xs) = (n, x) : index (n + 1) xs

run :: Map Int Int -> [Int]
run = elems . compute 0

compute :: Int -> Map Int Int -> Map Int Int
compute i m
    | opcode == 99 = m
    | otherwise = let (n1, n2, loc) = comp i m in
        compute (i + 4) $ M.insert loc (n1 `op` n2) m
    where
        opcode = fromJust $ M.lookup i m
        op = case opcode of
            1 -> (+)
            2 -> (*)

comp :: Int -> Map Int Int -> (Int, Int, Int)
comp i m = ( fromJust (M.lookup n1 m)
           , fromJust (M.lookup n2 m)
           , loc
           )
    where [n1, n2, loc] = mapMaybe (\x -> M.lookup (i + x) m) [1 .. 3]


