module Day3Solution where

import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Path = [(Int, Int)]

solve3p1IO :: FilePath -> IO ()
solve3p1IO fp = print . solve3p1 . lines =<< readFile fp

solve3p1 :: [String] -> Int
solve3p1 = minimum
         . map manhattan
         . intersections
         . map (path (0, 0) . toDirs)

manhattan :: (Int, Int) -> Int
manhattan (x, y) = abs x + abs y

intersections :: [[(Int, Int)]] -> [(Int, Int)]
intersections []     = []
intersections (x:xs) = dupes x xs ++ intersections xs
    where
        dupes xs xss = concatMap (\e -> mapMaybe (maybeIn e) xss) xs
        maybeIn e xs = if e `elem` xs then Just e else Nothing

toDirs :: String -> [(Int, Int)]
toDirs = map toDir . splitOn ","
    where toDir (dir : nums) = let n = read nums in
            case dir of
                'L' -> (-n, 0)
                'D' -> (0, -n)
                'U' -> (0, n)
                'R' -> (n, 0)

path :: (Int, Int) -> [(Int, Int)] -> Path
path base []     = []
path base (x:xs) = let line = connect base x in
                    line ++ path (head line) xs
    where
        zDec n = if n >= 0 then n - 1 else n + 1
        connect t (0, 0)   = []
        connect (a1, a2) d = case d of
            (m, 0) -> (a1 + m, a2) : connect (a1, a2) (zDec m, 0)
            (0, n) -> (a1, a2 + n) : connect (a1, a2) (0, zDec n)
