module Day3Solution where

import Data.List (intersect, sortOn)
import Data.List.Split (splitOn)

type Path = [(Int, Int)]

solve3p1IO :: FilePath -> IO ()
solve3p1IO fp = print . solve3p1 . lines =<< readFile fp

solve3p1 :: [String] -> Int
solve3p1 = minimum
         . map manhattan
         . (\(x:xs) -> x `intersect` head xs)
         . map (path (0, 0) . toDirs)

manhattan :: (Int, Int) -> Int
manhattan (x, y) = abs x + abs y

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
                    path (head line) xs ++ line
    where
        zDec n = if n >= 0 then n - 1 else n + 1
        connect t (0, 0)   = []
        connect (a1, a2) d = case d of
            (m, 0) -> (a1 + m, a2) : connect (a1, a2) (zDec m, 0)
            (0, n) -> (a1, a2 + n) : connect (a1, a2) (0, zDec n)

solve3p2IO :: FilePath -> IO ()
solve3p2IO fp = do
    input <- readFile fp

    let paths = map (reverse . path (0, 0) . toDirs) . lines $ input
    let w1 = head paths
    let w2 = paths !! 1

    print $ fewestSteps w1 w2 $ intersections (prepPath w1) (prepPath w2)

    where
        prepPath = sortOn fst . map addDist
        addDist t = (manhattan t, t)

fewestSteps :: [(Int, Int)]
            -> [(Int, Int)]
            -> [(Int, (Int, Int))]
            -> Int
fewestSteps xs ys is = minimum $ map (totalLength xs ys) is
    where
        totalLength xs ys (d, c) = steps xs c + steps ys c
        steps (x:xs) c = if x == c then 1 else 1 + steps xs c

intersections :: [(Int, (Int, Int))]
              -> [(Int, (Int, Int))]
              -> [(Int, (Int, Int))]
intersections _ [] = []
intersections [] _ = []
intersections xs@((dist, t):_) ys
    = wl `intersect` wr ++ intersections restL restR
    where
        restL = drop (length wl) xs
        restR = drop (length wr) ys
        sameDists = takeWhile (\t -> fst t == dist)
        wl = sameDists xs
        wr = sameDists ys
