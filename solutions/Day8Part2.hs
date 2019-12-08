module Day8Part2 where

import Data.List.Split (chunksOf)

width  = 25
height = 6

solve8p2 :: FilePath -> IO ()
solve8p2 fp = prettyPrint . render . parse =<< readFile fp

parse :: String -> [String]
parse [] = []
parse xs = take (width * height) xs : parse (drop (width * height) xs)

render :: [String] -> String
render = foldl1 fuse
  where
    fuse xs ys = map (\(a, b) -> if a /= '2' then a else b) $ zip xs ys

prettyPrint :: String -> IO ()
prettyPrint = mapM_ putStrLn
            . chunksOf width
            . map (\x -> if x == '0' then ' ' else '#')
