module Day1Solution where

solve1 :: FilePath -> IO ()
solve1 s = print =<< sum . map (reqFuel . read) . lines <$> readFile s
    where
        reqFuel :: Int -> Int
        reqFuel n = n `div` 3 - 2