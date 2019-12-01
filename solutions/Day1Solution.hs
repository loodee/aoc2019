module Day1Solution where

solve :: FilePath -> IO ()
solve fp = print =<< sum . map (reqFuel . read) . lines <$> readFile fp

reqFuel :: Int -> Int
reqFuel n = let fuel = n `div` 3 - 2 in
    if fuel < 0
        then 0
        else fuel + reqFuel fuel
