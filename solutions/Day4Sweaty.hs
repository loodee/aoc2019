module Day4Sweaty where

import Data.List (group, sort)
import Data.List.Split (splitOn)

solve4sweaty :: FilePath -> IO ()
solve4sweaty fp
    = print
    . length
    . filter (\xs -> sort xs == xs && (any ((2 ==) . length) . group) xs)
    . map show
    . (\(x, y) -> [x .. y] :: [Int])
    . (\xs -> (read . head $ xs, read . head . tail $ xs))
    . splitOn "-"
    =<< readFile fp
