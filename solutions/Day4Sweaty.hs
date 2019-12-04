module Day4Sweaty where

import Data.List (group)
import Data.List.Split (splitOn)

solve4sweaty :: FilePath -> IO ()
solve4sweaty fp
    = print
    . length
    . filter (\x -> ascends x && hasAdjacent x)
    . map show
    . (\(x, y) -> [x .. y] :: [Int])
    . (\xs -> (read . head $ xs, read . head . tail $ xs))
    . splitOn "-"
    =<< readFile fp
    where hasAdjacent = any ((2 ==) . length) . group
          ascends [x]    = True
          ascends (x:xs) = x <= head xs && ascends xs
