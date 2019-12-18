module Day16Part1 where

import Data.Char (digitToInt, intToDigit)

solve16p1IO :: FilePath -> IO ()
solve16p1IO fp = readFile fp >>= (putStrLn . solve16p1)

solve16p1 :: String -> String
solve16p1
  = map intToDigit
  . take 8
  . (\x -> iterate fft x !! 100)
  . map digitToInt

fft :: [Int] -> [Int]
fft xs = take (length xs) $ map (calcDigit xs) [1 .. ]
  where
    pat n = tail . cycle . concatMap (replicate n) $ [0, 1, 0, -1]

    calcDigit :: [Int] -> Int -> Int
    calcDigit input n = (`mod` 10) . abs . sum . zipWith (*) input $ pat n
