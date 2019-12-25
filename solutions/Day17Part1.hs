module Day17Part1 where

import Data.Char (chr)
import Data.Maybe (fromMaybe, listToMaybe)

import IntcodeComputer
  ( parse
  , readRunWithArgs
  , runWithArgs
  , stOutputs
  )

type ASCII = String
type Coordinate = (Int, Int)

solve17p1IO :: FilePath -> IO ()
solve17p1IO fp = print . solve17p1 =<< readFile fp

solve17p1 :: String -> Int
solve17p1
  = sum
  . alignmentParams
  . intersections
  . computeAscii

computeAscii :: String -> ASCII
computeAscii
  = toAscii
  . reverse . stOutputs
  . (\tape -> runWithArgs tape [])
  . parse
  where
    toAscii :: [Int] -> ASCII
    toAscii = map chr

intersections :: ASCII -> [Coordinate]
intersections s = scan [] "" s 0 0
  where
    width = length . head . lines $ s

    scan :: [Coordinate] -> ASCII -> ASCII -> Int -> Int -> [Coordinate]
    scan coords _ [] _ _ = coords
    scan coords bs (c:cs) x y = case c of
      '\n' -> nextLine
      '.'  -> continue
      _    -> if isIntersection
                then scan ((x, y) : coords) (c:bs) cs (x + 1) y
                else continue

      where
        continue = scan coords (c:bs) cs (x + 1) y
        nextLine = scan coords (c:bs) cs 0 (y + 1)

        isIntersection
          = fromMaybe '.' (listToMaybe bs) == '#'
            && fromMaybe '.' (listToMaybe $ drop width bs) == '#'
            && fromMaybe '.' (listToMaybe cs) == '#'
            && fromMaybe '.' (listToMaybe $ drop width cs) == '#'

alignmentParams :: [Coordinate] -> [Int]
alignmentParams = map $ uncurry (*)
