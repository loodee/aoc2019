module Day22Part1 where

import qualified Data.IntMap.Strict as IM
  ( IntMap
  , elems
  , empty
  , insert
  )
import Data.List (elemIndex, sortOn)
import Data.Maybe (fromJust)

data Technique
  = NewStack
  | Cut Int
  | Increment Int
  deriving (Eq, Show)

type Card = Int

solve22p1IO :: FilePath -> IO ()
solve22p1IO fp = print . solve22p1 . parseInput =<< readFile fp

solve22p1 :: [Technique] -> Int
solve22p1 = fromJust . elemIndex 2019 . shuffle factoryDeck
  where
    shuffle :: [Card] -> [Technique] -> [Card]
    shuffle xs []     = xs
    shuffle xs (t:ts) =
      let tech = case t of
            NewStack    -> techNewStack
            Cut n       -> techCut n
            Increment n -> techIncrement n
      in shuffle (tech xs) ts

factoryDeck :: [Card]
factoryDeck = [0 .. 10006]

parseInput :: String -> [Technique]
parseInput = map parseTech . lines
  where
    parseTech :: String -> Technique
    parseTech s = case words s of
      ["deal", "into", "new", "stack"] -> NewStack
      ("cut" : [n]) -> Cut (read n)
      ("deal" : "with" : "increment" : [n]) -> Increment (read n)

techNewStack :: [Card] -> [Card]
techNewStack = reverse

techCut :: Int -> [Card] -> [Card]
techCut n xs
  | n < 0 = swap $ splitAt (length xs - abs n) xs
  | otherwise = swap $ splitAt n xs
  where
    swap (x, y) = y ++ x

techIncrement :: Int -> [Card] -> [Card]
techIncrement n cards = IM.elems $ deal cards 0 IM.empty
  where
    deal :: [Card] -> Int -> IM.IntMap Card -> IM.IntMap Card
    deal [] _ m = m
    deal (x:xs) counter m =
      let inserted = IM.insert counter x m
      in deal xs ((counter + n) `mod` length cards) inserted
