module Day7Part2 where

import Data.List (permutations)

import Day7Part2IntComputer (St (St), parse, runWithArgs)

type Tape = [Int]

solve7p1 :: FilePath -> IO ()
solve7p1 fp = do
    tape <- parse <$> readFile fp

    print =<< bruteForce tape (permutations [0 .. 4]) 0

-- Given all permutations of phase settings, finds the highest
-- possible output signal
bruteForce :: Tape -> [[Int]] -> Int -> IO Int
bruteForce _ [] maxSignal = return maxSignal
bruteForce tape (x:xs) best = do
    signal <- calcThrusters tape x 0
    bruteForce tape xs $ max signal best

calcThrusters :: Tape -> [Int] -> Int -> IO Int
calcThrusters _ [] outputSignal = return outputSignal
calcThrusters tape (phaseSetting : ps) inputSignal = do
    nextSignal <- calcSignal tape [phaseSetting, inputSignal]
    calcThrusters tape ps nextSignal

calcSignal :: Tape -> [Int] -> IO Int
calcSignal tape inputs = do
    St _ outputs _ _ <- runWithArgs tape inputs
    return (head outputs)
