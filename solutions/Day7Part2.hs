module Day7Part2 where

import Data.List (permutations)

import Day7Part2IntComputer
    ( St (St, stOutputs)
    , parse
    , resumeExecution
    , runWithArgs)

type Tape = [Int]

solve7p1 :: FilePath -> IO ()
solve7p1 fp = do
    tape <- parse <$> readFile fp

    return ()

numAmps :: Int
numAmps = 5



-- | Puts the feedback loop into action for one combination
--   of phase settings.
startAmplificationH :: Tape     -- Program tape.
                    -> [Int]    -- One combination of phase settings.
                    -> [Int]    -- Initial inputs.
                    -> IO [St]  -- Running amplifiers.
startAmplificationH _ [] _ = return []
startAmplificationH tape (phaseSetting : ps) inputs = do
    postSt@(St _ outputs _ _ _) <- runWithArgs tape (phaseSetting : inputs)
    xs <- startAmplificationH tape ps (reverse outputs)
    return $ postSt { stOutputs = [] } : xs

-- | Loop an amplification run for one combination of phase settings
--   and return the output signal after halting.
runAmplification :: [St]   -- States of running amplifiers.
                 -> [Int]  -- New input for the active (1st in line) amp.
                 -> IO Int -- Output signal once halted.
runAmplification (st : sts) inputs = do
    postSt@(St _ outputs _ _ halted) <- resumeExecution st inputs
    if halted
        then if null sts
                then return $ head outputs
                else runAmplification sts (reverse outputs)
        else runAmplification
                (sts ++ [postSt { stOutputs = [] }]) (reverse outputs)
