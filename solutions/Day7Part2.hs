module Day7Part2 where

import Data.List (permutations)

import Day7Part2IntComputer
    ( St (St, stOutputs, stInputs)
    , parse
    , resumeExecution
    , runWithArgs
    )

type Tape = [Int]

solve7p1 :: FilePath -> IO ()
solve7p1 fp = print
            =<< (\t -> maxThrusterSignal t $ permutations [5 .. 9])
            . parse
            =<< readFile fp

maxThrusterSignal :: Tape     -- Program tape.
                  -> [[Int]]  -- All combinations of possible phase settings.
                  -> IO Int   -- The highest output signal to the thrusters.
maxThrusterSignal _ [] = return 0
maxThrusterSignal tape (ps : pss) = do
    sts <- startAmplification tape ps [0]
    signal <- runAmplification (map fst sts) (snd $ last sts)
    max signal <$> maxThrusterSignal tape pss

-- ^-This-^ and v-this-v use ugly hacks to get correct inputs,
-- should be fixed at some point.

-- | Puts the feedback loop into action for one combination
--   of phase settings.
startAmplification :: Tape              -- Program tape.
                   -> [Int]             -- One combination of phase settings.
                   -> [Int]             -- Initial inputs.
                   -> IO [(St, [Int])]  -- Running amplifiers and each of
                                        -- their outputs (first halt).
startAmplification _ [] _ = return []
startAmplification tape (phaseSetting : ps) inputs = do
    postSt@(St _ outputs _ _ _) <- runWithArgs tape (phaseSetting : inputs)
    xs <- startAmplification tape ps (reverse outputs)
    return $ (postSt { stOutputs = [] }, reverse outputs) : xs

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
