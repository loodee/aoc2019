-- Modified version of Intcode computer to not require
-- input during program execution.

module Day5Plus where

import Data.Char (digitToInt)
import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
    ( Map
    , elems
    , fromList
    , insert
    , lookup
    , (!)
    )

type Mode = Int  -- 0 = Position mode, 1 = Immediate mode
type Param = Int
type Opcode = Int

data Instruction = Instruction Opcode [(Mode, Param)]
    deriving (Eq, Show)

data St = St
    { stInputs  :: [Int]          -- All inputs that the program will need while running.
    , stOutputs :: [Int]          -- Collected outputs during the run
    , stPointer :: Int            -- Program pointer.
    , stMap     :: M.Map Int Int  -- Map serving as an indexing of values.
    } deriving (Eq, Show)

readRunWithArgs :: FilePath -> [Int] -> IO St
readRunWithArgs fp inputs = do
    tape <- parse <$> readFile fp
    runWithArgs tape inputs

runWithArgs :: [Int]  -- Program tape.
            -> [Int]  -- List of all inputs that the program will need.
            -> IO St
runWithArgs tape inputs = do
    let st = St (inputs ++ padding) [] 0 (M.fromList . zip [0 ..] $ tape)
    run st
    
    where padding = replicate 50 0

parse :: String -> [Int]
parse = map read . splitOn ","

run :: St -> IO St
run = compute

-- | Given an index, return the instruction contaning its opcode
--   and its parameters, annotated with their modes.
parseInstruction :: Int -> M.Map Int Int -> Instruction
parseInstruction pointer m =
    if length modes /= length params
        then error "Non-matching length of modes and params!"
        else Instruction opcode (zip modes params)

    where
        instructionBase = m M.! pointer
        opcodeLength = 2

        opcode :: Opcode
        opcode = read
               . reverse
               . take opcodeLength
               . reverse 
               . show
               $ instructionBase

        modes :: [Mode]
        modes = map digitToInt
              . (\xs -> xs ++ replicate (numParams - length xs) '0')
              . drop opcodeLength
              . reverse 
              . show
              $ instructionBase

        params :: [Param]
        params = map (m M.!) [pointer + 1 .. pointer + numParams]

        numParams :: Int
        numParams = case opcode of
          99 -> 0
          1 -> 3
          2 -> 3
          3 -> 1
          4 -> 1
          5 -> 2
          6 -> 2
          7 -> 3
          8 -> 3
          _ -> error $ "Invalid opcode (" ++ show opcode
                     ++ ") found at index (" ++ show pointer ++ ")"

compute :: St -> IO St
compute st@(St inputs@(i:is) outputs pointer m) =
    let Instruction opcode ps = parseInstruction pointer m

        -- Default new pointer location provided the pointer is
        -- not otherwise explicitly set.
        incPointer = pointer + length ps + 1

        -- | Returns the proper value from a Mode/Param pair, depending
        --   on the Mode.
        getVal :: (Mode, Param) -> Param
        getVal (mode, param) = case mode of
                                0 -> m M.! param
                                1 -> param

        binOp :: (Int -> Int -> Int)
              -> (Mode, Param)
              -> (Mode, Param)
              -> Int
        binOp op t1 t2 = getVal t1 `op` getVal t2

        add = binOp (+)
        mul = binOp (*)

        binBoolOp :: (Int -> Int -> Bool) -> IO St
        binBoolOp op = compute $ St inputs outputs incPointer newMap
            where
                boolToInt b = if b then 1 else 0
                res = getVal (ps !! 0) `op` getVal (ps !! 1)
                newMap = M.insert (snd $ ps !! 2) (boolToInt res) m

        lt = binBoolOp (<)
        eq = binBoolOp (==)

        jumpIf :: Bool -> IO St
        jumpIf b = compute $ St inputs outputs newPointer m
            where
                newPointer = if b == (getVal (head ps) /= 0)
                                then getVal (ps !! 1)
                                else incPointer

     in
        case opcode of

        99 -> return st

        1 -> compute
                $ St inputs outputs incPointer
                $ M.insert (snd $ ps !! 2) (add (ps !! 0) (ps !! 1)) m

        2 -> compute
                $ St inputs outputs incPointer
                $ M.insert (snd $ ps !! 2) (mul (ps !! 0) (ps !! 1)) m

        3 -> compute
                $ St is outputs incPointer $ M.insert (snd $ head ps) i m

        4 -> compute (St inputs (getVal (head ps) : outputs) incPointer m)

        5 -> jumpIf True

        6 -> jumpIf False

        7 -> lt

        8 -> eq
