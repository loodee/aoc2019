module Day9Part1 where

import Data.Char (digitToInt)
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
type Opcode = Int
type Param = Int
type Tape = [Int]

data Instruction = Instruction Opcode [(Mode, Param)]
    deriving (Eq, Show)

data St = St
    { stInputs  :: [Int]          -- All inputs that the program will need while running.
    , stOutputs :: [Int]          -- Collected outputs during the run
    , stPointer :: Int            -- Program pointer.
    , stMap     :: M.Map Int Int  -- Map serving as an indexing of values.
    , stHalted  :: Bool           -- True if the program has halted (99)
    } deriving (Eq, Show)

readRunWithArgs :: FilePath -> [Int] -> IO St
readRunWithArgs fp inputs = do
    tape <- parse <$> readFile fp
    return $ runWithArgs tape inputs

parse :: String -> [Int]
parse = map read . splitOn ","

runWithArgs :: Tape  -- Program tape.
            -> [Int] -- List of all inputs that the program will need.
            -> St
runWithArgs tape inputs =
    let st = St { stInputs  = inputs
                , stOutputs = []
                , stPointer = 0
                , stMap     = M.fromList . zip [0 ..] $ tape
                , stHalted  = False
                }
    in step st

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

step :: St -> St
step st@(St inputs outputs pointer m h) =
  let Instruction opcode ps = parseInstruction pointer m

      -- Default new pointer location provided the pointer is
      -- not otherwise explicitly set.
      incPointer :: Int
      incPointer = pointer + length ps + 1

      -- | Returns the proper value from a Mode/Param pair, depending
      --   on the Mode.
      getVal :: (Mode, Param) -> Param
      getVal (mode, param) = case mode of
                              0 -> m M.! param
                              1 -> param

      add :: St
      add = binNumOp (+)

      mul :: St
      mul = binNumOp (*)

      binNumOp :: (Int -> Int -> Int) -> St
      binNumOp op = step $ st { stMap = newMap
                              , stPointer = incPointer
                              }
        where
          res = getVal (ps !! 0) `op` getVal (ps !! 1)
          newMap = M.insert (snd $ ps !! 2) res m

      lt :: St
      lt = binBoolOp (<)

      eq :: St
      eq = binBoolOp (==)

      binBoolOp :: (Int -> Int -> Bool) -> St
      binBoolOp op = step $ st { stMap = newMap
                               , stPointer = incPointer
                               }
        where
          boolToInt b = if b then 1 else 0
          res = getVal (ps !! 0) `op` getVal (ps !! 1)
          newMap = M.insert (snd $ ps !! 2) (boolToInt res) m

      jumpIfTrue :: St
      jumpIfTrue = jumpIf True

      jumpIfFalse :: St
      jumpIfFalse = jumpIf False

      jumpIf :: Bool -> St
      jumpIf b = step $ st { stPointer = newPointer }
        where
          newPointer = if b == (getVal (head ps) /= 0)
                        then getVal (ps !! 1)
                        else incPointer

  in
    case opcode of

      99 -> st { stHalted = True }

      1 -> add

      2 -> mul

      3 -> if null inputs
            then st
            else step $ st { stInputs = tail inputs
                           , stPointer = incPointer
                           , stMap = M.insert (snd $ head ps) (head inputs) m
                           }

      4 -> step $ St inputs (getVal (head ps) : outputs) incPointer m h

      5 -> jumpIf True

      6 -> jumpIf False

      7 -> lt

      8 -> eq
