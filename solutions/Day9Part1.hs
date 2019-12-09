module Day9Part1 where

import Data.Char (digitToInt)
import Data.List.Split (splitOn)
import qualified Data.IntMap.Strict as IM
    ( IntMap
    , elems
    , fromList
    , insert
    , lookup
    , (!)
    )
import Data.Maybe (fromMaybe)

type Mode = Int  -- 0 = Position mode, 1 = Immediate mode, 2 = Relative mode
type Opcode = Int
type Param = Int
type Tape = [Int]

data ReadWrite = R | W

data Instruction = Instruction Opcode [(Mode, Param)]
    deriving (Eq, Show)

data St = St
    { stInputs  :: [Int]          -- All inputs that the program will need while running.
    , stOutputs :: [Int]          -- Collected outputs during the run
    , stPointer :: Int            -- Program pointer.
    , stMap     :: IM.IntMap Int  -- Map serving as an indexing of values.
    , stHalted  :: Bool           -- True if the program has halted (code 99).
    , stRelBase :: Int            -- Relative base value of computer.
    } deriving (Eq, Show)

readRunWithArgs :: FilePath -> [Int] -> IO ()
readRunWithArgs fp inputs = do
    tape <- parse <$> readFile fp
    mapM_ print
      . (\(St _ outputs _ _ _ _) -> outputs)
      . runWithArgs tape
      $ inputs

parse :: String -> [Int]
parse = map read . splitOn ","

runWithArgs :: Tape  -- Program tape.
            -> [Int] -- List of all inputs that the program will need.
            -> St
runWithArgs tape inputs =
    let st = St { stInputs  = inputs
                , stOutputs = []
                , stPointer = 0
                , stMap     = IM.fromList . zip [0 ..] $ tape
                , stHalted  = False
                , stRelBase = 0
                }
    in step st

-- | Given an index, return the instruction contaning its opcode
--   and its parameters, annotated with their modes.
parseInstruction :: Int -> IM.IntMap Int -> Instruction
parseInstruction pointer m =
    if length modes /= length params
      then error "Non-matching length of modes and params!"
      else Instruction opcode (zip modes params)

    where
      instructionBase = m IM.! pointer
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
      params = map (m IM.!) [pointer + 1 .. pointer + numParams]

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
        9 -> 1
        _ -> error $ "Invalid opcode (" ++ show opcode
                   ++ ") found at index (" ++ show pointer ++ ")"

step :: St -> St
step st@(St inputs outputs pointer m h relBase) =
  let Instruction opcode ps = parseInstruction pointer m

      -- Default new pointer location provided the pointer is
      -- not otherwise explicitly set.
      incPointer :: Int
      incPointer = pointer + length ps + 1

      -- | Returns the proper value from a Mode/Param pair, depending
      --   on the Mode.
      getVal :: ReadWrite -> (Mode, Param) -> Param
      getVal W (mode, param) = case mode of
        0 -> param
        1 -> error "Attempted write in immediate mode!"
        2 -> param + relBase
      getVal R (mode, param) = case mode of
        0 -> fromMaybe 0 $ IM.lookup param m
        1 -> param
        2 -> fromMaybe 0 $ IM.lookup (param + relBase) m

      add :: St
      add = binNumOp (+)

      mul :: St
      mul = binNumOp (*)

      binNumOp :: (Int -> Int -> Int) -> St
      binNumOp op = step $ st { stMap = newMap
                              , stPointer = incPointer
                              }
        where
          res = getVal R (ps !! 0) `op` getVal R (ps !! 1)
          newMap = IM.insert (getVal W $ ps !! 2) res m

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
          res = getVal R (ps !! 0) `op` getVal R (ps !! 1)
          newMap = IM.insert (getVal W $ ps !! 2) (boolToInt res) m

      jumpIfTrue :: St
      jumpIfTrue = jumpIf True

      jumpIfFalse :: St
      jumpIfFalse = jumpIf False

      jumpIf :: Bool -> St
      jumpIf b = step $ st { stPointer = newPointer }
        where
          newPointer = if b == (getVal R (head ps) /= 0)
                        then getVal R (ps !! 1)
                        else incPointer

      input :: St
      input = if null inputs
                then st  -- Halt if there is no input to read
                else step
                  $ st { stInputs = tail inputs
                       , stPointer = incPointer
                       , stMap = IM.insert (getVal W $ head ps) (head inputs) m
                       }

      output :: St
      output = step $ st { stOutputs = getVal R (head ps) : outputs
                         , stPointer = incPointer
                         }

      relBaseOffset :: St
      relBaseOffset = step $ st { stPointer = incPointer
                                , stRelBase = getVal R (head ps) + relBase
                                }

  in
    case opcode of
      99 -> st { stHalted = True }
      1 -> add
      2 -> mul
      3 -> input
      4 -> output
      5 -> jumpIfTrue
      6 -> jumpIfFalse
      7 -> lt
      8 -> eq
      9 -> relBaseOffset
