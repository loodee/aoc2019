module Day5Part2 where

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

solve5p2 :: FilePath -> IO ()
solve5p2 fp = do
    putStrLn $ "To run the TEST diagnostic program (and solve part 1),"
             ++ " enter '1' at the prompt."
    run . parse =<< readFile fp
    return ()

parse :: String -> [Int]
parse = map read . splitOn ","

-- | Takes an input tape and performs computations until halting,
--   returning the output tape.
run :: [Int] -> IO [Int]
run = fmap M.elems . compute 0 . M.fromList . zip [0 ..]

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

compute :: Int -> M.Map Int Int -> IO (M.Map Int Int)
compute pointer m =
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

        binBoolOp :: (Int -> Int -> Bool) -> IO (M.Map Int Int)
        binBoolOp op = compute incPointer newMap
            where
                boolToInt b = if b then 1 else 0
                res = getVal (ps !! 0) `op` getVal (ps !! 1)
                newMap = M.insert (snd $ ps !! 2) (boolToInt res) m

        lt = binBoolOp (<)
        eq = binBoolOp (==)

        jumpIf :: Bool -> IO (M.Map Int Int)
        jumpIf b = compute newPointer m
            where
                newPointer = if b == (getVal (head ps) /= 0)
                                then getVal (ps !! 1)
                                else incPointer

     in
        case opcode of

        99 -> return m

        1 -> compute incPointer
                $ M.insert (snd $ ps !! 2) (add (ps !! 0) (ps !! 1)) m

        2 -> compute incPointer
                $ M.insert (snd $ ps !! 2) (mul (ps !! 0) (ps !! 1)) m

        3 -> putStr "Input > "
            >> getLine
            <&> read
            >>= \input -> compute incPointer
                            $ M.insert (snd $ head ps) input m

        4 -> print (getVal $ head ps)
            >> compute incPointer m

        5 -> jumpIf True

        6 -> jumpIf False

        7 -> lt

        8 -> eq
