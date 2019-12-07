module Day5Solution where

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

solve5p1 :: FilePath -> IO ()
solve5p1 fp = do
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

-- | Given an index, return the instruction and the next location of
--   the program pointer.
parseInstruction :: Int -> M.Map Int Int -> (Instruction, Int)
parseInstruction index m =
    if length modes /= length params
        then error "Non-matching length of modes and params!"
        else (Instruction opcode (zip modes params), index + numParams + 1)

    where
        instructionBase = m M.! index
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
        params = map (m M.!) [index + 1 .. index + numParams]

        numParams :: Int
        numParams = case opcode of
          99 -> 0
          1 -> 3
          2 -> 3
          3 -> 1
          4 -> 1
          _ -> error $ "Invalid opcode (" ++ show opcode
                     ++ ") found at index (" ++ show index ++ ")"

compute :: Int -> M.Map Int Int -> IO (M.Map Int Int)
compute pointer m =
    let (Instruction opcode xs, newPointer) = parseInstruction pointer m

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

     in
        case opcode of

        99 -> return m

        1 -> compute newPointer
                $ M.insert
                    (snd $ xs !! 2)
                    (add (xs !! 0) (xs !! 1))
                    m

        2 -> compute newPointer
                $ M.insert
                    (snd $ xs !! 2)
                    (mul (xs !! 0) (xs !! 1))
                    m

        3 -> putStr "Input > "
            >> getLine
            <&> read
            >>= \input -> compute newPointer
                            $ M.insert (snd $ head xs) input m

        4 -> print (getVal $ head xs)
            >> compute newPointer m
