module Day5Solution where

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
type Param = Int
type Opcode = Int

data Instruction = Instruction Opcode [(Mode, Param)]
    deriving (Eq, Show)

solve5 :: FilePath -> IO ()
solve5 fp = do
    tape <- parse <$> readFile fp

    print tape

    return ()

parse :: String -> [Int]
parse = map read . splitOn ","

-- prepInput :: (Int, Int) -> [Int] -> [Int]
-- prepInput (noun, verb) (x:_:_:xs) = x : noun : verb : xs

-- | Takes an input tape and performs computations until halting,
--   returning the output tape.
run :: [Int] -> [Int]
run = M.elems . compute 0 . M.fromList . zip [0 ..]

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

compute :: Int -> M.Map Int Int -> M.Map Int Int
compute pointer m =
    let (Instruction opcode xs, newPointer) = parseInstruction pointer m

        binOp :: (Mode, Param)
              -> (Mode, Param)
              -> (Int -> Int -> Int)
              -> M.Map Int Int
              -> Int
        binOp t1 t2 f m = getVal t1 `f` getVal t2
            where 
                getVal (mode, param) = case mode of
                                        0 -> m M.! param
                                        1 -> param

     in
        case opcode of
        99 -> m

        1 -> compute newPointer
                $ M.insert
                    (snd $ xs !! 2)
                    (binOp (xs !! 0) (xs !! 1) (+) m)
                    m

        2 -> compute newPointer
                $ M.insert
                    (snd $ xs !! 2)
                    (binOp (xs !! 0) (xs !! 1) (*) m)
                    m

        3 -> undefined

        4 -> undefined

-- compute :: Int -> Map Int Int -> Map Int Int
-- compute i m
--     | opcode == 99 = m
--     | otherwise = let (n1, n2, loc) = comp i m in
--         compute (i + 4) $ M.insert loc (n1 `op` n2) m
--     where
--         opcode = fromJust $ M.lookup i m
--         op = case opcode of
--             1 -> (+)
--             2 -> (*)

-- comp :: Int -> Map Int Int -> (Int, Int, Int)
-- comp i m = ( fromJust (M.lookup n1 m)
--            , fromJust (M.lookup n2 m)
--            , loc
--            )
--     where [n1, n2, loc] = mapMaybe (\x -> M.lookup (i + x) m) [1 .. 3]
