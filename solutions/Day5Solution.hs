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

-- | Given an index, return the instruction and how many steps to move
--   afterwards.
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

solve5 :: FilePath -> IO ()
solve5 fp = do
    tape <- parse <$> readFile fp

    print tape

    return ()

parse :: String -> [Int]
parse = map read . splitOn ","

-- prepInput :: (Int, Int) -> [Int] -> [Int]
-- prepInput (noun, verb) (x:_:_:xs) = x : noun : verb : xs


-- buildMap :: [Int] -> Map Int Int
-- buildMap = M.fromList . index 0
--     where
--         index :: Int -> [Int] -> [(Int, Int)]
--         index _ [] = []
--         index n (x:xs) = (n, x) : index (n + 1) xs

-- run :: Map Int Int -> [Int]
-- run = elems . compute 0

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
