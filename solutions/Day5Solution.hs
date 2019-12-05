module Day5Solution where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

type Mode = Int  -- 0 = Position mode, 1 = Immediate mode
type Param = Int
type Opcode = Int

data Instruction = Instruction Opcode [(Mode, Param)]

readInstruction :: [Int] -> (Instruction, Int)

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
