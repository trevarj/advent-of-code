module Day2 (solvePart1, solvePart2) where

import AoC.Intcode (Program, interpret)
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Vector ((//))
import qualified Data.Vector as V

parse :: [String] -> [Int]
parse = concatMap (map read <$> splitOn ",")

program :: [String] -> Program
program = V.fromList . parse

solve :: Int -> Int -> Program -> Int
solve a b prog = interpret (prog // [(1, a), (2, b)]) 0 V.! 0

solvePart1 :: [String] -> Int
solvePart1 = solve 12 2 . program

solvePart2 :: [String] -> Int
solvePart2 lns =
  let prog = program lns
   in let (noun, verb) =
            fromJust $
              find
                (\(a, b) -> 19690720 == solve a b prog)
                [(i, j) | i <- [0 .. 99], j <- [0 .. 99]]
       in 100 * noun + verb
