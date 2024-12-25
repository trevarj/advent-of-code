module Day4Tests where

import qualified Day4

part1 :: [String] -> (String, Int, Int)
part1 input = ("Day 4 Part 1", Day4.solvePart1 input, 2050)

part2 :: [String] -> (String, Int, Int)
part2 input = ("Day 4 Part 2", Day4.solvePart2 input, 1390)
