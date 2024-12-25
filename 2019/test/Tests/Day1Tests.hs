module Day1Tests where

import qualified Day1

part1 :: [String] -> (String, Int, Int)
part1 input = ("Day 1 Part 1", Day1.solvePart1 input, 3380880)

part2 :: [String] -> (String, Int, Int)
part2 input = ("Day 1 Part 2", Day1.solvePart2 input, 5068454)
