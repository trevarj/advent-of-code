#/bin/sh

solution_template='module Day# (solvePart1, solvePart2) where

solvePart1 :: [String] -> Int
solvePart1 lns = 0

solvePart2 :: [String] -> Int
solvePart2 lns = 0'

test_template='module Day#Tests where

import qualified Day#

part1 :: [String] -> (String, Int, Int)
part1 input = ("Day # Part 1", Day#.solvePart1 input, 1)

part2 :: [String] -> (String, Int, Int)
part2 input = ("Day # Part 2", Day#.solvePart2 input, 1)'

for i in {1..25}; do
	replace="s/#/$i/g"
	echo "$solution_template" | sed -e $replace >lib/Day${i}.hs
	echo "$test_template" | sed -e $replace >test/Day${i}Tests.hs
done
