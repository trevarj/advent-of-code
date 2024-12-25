module Day1 (solvePart1, solvePart2) where

fuelRequired :: Int -> Int
fuelRequired mass = div mass 3 - 2

fuelRecurse :: Int -> Int
fuelRecurse mass =
  let f = fuelRequired mass
   in sum $ takeWhile (> 0) (scanl (\f' _ -> fuelRequired f') f ([1 ..] :: [Int]))

solvePart1 :: [String] -> Int
solvePart1 = foldr (\m acc -> acc + (fuelRequired . read $ m)) 0

solvePart2 :: [String] -> Int
solvePart2 = foldr (\m acc -> acc + (fuelRecurse . read $ m)) 0
