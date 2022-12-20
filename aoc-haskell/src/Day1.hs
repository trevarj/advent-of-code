module Day1 (solve1, solve2) where

import Data.List (insert)

accumulateUntil :: Int -> [String] -> (String -> Bool) -> (Int, [String])
accumulateUntil acc [] _ = (acc, [])
accumulateUntil acc (x : xs) cond
  | cond x = (acc, xs)
  | otherwise = accumulateUntil (acc + read x) xs cond

solve1' :: Int -> [String] -> (Int, [String])
solve1' high [] = (high, [])
solve1' high input
  | acc > high = solve1' acc xs
  | otherwise = solve1' high xs
  where
    (acc, xs) = accumulateUntil 0 input (== "")

{- Finding the largest sum in list delimited by newlines -}
solve1 :: String -> IO String
solve1 = pure . show . fst . solve1' 0 . lines

solve2' :: [Int] -> [String] -> ([Int], [String])
solve2' top3 [] = (top3, [])
solve2' top3 input = let (acc, xs) = accumulateUntil 0 input (== "") in solve2' (insert acc top3) xs

{- Finding the top three largest sums in list delimited by newlines -}
solve2 :: String -> IO String
solve2 = pure . show . sum . take 3 . reverse . fst . solve2' [] . lines
