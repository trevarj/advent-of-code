module Day3 (solve1, solve2) where

import Data.Char (isLower, isUpper, ord)
import Data.List (intersect)
import Data.List.Split
import Data.Set (fromList, intersection, toList)

priority :: Char -> Int
priority c
  | isLower c = ord c - 96
  | isUpper c = ord c - 38
  | otherwise = error "invalid char"

findDuplicate :: String -> Char
findDuplicate s = let (left, right) = splitAt (div (length s) 2) s in last $ intersect left right

findCommon :: [String] -> Char
findCommon s = last . toList $ foldr1 intersection (fromList <$> s)

solve1 :: String -> IO String
solve1 input = pure . show . sum $ priority . findDuplicate <$> lines input

solve2 :: String -> IO String
solve2 input = pure . show . sum $ priority . findCommon <$> chunksOf 3 (lines input)
