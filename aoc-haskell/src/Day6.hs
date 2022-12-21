module Day6 (solve1, solve2) where

import Data.List (union)

indexAfterMarker' :: Int -> Int -> String -> IO Int
indexAfterMarker' _ _ [] = error "not found"
indexAfterMarker' pos count (ch : chrs)
  | unique = pure $ count + pos
  | otherwise = indexAfterMarker' pos (count + 1) chrs
  where
    unique = length ([ch] `union` take (pos - 1) chrs) == pos

indexAfterMarker :: Int -> String -> IO Int
indexAfterMarker pos = indexAfterMarker' pos 0

solve1 :: String -> IO String
solve1 input = do
  chars <- indexAfterMarker 4 input
  pure $ show chars

solve2 :: String -> IO String
solve2 input = do
  chars <- indexAfterMarker 14 input
  pure $ show chars
