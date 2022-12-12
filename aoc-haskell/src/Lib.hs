{-# LANGUAGE ImportQualifiedPost #-}

module Lib (
  parseArgs,
  Error,
  runSolution,
) where

import Data.Maybe
import Day1 qualified
import Day2 qualified
import Text.Printf
import Text.Read

data Error = ArgsError | InvalidDay | InvalidPart deriving (Show)

checkDay :: String -> Either Error Int
checkDay s
  | day <= Just 25 && day > Just 0 = Right $ fromJust day
  | otherwise = Left InvalidDay
 where
  day = readMaybe s
checkPart :: String -> Either Error Int
checkPart s
  | part == Just 1 || part == Just 2 = Right $ fromJust part
  | otherwise = Left InvalidPart
 where
  part = readMaybe s

parseArgs :: [String] -> Either Error (Int, Int)
parseArgs (d : p : _) = do
  day <- checkDay d
  part <- checkPart p
  Right (day, part)
parseArgs _ = Left ArgsError

getInputFileName :: (Int, Int) -> String
getInputFileName (day, part) = printf "./inputs/day%d_part%d.txt" day part

runSolution :: (Int, Int) -> IO String
runSolution dayPart =
  do
    let fileName = getInputFileName dayPart
    input <- readFile fileName
    let solver = case dayPart of
          (1, 1) -> Day1.solve1
          (1, 2) -> Day1.solve2
          (2, 1) -> Day2.solve1
          _ -> const "no solution yet"
     in pure $ solver input
