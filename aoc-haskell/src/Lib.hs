{-# LANGUAGE ImportQualifiedPost #-}

module Lib
  ( parseArgs,
    Error,
    runSolution,
  )
where

import Data.Maybe
import Day1 qualified
import Day2 qualified
import Day3 qualified
import Day4 qualified
import Day5 qualified
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

getInputFileName :: Int -> String
getInputFileName = printf "./inputs/day%d.txt"

runSolution :: (Int, Int) -> IO String
runSolution dayPart =
  do
    let fileName = getInputFileName $ fst dayPart
    input <- readFile fileName
    let solver = case dayPart of
          (1, 1) -> Day1.solve1
          (1, 2) -> Day1.solve2
          (2, 1) -> Day2.solve1
          (2, 2) -> Day2.solve2
          (3, 1) -> Day3.solve1
          (3, 2) -> Day3.solve2
          (4, 1) -> Day4.solve1
          (4, 2) -> Day4.solve2
          (5, 1) -> Day5.solve1
          -- (5, 2) -> Day5.solve2
          _ -> pure . const "no solution yet"
     in solver input
