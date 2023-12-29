{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString (writeFile)
import Data.ByteString.Char8 (pack)
import qualified Day10Tests
import qualified Day11Tests
import qualified Day12Tests
import qualified Day13Tests
import qualified Day14Tests
import qualified Day15Tests
import qualified Day16Tests
import qualified Day17Tests
import qualified Day18Tests
import qualified Day19Tests
import qualified Day1Tests
import qualified Day20Tests
import qualified Day21Tests
import qualified Day22Tests
import qualified Day23Tests
import qualified Day24Tests
import qualified Day25Tests
import qualified Day2Tests
import qualified Day3Tests
import qualified Day4Tests
import qualified Day5Tests
import qualified Day6Tests
import qualified Day7Tests
import qualified Day8Tests
import qualified Day9Tests
import Network.HTTP.Client
import Network.HTTP.Simple (addRequestHeader, getResponseBody, getResponseStatusCode, httpBS)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Prelude hiding (writeFile)

type TestResult = (String, Int, Int)

type Test = [String] -> TestResult

data Day
  = Day1
  | Day2
  | Day3
  | Day4
  | Day5
  | Day6
  | Day7
  | Day8
  | Day9
  | Day10
  | Day11
  | Day12
  | Day13
  | Day14
  | Day15
  | Day16
  | Day17
  | Day18
  | Day19
  | Day20
  | Day21
  | Day22
  | Day23
  | Day24
  | Day25
  deriving (Enum, Read, Show)

data Command = Run Day | RunAll

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    RunAll -> return () {- TODO -}
    Run day -> do
      putStrLn $ "Testing " ++ show day
      session <- getSessionKey
      maybeInput <- getInputFile session day
      case maybeInput of
        Just input -> printResults . runDayTests input . getTests $ day
        Nothing -> error "no input file"

parseArgs :: [String] -> Command
parseArgs [] = RunAll
parseArgs (arg : _) = case readMaybe arg of
  Just day -> Run day
  Nothing -> error "no day provided"

getTests :: Day -> [Test]
getTests Day1 = [Day1Tests.part1, Day1Tests.part2]
getTests Day2 = [Day2Tests.part1, Day2Tests.part2]
getTests Day3 = [Day3Tests.part1, Day3Tests.part2]
getTests Day4 = [Day4Tests.part1, Day4Tests.part2]
getTests Day5 = [Day5Tests.part1, Day5Tests.part2]
getTests Day6 = [Day6Tests.part1, Day6Tests.part2]
getTests Day7 = [Day7Tests.part1, Day7Tests.part2]
getTests Day8 = [Day8Tests.part1, Day8Tests.part2]
getTests Day9 = [Day9Tests.part1, Day9Tests.part2]
getTests Day10 = [Day10Tests.part1, Day10Tests.part2]
getTests Day11 = [Day11Tests.part1, Day11Tests.part2]
getTests Day12 = [Day12Tests.part1, Day12Tests.part2]
getTests Day13 = [Day13Tests.part1, Day13Tests.part2]
getTests Day14 = [Day14Tests.part1, Day14Tests.part2]
getTests Day15 = [Day15Tests.part1, Day15Tests.part2]
getTests Day16 = [Day16Tests.part1, Day16Tests.part2]
getTests Day17 = [Day17Tests.part1, Day17Tests.part2]
getTests Day18 = [Day18Tests.part1, Day18Tests.part2]
getTests Day19 = [Day19Tests.part1, Day19Tests.part2]
getTests Day20 = [Day20Tests.part1, Day20Tests.part2]
getTests Day21 = [Day21Tests.part1, Day21Tests.part2]
getTests Day22 = [Day22Tests.part1, Day22Tests.part2]
getTests Day23 = [Day23Tests.part1, Day23Tests.part2]
getTests Day24 = [Day24Tests.part1, Day24Tests.part2]
getTests Day25 = [Day25Tests.part1, Day25Tests.part2]

runDayTests :: [String] -> [Test] -> [TestResult]
runDayTests input = map (\t -> t input)

printResult :: TestResult -> String
printResult (name, got, want)
  | got == want = name ++ ": ✅"
  | otherwise = name ++ ": ❎ => expected " ++ show got ++ ", but got " ++ show want

printResults :: [TestResult] -> IO ()
printResults [] = pure ()
printResults (res : rs) = do
  putStrLn . printResult $ res
  printResults rs

getSessionKey :: IO String
getSessionKey = head . lines <$> readFile "SESSION_KEY"

getInputFile :: String -> Day -> IO (Maybe [String])
getInputFile session day = do
  let file = "test/inputs/" ++ show day ++ ".txt"
  exists <- doesFileExist file
  if exists
    then Just . lines <$> readFile file
    else do
      success <- fetchInputFile (fromEnum day + 1) session file
      if success
        then Just . lines <$> readFile file
        else pure Nothing

fetchInputFile :: Int -> String -> String -> IO Bool
fetchInputFile day token filePath = do
  initRequest <- parseRequest $ "https://adventofcode.com/2019/day/" ++ show day ++ "/input"
  let sessionToken = pack $ "session=" ++ token
  let request =
        addRequestHeader "User-Agent" "trevarj via Haskell AoC solutions" $
          addRequestHeader "COOKIE" sessionToken initRequest

  response <- httpBS request
  case getResponseStatusCode response of
    200 -> do
      let body = getResponseBody response
      writeFile filePath body
      return True
    code -> do
      putStrLn $ "error fetching input file: status " ++ show code
      return False
