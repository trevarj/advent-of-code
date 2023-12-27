{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString (writeFile)
import Data.ByteString.Char8 (pack)
import Day1Tests
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
getTests _ = error "no tests for given day"

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
  let request = addRequestHeader "COOKIE" sessionToken initRequest

  response <- httpBS request
  case getResponseStatusCode response of
    200 -> do
      let body = getResponseBody response
      writeFile filePath body
      return True
    _ -> return False
