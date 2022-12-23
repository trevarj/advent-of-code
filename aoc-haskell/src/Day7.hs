{-# LANGUAGE NamedFieldPuns #-}

module Day7 (solve1, solve2) where

import Data.Map.Strict (Map, filter, insert, mapWithKey, singleton, (!))
import Debug.Trace

data CommandLineInput
  = ChangeDir String
  | ChangeDirBack
  | Ls -- list current directory's files
  | DirListing String -- a named directory of files
  | FileListing Int String -- a regular data file with size and name
  deriving (Show)

data FileType
  = Dir String
  | File String Int
  deriving (Show)

type FileSystem = Map [String] [FileType]

data Context = Context
  { dirs :: FileSystem,
    currentPath :: [String] -- ex. /a/b/c but ["c", "b", "a"]
  }
  deriving (Show)

parseCommandLine :: String -> CommandLineInput
parseCommandLine line = case tokens of
  ["$", "cd", ".."] -> ChangeDirBack
  ["$", "cd", target] -> ChangeDir target
  ["$", "ls"] -> Ls
  ["dir", dirName] -> DirListing dirName
  [fileSize, name] -> FileListing (read fileSize) name
  _ -> error "unexpected line"
  where
    tokens = words line

processCommand :: CommandLineInput -> Context -> Context
processCommand (ChangeDir target) context = traceShow ("changing to dir " ++ target) $ context {currentPath = target : currentPath context}
processCommand ChangeDirBack context = context {currentPath = drop 1 $ currentPath context}
processCommand Ls context = context
processCommand (DirListing name) (Context {dirs, currentPath}) =
  traceShow ("found dir " ++ name ++ " " ++ show currentPath) $
    let dirPath = name : currentPath
        updatedDirs = insert currentPath (Dir name : dirs ! currentPath) (insert dirPath [] dirs)
     in traceShow updatedDirs $ Context {dirs = updatedDirs, currentPath}
processCommand (FileListing fsize name) (Context {dirs, currentPath}) =
  traceShow ("found file " ++ name) $
    let updatedDirs = insert currentPath (File name fsize : dirs ! currentPath) dirs
     in Context {dirs = updatedDirs, currentPath}

calculateDirSize :: [String] -> Context -> [FileType] -> Int -> Int
calculateDirSize _ _ [] acc = acc
calculateDirSize path context (File _ size : fs) acc = calculateDirSize path context fs (size + acc)
calculateDirSize path context (Dir name : fs) acc =
  let newPath = name : path
   in calculateDirSize path context fs acc + calculateDirSize (name : path) context (dirs context ! newPath) 0

calculateDirSizes :: Context -> Map [String] Int
calculateDirSizes context = mapWithKey (\k v -> calculateDirSize k context v 0) (dirs context)

solve1 :: String -> IO String
solve1 input = do
  let commands = parseCommandLine <$> lines input
  -- print commands
  let context = foldr processCommand (Context {dirs = singleton ["/"] [], currentPath = []}) (reverse commands)
  let dirSizes = sum $ Data.Map.Strict.filter (<= 100000) (calculateDirSizes context)
  -- print context
  pure $ show dirSizes

solve2 :: String -> IO String
solve2 input = do
  let commands = parseCommandLine <$> lines input
      -- print commands
      context = foldr processCommand (Context {dirs = singleton ["/"] [], currentPath = []}) (reverse commands)
      dirSizes = calculateDirSizes context
      rootDirSize = dirSizes ! ["/"]
      toRemove = minimum $ Data.Map.Strict.filter (\v -> (70000000 - rootDirSize + v) >= 30000000) dirSizes
  -- print rootDirSize
  -- print dirSizes
  -- print context
  pure $ show toRemove
