module Day5 (solve1, solve2) where

import Control.Monad (foldM)
import Data.Char (isLetter, isNumber)
import Data.List (transpose)
import Data.List.Split (splitWhen)
import Data.Text (Text, chunksOf, dropAround, pack, unpack)

-- Move X from Y into Z
type Command = (Int, Int, Int)

parseStackLines :: [Text] -> [[String]]
parseStackLines rows =
  map (map (unpack . dropAround (not . isLetter))) (chunksOf 4 <$> take (length rows - 1) rows)

stringToCommand :: [String] -> Command
stringToCommand s =
  let move = s !! 0
      from = s !! 1
      to = s !! 2
   in (read move, read from - 1, read to - 1)

parseCommands :: [String] -> [Command]
parseCommands cmds = stringToCommand <$> map (filter (/= "") . fmap (dropWhile (not . isNumber))) (splitWhen (== ' ') <$> cmds)

pushToStack :: Int -> [a] -> [[a]] -> [[a]]
pushToStack _ [] stacks = stacks
pushToStack idx stack stacks = do
  -- split at idx, target stack to replace in right side
  let (left, right) = splitAt idx stacks
  -- pop off the head of the right side
  left ++ ((stack ++ first right) : rest right)

pushToStackRev :: Int -> [a] -> [[a]] -> [[a]]
pushToStackRev _ [] stacks = stacks
pushToStackRev idx stack stacks = do
  -- split at idx, target stack to replace in right side
  let (left, right) = splitAt idx stacks
  -- pop off the head of the right side
  left ++ ((reverse stack ++ first right) : rest right)

popFront :: Int -> [a] -> [a]
popFront _ [] = []
popFront 0 xs = xs
popFront n (_ : xs) = popFront (n - 1) xs

first :: [[a]] -> [a]
first [] = []
first (x : _) = x

rest :: [[a]] -> [[a]]
rest [] = []
rest (_ : xs) = xs

popFromStack :: Int -> Int -> [[a]] -> ([a], [[a]])
popFromStack idx n stacks = do
  let (left, right) = splitAt idx stacks
      popped = take n (first right)
  (popped, left ++ (popFront n (first right) : rest right))

runCommand1 :: Command -> [[String]] -> IO [[String]]
runCommand1 (move, from, to) stacks = do
  let (popped, newStacks) = popFromStack from move stacks
  let new = pushToStackRev to popped newStacks
  -- print new
  pure new

runCommands1 :: [Command] -> [[String]] -> IO [[String]]
runCommands1 cmds stacks = foldM (flip runCommand1) stacks cmds

runCommand2 :: Command -> [[String]] -> IO [[String]]
runCommand2 (move, from, to) stacks = do
  let (popped, newStacks) = popFromStack from move stacks
  let new = pushToStack to popped newStacks
  -- print new
  pure new

runCommands2 :: [Command] -> [[String]] -> IO [[String]]
runCommands2 cmds stacks = foldM (flip runCommand2) stacks cmds

headOrBlank :: [String] -> String
headOrBlank [] = ""
headOrBlank (x : _) = x

-- for testing parse >:[
-- reverseCommandParse :: [Command] -> String
-- reverseCommandParse [] = []
-- reverseCommandParse ((move, from, to) : cmds) =
--   "move " ++ show move ++ " from " ++ show (from + 1) ++ " to " ++ show (to + 1) ++ "\n" ++ reverseCommandParse cmds

--
solve1 :: String -> IO String
solve1 input =
  let linez = lines input
      parsed = splitWhen (== "") linez -- split between stacks and commands
      stacks = map (filter (/= [])) . transpose $ parseStackLines $ pack <$> parsed !! 0
      commands = parseCommands $ parsed !! 1
   in do
        print stacks
        -- putStr $ reverseCommandParse commands
        final <- runCommands1 commands stacks
        pure $ concatMap headOrBlank final

solve2 :: String -> IO String
solve2 input =
  let linez = lines input
      parsed = splitWhen (== "") linez -- split between stacks and commands
      stacks = map (filter (/= [])) . transpose $ parseStackLines $ pack <$> parsed !! 0
      commands = parseCommands $ parsed !! 1
   in do
        print stacks
        -- putStr $ reverseCommandParse commands
        final <- runCommands2 commands stacks
        pure $ concatMap headOrBlank final
