module Day3 (solvePart1, solvePart2) where

import AoC.Grid (Direction (..), Point, gridDrawLine, manhattan)
import Data.List (foldl1')
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (Left, Right)

parseDirection :: Char -> Direction
parseDirection 'U' = Up
parseDirection 'D' = Down
parseDirection 'L' = Left
parseDirection 'R' = Right
parseDirection _ = error "invalid direction"

-- >>> parseMovement "D30"
-- (Down, 30)
parseMovement :: String -> (Direction, Int)
parseMovement s = (parseDirection $ head s, read $ tail s)

-- >>> parse "R75,D30,R83"
-- [(Right,75),(Down,30),(Right,83)]
parse :: String -> [(Direction, Int)]
parse = map parseMovement . splitOn ","

pathIntersections :: [[(Direction, Int)]] -> Map Point Int
pathIntersections = foldl1' (Map.intersectionWith (+)) . map distances

distances :: [(Direction, Int)] -> Map Point Int
distances steps = Map.fromListWith min (zip (gridDrawLine steps) [1 ..])

-- >>> solvePart1 ["R8,U5,L5,D3", "U7,R6,D4,L4"]
-- 6
solvePart1 :: [String] -> Int
solvePart1 lns =
  let intersections = pathIntersections . map parse $ lns
   in minimum . map (manhattan (0, 0)) . Map.keys $ intersections

solvePart2 :: [String] -> Int
solvePart2 = minimum . pathIntersections . map parse
