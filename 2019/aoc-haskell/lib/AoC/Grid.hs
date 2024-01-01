module AoC.Grid (
  Point,
  Direction (Up, Down, Left, Right),
  Grid,
  directionPoint,
  directionScalarMul,
  gridDrawLine,
  manhattan,
) where

import Data.Vector (Vector)
import Prelude hiding (Left, Right)

newtype Grid h w = Vector (Vector Int) deriving (Eq, Ord, Read, Show)

type Point = (Int, Int)

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving (Eq, Read, Show)

directionPoint :: Direction -> Point
directionPoint Up = (-1, 0)
directionPoint Down = (1, 0)
directionPoint Left = (0, -1)
directionPoint Right = (0, 1)

directionScalarMul :: Direction -> Int -> Point
directionScalarMul Up c = (-c, 0)
directionScalarMul Down c = (c, 0)
directionScalarMul Left c = (0, -c)
directionScalarMul Right c = (0, c)

addPoints :: Point -> Point -> Point
addPoints (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

-- >>> gridDrawLine [(Right,8),(Up,5)]
-- [(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(0,8),(-1,8),(-2,8),(-3,8),(-4,8),(-5,8)]
gridDrawLine :: [(Direction, Int)] -> [Point {- points -}]
gridDrawLine =
  scanl1 addPoints
    . concatMap (\(d, n) -> replicate n (directionPoint d))

manhattan :: Point -> Point -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
