module Day4 (solvePart1, solvePart2) where

import Data.List (foldl', group)
import Data.List.Split (splitOn)

-- >>> validPasswordPart1 "111123"
-- True
-- >>> validPasswordPart1 "123789"
-- False
validPasswordPart1 :: String -> Bool
validPasswordPart1 pass =
  let (dub, asc, _) =
        foldl'
          (\(d, a, prev) ch -> (d || ch == prev, a && ch >= prev, ch))
          (False, True, '0')
          pass
   in dub && asc

-- >>> validPasswordPart2 "123444"
-- False
validPasswordPart2 :: String -> Bool
validPasswordPart2 pass =
  let (dub, asc, _) =
        foldl'
          (\(d, a, prev) chs -> (d || (length chs == 2), a && (head chs >= head prev), chs))
          (False, True, "0")
          (group pass)
   in dub && asc

parse :: String -> (Int, Int)
parse s = case splitOn "-" s of
  [l, h] -> (read l, read h)
  _ -> error "unexpected range"

solve :: (String -> Bool) -> [String] -> Int
solve validator lns =
  length $
    [ i
    | let (l, h) = parse . head $ lns
    , i <- [l .. h]
    , validator . show $ i
    ]

-- >>> solvePart1 ["128392-643281"]
-- 2050
solvePart1 :: [String] -> Int
solvePart1 = solve validPasswordPart1

-- >>> solvePart2 ["128392-643281"]
-- 514494
solvePart2 :: [String] -> Int
solvePart2 = solve validPasswordPart2
