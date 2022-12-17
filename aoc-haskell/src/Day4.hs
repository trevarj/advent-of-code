module Day4 (solve1) where

type Ranges = ((Int, Int), (Int, Int))

stupidHelper :: ((String, String), (String, String)) -> Ranges
stupidHelper ((a, b), (c, d)) = ((read a, read b), (read c, read d))

splitOn' :: Char -> String -> String -> (String, String)
splitOn' _ left [] = (left, [])
splitOn' delim left (x : xs)
  | delim == x = (left, xs)
  | otherwise = splitOn' delim (left ++ [x]) xs

splitOn :: Char -> String -> (String, String)
splitOn delim = splitOn' delim []

parseLine :: String -> Ranges
parseLine line = let (left, right) = splitOn ',' line in stupidHelper (splitOn '-' left, splitOn '-' right)

fullyContained :: Ranges -> Bool
fullyContained ((a1, a2), (b1, b2))
  | a || b = True
  | otherwise = False
  where
    a = a1 >= b1 && a2 <= b2
    b = b1 >= a1 && b2 <= a2

solve1 :: String -> String
solve1 input =
  show $
    length . filter (== True) $
      fullyContained . parseLine <$> lines input
