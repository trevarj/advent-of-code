module Day2 (solve1, solve2) where

data Hand = Rock | Paper | Scissors deriving (Show, Eq)

class FromStr a where
  fromStr :: String -> a

instance FromStr Hand where
  fromStr op
    | op == "A" || op == "X" = Rock
    | op == "B" || op == "Y" = Paper
    | op == "C" || op == "Z" = Scissors
  fromStr _ = error "variant not found"

instance Ord Hand where
  compare Rock Rock = EQ
  compare Rock Paper = LT
  compare Rock Scissors = GT
  compare Paper Paper = EQ
  compare Paper Rock = GT
  compare Paper Scissors = LT
  compare Scissors Scissors = EQ
  compare Scissors Rock = LT
  compare Scissors Paper = GT

handScore :: Hand -> Int
handScore Rock = 1
handScore Paper = 2
handScore Scissors = 3

wordsToHand :: [String] -> (Hand, Hand)
wordsToHand [op, me] = (fromStr op, fromStr me)
wordsToHand _ = error "expected opponent hand and our hand"

{-
The score for a single round is the score for the shape you selected (1 for Rock, 2 for Paper, and 3 for Scissors)
plus the score for the outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won).
-}
play :: Hand -> Hand -> Int
play op me =
  let selected = handScore me
   in selected + case compare op me of
        LT -> 6
        GT -> 0
        EQ -> 3

solve1 :: String -> IO String
solve1 input = pure . show . sum $ uncurry play . wordsToHand . words <$> lines input

wordsToOutcome :: [String] -> (Hand, Ordering)
wordsToOutcome [op, outcome] =
  let opHand = fromStr op
   in ( opHand,
        case outcome of
          "X" -> LT
          "Y" -> EQ
          "Z" -> GT
          _ -> error "expected X, Y or Z"
      )
wordsToOutcome _ = error "unexpected words"

-- Solve for our move based on what the outcome should be
moveSolver :: Hand -> Ordering -> Hand
moveSolver Rock LT = Scissors
moveSolver Rock GT = Paper
moveSolver Rock EQ = Rock
moveSolver Paper LT = Rock
moveSolver Paper GT = Scissors
moveSolver Paper EQ = Paper
moveSolver Scissors LT = Paper
moveSolver Scissors GT = Rock
moveSolver Scissors EQ = Scissors

solve2 :: String -> IO String
solve2 input =
  pure . show . sum $
    ( (\(opHand, outcome) -> play opHand (moveSolver opHand outcome))
        . wordsToOutcome
        . words
        <$> lines input
    )
