module Day2 (solve1) where

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
play :: (Hand, Hand) -> Int
play (op, me) =
    let selected = handScore me
     in selected + case compare op me of
            LT -> 6
            GT -> 0
            EQ -> 3

solve1 :: String -> String
solve1 input = show . sum $ play . wordsToHand . words <$> lines input
