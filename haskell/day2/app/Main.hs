module Main where

data RPS = Rock | Paper | Scissors deriving (Eq, Show, Read)

instance Ord RPS where
    Rock     <= Paper    = True
    Rock     <= Scissors = False
    Paper    <= Scissors = True
    Paper    <= Rock     = False
    Scissors <= Rock     = True
    Scissors <= Paper    = False
    _        <= _        = True

data Outcome = Lose | Draw | Win

parseOutcome :: String -> Outcome
parseOutcome "X" = Lose
parseOutcome "Y" = Draw
parseOutcome "Z" = Win
parseOutcome _   = Lose

outcomeScore :: Outcome -> Int
outcomeScore Lose = 0
outcomeScore Draw = 3
outcomeScore Win  = 6

parseSign :: String -> RPS
parseSign "A" = Rock
parseSign "X" = Rock
parseSign "B" = Paper
parseSign "Y" = Paper
parseSign "C" = Scissors
parseSign "Z" = Scissors
parseSign _   = Rock

matchResult :: [RPS] -> Int
matchResult (x : y : []) = if x < y then 6 else if x == y then 3 else 0
matchResult _ = 0

choiceScore :: RPS -> Int
choiceScore Rock     = 1
choiceScore Paper    = 2
choiceScore Scissors = 3

roundScore :: [String] -> Int
roundScore xs = matchResult rpss + choiceScore (last rpss)
    where rpss = map parseSign xs

choiceScore2 :: RPS -> Outcome -> Int
choiceScore2 x Lose = 1 + (choiceScore x - 2) `mod` 3 
choiceScore2 x Draw = 1 + (choiceScore x - 1) `mod` 3 
choiceScore2 x Win  = 1 + (choiceScore x) `mod` 3 

roundScore2 :: [String] -> Int
roundScore2 (x : y : []) = outcomeScore (parseOutcome y) + choiceScore2 (parseSign x) (parseOutcome y)
roundScore2 _            = 0

main :: IO ()
main = do
    content <- readFile "app/input.txt"
    print . sum . map roundScore2 . map words . lines $ content
