import System.IO ()
import Control.Monad ()

data RPS = Rock | Paper | Scissors deriving (Show, Eq)

beats :: RPS -> RPS
beats Rock = Paper
beats Paper = Scissors
beats Scissors = Rock

score :: RPS -> Int
score Rock = 1
score Paper = 2
score Scissors = 3

scoreRound :: RPS -> RPS -> Int
scoreRound opp you
  | beats opp == you = 6 + score you
  | you == opp = 3 + score you
  | otherwise = 0 + score you

readRPS :: String -> RPS
readRPS "A" = Rock
readRPS "B" = Paper
readRPS "C" = Scissors
readRPS "X" = Rock
readRPS "Y" = Paper
readRPS "Z" = Scissors

needs :: RPS -> String -> RPS
needs rps "X" = beats $ beats rps
needs rps "Y" = rps
needs rps "Z" = beats rps

readRound1 :: [String] -> Int
readRound1 [opp, you] = scoreRound (readRPS opp) (readRPS you)
readRound1 _ = -1000000000000000000

readRound2 [opp, result] = scoreRound (readRPS opp) (needs (readRPS opp) result)
readRound2 _ = -1000000000000000000

main = do
    contents <- readFile "day2.txt"
    print . sum . map (readRound2 . words) . lines $ contents