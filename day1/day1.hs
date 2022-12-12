import System.IO
import Control.Monad
import Data.List (sort)

sumElves :: Int -> [String] -> [Int]
sumElves i (e@(x:xs):es) = sumElves (i + read e) es
sumElves i ("":es) = i:sumElves 0 es
sumElves i _ = []

topThree :: [Int] -> Int
topThree (x:xs) = topThreeHelper x 0 0 xs
    where topThreeHelper one two three (y:ys) = if y > three then 
                                                        if y > two then 
                                                            if y > one then 
                                                                topThreeHelper y one two ys
                                                            else topThreeHelper one y two ys
                                                        else topThreeHelper one two y ys
                                                    else topThreeHelper one two three ys
          topThreeHelper one two three [] = one + two + three
topThree [] = 0

main = do
    contents <- readFile "day1.txt"
    print . topThree . sumElves 0 . lines $ contents
    