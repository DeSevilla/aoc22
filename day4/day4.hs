import System.IO
import Text.ParserCombinators.Parsec
    ( many, parse, char, digit, GenParser )
import Data.Either ( fromRight )

row :: GenParser Char st ((Int, Int), (Int, Int))
row = do
        r1 <- range
        char ','
        r2 <- range
        return (r1, r2)

range :: GenParser Char st (Int, Int)
range = do 
    s <- digits
    char '-'
    e <- digits
    return (read s, read e)

digits :: GenParser Char st String
digits = many digit

nested :: (Ord a) => ((a, a), (a, a)) -> Bool
nested ((a, b), (c, d)) = leftNested (a, b) (c, d) || leftNested (c, d) (a, b)
    where leftNested (a, b) (c, d) = a >= c && b <= d

overlap :: Ord a => ((a, a), (a, a)) -> Bool
overlap ((a, b), (c, d)) = leftOverlap (a, b) (c, d) || leftOverlap (c, d) (a, b)
    where leftOverlap (a, b) (c, d) = a >= c && a <= d

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main = do
    contents <- readFile "day4.txt"
    print . count overlap . map (fromRight ((-100, 100), (-10000000, 10000)) . parse row "(unknown)") . lines $ contents