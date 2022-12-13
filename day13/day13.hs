{-# LANGUAGE InstanceSigs #-}
import System.Environment
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Data.Either (fromRight)

data ListInt = I Int | L [ListInt] deriving (Show, Eq)

prettyPair (x, y) = "(" ++ prettyShow x ++ "," ++ prettyShow y ++ ")"

prettyShow (I x) = show x
prettyShow (L xs) = "[" ++ foldl (\x y -> x ++ prettyShow y ++ ",") "" xs ++ "]"

prettyPrint = putStrLn . prettyShow

parseInt :: GenParser Char st Int
parseInt = do
    x <- many1 digit
    return . read $ x

parseListInt :: GenParser Char st ListInt
parseListInt = (I <$> parseInt) <|> do
    char '['
    xs <- sepBy parseListInt (char ',')
    char ']'
    return . L $ xs

parseLR :: GenParser Char st (ListInt, ListInt)
parseLR = do
    l <- parseListInt
    endOfLine
    r <- parseListInt
    return (l, r)

parseFile :: GenParser Char st [(ListInt, ListInt)]
parseFile = sepBy parseLR (endOfLine >> endOfLine)

instance Ord ListInt where
    (<=) :: ListInt -> ListInt -> Bool
    I x <= I y = x <= y
    I x <= L xs = L [I x] <= L xs
    L xs <= I x = L xs <= L [I x]
    L (x:xs) <= L (y:ys) = if x == y then L xs <= L ys else x <= y
    L [] <= L (x:xs) = True
    L (x:xs) <= L [] = False
    L [] <= L [] = True

rightOrder :: Ord a => (a, a) -> Bool
rightOrder (x, y) = x <= y

findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices f xs = fiHelper f xs 1
    where
        fiHelper f (x:xs) idx = if f x then idx:fiHelper f xs (idx+1) else fiHelper f xs (idx+1)
        fiHelper f [] idx = []

quickSort :: Ord a => [a] -> [a]
quickSort xs@(_:_:_) = let pivot = xs !! (length xs `div` 2) in
    quickSort (filter (pivot >) xs) ++ filter (pivot ==) xs ++ quickSort (filter (pivot <) xs)
quickSort [x] = [x]
quickSort [] = []

flattenPairs :: [(a, a)] -> [a]
flattenPairs ((x, y):pairs) = x:y:flattenPairs pairs
flattenPairs [] = []

main = do
    fnRaw <- getArgs
    let fn = if null fnRaw then "input.txt" else head fnRaw
    contents <- readFile fn
    let result = parse parseFile "unknown" contents
    print result
    let pairs = fromRight [] result
    -- putStrLn . concatMap prettyPair $ pairs
    -- print . map rightOrder $ pairs
    let six = L [L [I 6]]
    let two = L[L[I 2]]
    let allLists = six:two:flattenPairs pairs
    let sorted = quickSort allLists
    print sorted
    print (findIndices (`elem` [two, six]) sorted)

