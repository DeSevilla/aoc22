import System.Environment
import Text.ParserCombinators.Parsec
import Text.Parsec (endOfLine)
import Data.Either (fromRight)
import Data.Map (Map, fromList, lookup, insertWith, insert, empty, elems, toList)
import Control.Exception (assert)
import Data.List (sort)
import GHC.IO (unsafePerformIO)

data Monkey = Monkey {mId :: Int, oper :: Int -> Int, divisor :: Int, nextTrue :: Int, nextFalse :: Int}
type ItemState = Map Int (Int, [Int])


instance Show Monkey where
    show Monkey {mId=i, oper=w, divisor=d, nextTrue=t, nextFalse=f} =
        "Monkey {mId=" ++ show i ++ ", oper(10)=" ++ show (w 10) ++ ", divisor=" ++ show d
         ++ ", nextTrue=" ++ show t ++ ", nextFalse=" ++ show f ++ "}"

parseInput :: GenParser Char st [(Monkey, [Int])]
parseInput = endBy parseMonkey (try endOfLine >> endOfLine)

parseMonkey :: GenParser Char st (Monkey, [Int])
parseMonkey = do
    string "Monkey "
    i <- parseInt
    char ':'
    endOfLine
    it <- parseItems
    endOfLine
    op <- parseOperation
    endOfLine
    div <- parseDivisor
    endOfLine
    tr <- parseNext
    endOfLine
    fl <- parseNext
    return (Monkey {mId=i, oper=op, divisor=div, nextTrue=tr, nextFalse=fl}, it)


parseString :: String -> a -> GenParser Char st a
parseString s v = string s >> return v

parseInt :: GenParser Char st Int
parseInt = do
    x <- many1 digit
    return . read $ x

parseItems :: GenParser Char st [Int]
parseItems = do
    string "  Starting items: "
    sepBy parseInt (string ", ")

old :: (Int -> Int -> Int) -> GenParser Char st (Int -> Int)
old fn = do
    string "old"
    return $ \x -> fn x x

parseOperation :: GenParser Char st (Int -> Int)
parseOperation = do
    string "  Operation: new = old "
    op <- parseString "*" (*) <|> parseString "+" (+)
    char ' '
    old op <|> op <$> parseInt

parseDivisor :: GenParser Char st Int
parseDivisor = do
    string "  Test: divisible by "
    parseInt

parseNext :: GenParser Char st Int
parseNext = do
    string "    If "
    many letter
    string ": throw to monkey "
    parseInt

mapToFst :: (a -> c) -> [(a, b)] -> [(c, b)]
mapToFst fn ((x, y):pairs) = (fn x, y):mapToFst fn pairs
mapToFst _ [] = []

startCount :: (Int, [Int]) -> (Int, (Int, [Int]))
startCount (m, items) = (m, (0, items))

getNext :: Int -> Monkey -> Int
getNext x m = if x `mod` divisor m == 0 then nextTrue m else nextFalse m

getItems :: Monkey -> ItemState -> [Int]
getItems m state = case Data.Map.lookup (mId m) state of
    Just (count, xs) -> xs
    Nothing -> []

appendToItems :: Int -> Int -> ItemState -> ItemState
appendToItems new item state = case Data.Map.lookup new state of
    Just (count, items) -> insert new (count, item:items) state
    Nothing -> insert new (0, [item]) state

runMonkey :: ItemState -> Monkey -> ItemState
runMonkey state m@Monkey{mId=mid, oper=op} = case Data.Map.lookup mid state of
    Just (count, x:xs) -> let newWorry = op x `mod` (2*3*5*7*11*13*17*19*23) in
        let state' = appendToItems (getNext newWorry m) newWorry (insert mid (count+1, xs) state) in
            runMonkey state' m
    Just (count, []) -> state
    Nothing -> state

runRounds :: ItemState -> [Monkey] -> Int -> IO ItemState
runRounds st ms i
    | i > 0 = do
        if (10000 - i) `mod` 1000 == 0 then print i else putStr ""
        if (10000 - i) `mod` 1000 == 1 then putStrLn . showWorries $ st else putStr ""
        runRounds (foldl runMonkey st ms) ms (i-1) 
    | i <= 0 = putStrLn "DONE" >> return st
runRounds st ms i = putStrLn "WHAT THE FUCK" >> print st >> print ms >> print i >> return st

maxTwo :: (Ord a, Num a) => [a] -> (a, a)
maxTwo (x:y:vs) = maxTwoHelper (sort [x, y]) (y:vs)
    where 
        maxTwoHelper [x', y'] (z:zs) = if z > x' then maxTwoHelper (sort [y', z]) zs else maxTwoHelper [x', y'] zs
        maxTwoHelper [x', y'] [] = (x', y')
        maxTwoHelper _ _ = (negate 100000, negate 100000)
maxTwo _ = (negate 12243243, negate 1894329)

showWorries :: ItemState -> String
showWorries st = foldl (\st (x, (y, z)) -> st ++ "Monkey" ++ show x ++ ":" ++ concatMap ((++) " " . show) z ++ " (" ++ show y ++ ")\n") "" (toList st)

main = do
    fnRaw <- getArgs
    let fn = if null fnRaw then "day11.txt" else head fnRaw
    contents <- readFile fn
    let result = parse parseInput "unknown" contents
    print result
    let completed = fromRight [] result
    let itemState = fromList . map startCount . mapToFst mId $ completed
    let monkeys = map fst completed
    print monkeys
    finalState <- runRounds itemState monkeys 10000
    putStrLn . showWorries $ finalState
    let throwCounts = map fst . elems $ finalState
    print throwCounts
    let (w1, w2) = maxTwo throwCounts
    print (w1, w2)
    print $ w1 * w2