import Text.ParserCombinators.Parsec
import Data.Map ( Map, fromList, lookup, insert, adjust, map, foldr )
import Data.List ( transpose )

type Box = Char

parseWhole :: GenParser Char st (Map Int [Box])
parseWhole = do
    sm <- stacks
    newLine
    newLine
    is <- instructions
    return $ run9001s is sm

parseNotRun :: GenParser Char st (Map Int [Box])
parseNotRun = do
    sm <- stacks
    newLine
    newLine
    is <- instructions
    return sm

parseInstructions :: GenParser Char st [(Int, Int, Int)]
parseInstructions = do
    stacks
    newLine
    newLine
    instructions

runInstructions :: [(Int, Int, Int)] -> Map Int [Box] -> Map Int [Box]
runInstructions is stackMap = foldl (flip runInstruction) stackMap is

run9001s :: [(Int, Int, Int)] -> Map Int [Box] -> Map Int [Box]
run9001s is stackMap = foldl (flip run9001) stackMap is

runInstruction :: (Int, Int, Int) -> Map Int [Box] -> Map Int [Box]
runInstruction (0, _, _) stackMap = stackMap
runInstruction (a, b, c) stackMap = case Data.Map.lookup b stackMap of
    Just (x:xs) -> runInstruction (a - 1, b, c) $ adjust (x:) c $ Data.Map.insert b xs stackMap
    Just [] -> stackMap
    Nothing -> stackMap

run9001 :: (Int, Int, Int) -> Map Int [Box] -> Map Int [Box]
run9001 (0, _, _) stackMap = stackMap
run9001 (a, b, c) stackMap = case Data.Map.lookup b stackMap of
    Just (x:xs) -> adjust (x:) c $ run9001 (a - 1, b, c) $ Data.Map.insert b xs stackMap
    Just [] -> stackMap
    Nothing -> stackMap

instructions :: GenParser Char st [(Int, Int, Int)]
instructions = endBy1 instruction newLine

instruction :: GenParser Char st (Int, Int, Int)
instruction = do
    string "move "
    m <- many1 digit
    string " from "
    f <- many1 digit
    string " to "
    t <- many1 digit
    return (read m, read f, read t)

assign :: [a] -> [b] -> [(a, b)]
assign (n:names) (c:cols) = (n, c):assign names cols
assign _ _ = []

orientStacks :: [[Box]] -> [[Box]]
orientStacks = Prelude.map (filter (' ' /=)) . transpose

stacks :: GenParser Char st (Map Int [Box])
stacks = do
    b <- boxes
    c <- columns
    return $ fromList(assign c (orientStacks b))

columns :: GenParser Char st [Int]
columns = sepBy column $ char ' '

column :: GenParser Char st Int
column = do
    char ' '
    s <- many1 digit
    char ' '
    return $ read s

boxes :: GenParser Char st [[Box]]
boxes = endBy boxLine newLine

-- boxes :: GenParser Char st [[Box]]
-- boxes = many boxLine

boxLine :: GenParser Char st [Box]
boxLine = try $ sepBy box $ char ' '

box :: GenParser Char st Box
box = fullBox <|> emptyBox

emptyBox :: GenParser Char st Box
emptyBox = do
    char ' '
    char ' '
    char ' '
    return ' '

fullBox :: GenParser Char st Box
fullBox = do
    char '['
    s <- letter
    char ']'
    return s

newLine = newline -- string "\n\r"

getHeads :: Map k [a] -> [a]
getHeads = Data.Map.foldr (:) [] . Data.Map.map head

main = do
    contents <- readFile "day5.txt"
    -- print . parse parseInstructions "[unknown]" $ contents
    print . fmap getHeads . parse parseNotRun "unknown" $ contents
    print . fmap getHeads . parse parseWhole "(unknown)" $ contents
