import System.Environment
import Text.ParserCombinators.Parsec
import Text.Parsec (endOfLine)
import Data.Either (fromRight)

data Command = Noop | Addx Int deriving (Show, Eq)
data CPUState = CPU { x :: Int, cycles :: Int, wait :: Int, instrs :: [Command] } deriving (Show, Eq)

addX :: GenParser Char st Command
addX = do
    string "addx "
    minus <- optionMaybe (char '-')
    n <- many1 digit
    case minus of
        Just _ -> return . Addx . negate . read $ n
        Nothing -> return . Addx . read $ n

parseCommand :: GenParser Char st Command
parseCommand = (string "noop" >> return Noop) <|> addX

parseCommands :: GenParser Char st [Command]
parseCommands = endBy1 parseCommand endOfLine

waitFor :: [Command] -> Int
waitFor (Noop:xs) = 0
waitFor (Addx _:xs) = 1
waitFor [] = 0

runCommand :: CPUState -> CPUState
runCommand CPU {x=acc, cycles=n, wait=0, instrs=(Noop:xs)} = CPU {x=acc, cycles=n+1, wait=waitFor xs, instrs=xs}
runCommand CPU {x=acc, cycles=n, wait=0, instrs=((Addx a):xs)} = CPU {x=acc + a, cycles=n+1, wait=waitFor xs, instrs=xs}
runCommand CPU {x=acc, cycles=n, wait=w, instrs=xs} = CPU {x=acc, cycles=n+1, wait=w-1, instrs=xs} 

runCommands :: [Int] -> [Command] -> IO [Int]
runCommands interval xs = rcHelper interval CPU {x=1, cycles=1, wait=waitFor xs, instrs=xs }
    where
        rcHelper :: [Int] -> CPUState -> IO [Int]
        rcHelper interval st = do
            -- print ("start", cycles st, x st, head (instrs st))
            let st' = runCommand st
            -- print ("ended", cycles st, x st')
            if abs (x st - cycles st `mod` 40 + 1) < 2 then putChar '#' else putChar '.'
            if null (instrs st') then print (cycles st', x st, cycles st' * x st) >> return [] else 
                if cycles st `elem` interval then 
                    do
                        putStrLn ""
                        -- print (cycles st, x st, head (instrs st), head (instrs st'), cycles st * x st)
                        rest <- rcHelper interval st'
                        return $ (cycles st * x st):rest
                else rcHelper interval st'

main = do
    fnRaw <- getArgs
    let fn = if null fnRaw then "day10.txt" else head fnRaw
    contents <- readFile fn
    let commands = parse parseCommands "idk" contents
    print commands
    results <- runCommands [40, 80, 120, 160, 200, 240] . fromRight [] $ commands 
    print results
    print . sum $ results
