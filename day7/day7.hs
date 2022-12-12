import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Data.List (intercalate, sort)
import Control.Monad

data File = Regular String Int | Dir String [File] deriving (Show, Eq)
data Command = CD String | LS [File] deriving (Show, Eq)
data CommandError = LSFail String | CDFail String | OtherFail String
data FileSystem = FileSys { root :: File, cwd :: [String] } deriving (Show, Eq)

prettify :: Command -> String
prettify (CD s) = "cd " ++ s
prettify (LS xs) = "ls: " ++ unwords (map nameOf xs)

totalSize :: File -> Int
totalSize (Regular _ i) = i
totalSize (Dir _ kids) = (sum . map totalSize) kids

sumLimited :: Int -> File -> Int
sumLimited max (Regular _ i) = 0
sumLimited max f@(Dir _ xs) = (sum . map (sumLimited max)) xs + if totalSize f <= max then totalSize f else 0

bigEnoughDirs :: Int -> File -> [Int]
bigEnoughDirs max f@(Dir name kids) = ([totalSize f | totalSize f >= max]) ++ concatMap (bigEnoughDirs max) kids
bigEnoughDirs _ _ = []

allSizes :: File -> [Int]
allSizes f@(Dir _ kids) = totalSize f:concatMap allSizes kids
allSizes _ = []

nameOf :: File -> String
nameOf (Regular n _) = n
nameOf (Dir n _) = n

findName :: String -> [File] -> Maybe File
findName y (x:xs) = if nameOf x == y then Just x else findName y xs
findName y [] = Nothing

setKids :: [String] -> [File] -> File -> File
setKids [step] newKids old@(Dir n kids) = if n == step then Dir n newKids else old
setKids (step:path) newKids old@(Dir n xs) = if n == step then Dir n (map (setKids path newKids) xs) else old
setKids _ _ old = old

removeLast :: [a] -> [a]
removeLast [x] = []
removeLast (x:xs) = x:removeLast xs
removeLast [] = []

process :: FileSystem -> Command -> FileSystem
process FileSys{root=f, cwd=path} (CD "..") = FileSys {root=f, cwd=removeLast path}
process FileSys{root=f, cwd=path} (CD name) = FileSys {root=f, cwd=path ++ [name]}
process FileSys{root=f, cwd=path} (LS ys) = FileSys{root=setKids path ys f, cwd=path}

commands :: GenParser Char st [Command]
commands = many command

command :: GenParser Char st Command
command = do
    string "$ "
    cd <|> ls

cd :: GenParser Char st Command
cd = do
    string "cd "
    dir <- many1 $ noneOf "\r\n"
    endOfLine
    return $ CD dir

ls :: GenParser Char st Command
ls = do
    string "ls"
    endOfLine
    fs <- endBy (regular <|> dir) endOfLine
    return $ LS fs

regular :: GenParser Char st File
regular = do
    size <- many1 digit
    char ' '
    name <- many1 $ noneOf "\r\n"
    return $ Regular name $ read size

dir :: GenParser Char st File
dir = do
    string "dir "
    name <- many1 $ noneOf "\r\n"
    return $ Dir name []

printNProc f c = do
    putStrLn $ prettify c
    let x = process f c
    if x == f then putStrLn "command did nothing" else putStrLn "changed file system"
    print x
    return x

main = do
    contents <- readFile "day7.txt"
    print . fmap length . parse commands "(unknown)" $ contents
    putStrLn "first 5 commands"
    print . fmap (map prettify . take 5) . parse commands "(unknown)" $ contents
    putStrLn "real resulting filesystem"
    print . fmap (root . foldl process FileSys{root=Dir "/" [], cwd=[]}) . parse commands "unknown" $ contents
    let fs = FileSys {root = Dir "/" [Dir "gts" [Regular "grwwbrgz.wft" 846,Regular "mrnhn.psz" 72000,Regular "qvnbd.dqs" 155241,Regular "tndtmwfv" 6655],Regular "jvdqjhr.jvp" 68377,Dir "lwhbw" [],Regular "nqth.gcn" 228884,Dir "pcqjnl" [],Regular "ppwv.zsh" 94844,Regular "rqpw" 97889,Dir "sqhw" [],Dir "vllgn" [],Dir "wdtm" [],Dir "ztfdwp" []], cwd = ["lwhbw"]}
    fs' <- printNProc fs (CD "/")
    fs'' <- printNProc fs' (LS [Dir "hi" [], Regular "hello" 1000])
    print fs''
    print "complicated..."
    case parse commands "unknown" contents of
        Right comms -> foldM printNProc FileSys{root=Dir "/" [], cwd=[]} (take 10 comms) >>= print
        Left error -> print FileSys{root=Dir "/" [], cwd=["errorrrr"]}
    print . fmap (sumLimited 100000 . root . foldl process FileSys{root=Dir "/" [], cwd=[]}) . parse commands "unknown" $ contents
    print . fmap (bigEnoughDirs 8000000 . root . foldl process FileSys{root=Dir "/" [], cwd=[]}) . parse commands "unknown" $ contents
    let fs = case (fmap (foldl process FileSys{root=Dir "/" [], cwd=[]}) . parse commands "unknown") contents of
            Right x -> x
            Left err -> FileSys{root=Dir "err" [], cwd=["error"]}
    let sizes = allSizes . root $ fs
    print $ 70000000 - totalSize (root fs)
    print $ foldl min 1000000000000000 . filter (30000000 - (70000000 - totalSize (root fs)) <=) $ sizes
