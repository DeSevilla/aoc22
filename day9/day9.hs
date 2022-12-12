import System.Environment
import Text.ParserCombinators.Parsec
import Prelude hiding (Left, Right)
import Data.Map (Map, insert, empty, lookup, keys, fromList)
import Data.Either (fromRight)
import Data.List (intercalate, intersperse)

data Command = Left | Right | Up | Down deriving (Show, Eq)
type Point = (Int, Int)
data Rope = Rope { h :: Point, t :: Point }
type LongRope = [Point]

singleChar :: Char -> a -> GenParser Char st a
singleChar c result = char c >> return result

parseCommand :: GenParser Char st Command
parseCommand = singleChar 'L' Left <|> singleChar 'R' Right <|> singleChar 'U' Up <|> singleChar 'D' Down

parseLine :: GenParser Char st (Command, Int)
parseLine = do
    comm <- parseCommand
    char ' '
    num <- many1 digit
    return (comm, read num)

stepTowards :: Int -> Int -> Int
stepTowards n m
    | n - m > 0 = m + 1
    | m - n > 0 = m - 1
    | otherwise = m

updateTail :: Rope -> Rope
updateTail r@Rope { h=(xh, yh), t=(xt, yt) }
    | (abs (xh - xt) <= 1) && (abs (yh - yt) <= 1) = r
    | xh == xt = Rope { h=(xh, yh), t=(xt, stepTowards yh yt) }
    | yh == yt = Rope { h=(xh, yh), t=(stepTowards xh xt, yt) }
    | otherwise = Rope { h=(xh, yh), t=(stepTowards xh xt, stepTowards yh yt) }

runCommand :: Rope -> Command -> Rope
runCommand Rope {h=(xh, yh), t=(xt, yt)} Left = updateTail Rope {h=(xh, yh-1), t=(xt, yt)}
runCommand Rope {h=(xh, yh), t=(xt, yt)} Right = updateTail Rope {h=(xh, yh+1), t=(xt, yt)}
runCommand Rope {h=(xh, yh), t=(xt, yt)} Up = updateTail Rope {h=(xh-1, yh), t=(xt, yt)}
runCommand Rope {h=(xh, yh), t=(xt, yt)} Down = updateTail Rope {h=(xh+1, yh), t=(xt, yt)}

getUpdated :: Point -> Point -> Point
getUpdated h@(xh, yh) t@(xt, yt)
    | (abs (xh - xt) <= 1) && (abs (yh - yt) <= 1) = t
    | xh == xt = (xt, stepTowards yh yt)
    | yh == yt = (stepTowards xh xt, yt)
    | otherwise = (stepTowards xh xt, stepTowards yh yt)

updateLong :: LongRope -> LongRope
updateLong (h:t:ks) = h:updateLong (getUpdated h t:ks)
updateLong ks = ks



runLong :: LongRope -> Command -> LongRope
runLong ((xh, yh):ks) Left = updateLong ((xh, yh-1):ks)
runLong ((xh, yh):ks) Right = updateLong ((xh, yh+1):ks)
runLong ((xh, yh):ks) Up = updateLong ((xh-1, yh):ks)
runLong ((xh, yh):ks) Down = updateLong ((xh+1, yh):ks)
runLong ks _ = ks

trackLong :: (LongRope, Map Point Command) -> (Command, Int) -> (LongRope, Map Point Command)
trackLong (rope, map) (comm, times)
    | times > 0 = let rope' = runLong rope comm in
        trackLong (rope', Data.Map.insert (last rope') comm map) (comm, times - 1)
    | otherwise = (rope, Data.Map.insert(last rope) comm map)


trackTail :: (Rope, Map Point Command) -> (Command, Int) -> (Rope, Map Point Command)
trackTail (rope, map)  (comm, times)
    | times > 0 = let rope' = runCommand rope comm in
        trackTail (rope', Data.Map.insert (t rope') comm map) (comm, times - 1)
    | otherwise = (rope, Data.Map.insert (t rope) comm map)

showMap :: Show b => Map (Int, Int) b -> String
showMap board = do
        intercalate "\n" [[
            case Data.Map.lookup (x, y) board of
                (Just b) -> head (show b)
                Nothing -> '.'
            | y <- [minY..maxY]] | x <- [minX..maxX]]
    where maxX = maximum $ map fst $ keys board
          maxY = maximum $ map snd $ keys board
          minX = minimum $ map fst $ keys board
          minY = minimum $ map snd $ keys board

main = do
    fnRaw <- getArgs
    let fn = if null fnRaw then "day9.txt" else head fnRaw
    contents <- readFile fn
    let commands = map (fromRight (Left, 0) . parse parseLine "(unknown)") . lines $ contents
    -- print commands
    -- let (rope, locs) = foldl trackTail (Rope {h=(0, 0), t=(0, 0)}, fromList [((0, 0), Left)]) commands
    let (rope, locs) = foldl trackLong (replicate 10 (0, 0), fromList [((0, 0), Left)]) commands
    -- print . keys $ locs
    writeFile "output.txt" . showMap $ locs
    print . length . keys $ locs
