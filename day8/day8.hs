import Data.Map ( Map, insert, fromList, empty, keys, member, insertWith, lookup, elems, toList )
import Data.List (transpose, maximum, intercalate, intersperse)
import Data.Maybe (fromMaybe)
import System.Environment

type Point = (Int, Int)

updateScenic :: Map Point Int -> [(Point, Int)] -> (Point, Int) -> IO (Map Point Int, [(Point, Int)])
updateScenic scenicness ((loc, height):hs) cur@(curLoc, curHeight)
    | height > curHeight = do
        -- print (True, loc, height)
        (newScenicness, newHs) <- updateScenic (insertWith (+) loc 1 scenicness) hs cur
        return (newScenicness, (loc, height):newHs)
    | otherwise = do
        -- print (False, loc, height) 
        updateScenic (insertWith (+) loc 1 scenicness) hs cur
updateScenic scenicness [] cur = return (scenicness, [cur])

scanScenic :: Map Point Int -> (Point -> Point) -> Point -> [(Point, Int)] -> [Int] -> IO (Map Point Int)
scanScenic scenicness step loc@(x, y) heights (tree:trees) = do
    (newScenicness, newHeights) <- updateScenic scenicness heights (loc, tree)
    scanScenic newScenicness step (step loc) newHeights trees
scanScenic scenicness _ _ _ [] = return scenicness

scanAllScenic :: Map Point Int -> Point -> (Point -> Point) -> (Point -> Point) -> [[Int]] -> IO (Map Point Int)
scanAllScenic scenicness start stepRow step (row:rows) = do
    m' <- scanScenic scenicness step start [] row
    -- print m'
    scanAllScenic m' (stepRow start) stepRow step rows
scanAllScenic scenicness _ _ _ [] = return scenicness


scanLeft :: Map Point Int -> Point -> (Point -> Point) -> Int -> Int -> [Int] -> IO (Map Point Int)
scanLeft m c@(x, y) step h s (v:vs)
    | v > h = scanLeft (insertWith (+) (x, y) s m) (step c) step v (s+1) vs 
    | otherwise = scanLeft m (step c) step h 0 vs
scanLeft m _ _ _ _ [] = return m

scanAll :: Map Point Int -> Point -> (Point -> Point) -> (Point -> Point) -> [[Int]] -> IO (Map Point Int)
scanAll m start stepRow step (row:rows) = do
    -- putStr $ "Scanning from: " ++ show start ++ " direction:" ++ show (step (0, 0)) ++ ";"
    m' <- scanLeft m start step (negate 1) 0 row
    -- print $ keys m'
    scanAll m' (stepRow start) stepRow step rows
scanAll m _ _ _ [] = return m

printGrid ((x:xs):ls) = putStr (" " ++ show x) >> printGrid (xs:ls)
printGrid ([]:ls) = putStrLn "" >> printGrid ls
printGrid [] = putStr ""

printMap :: Show b => Map (Int, Int) b -> IO ()
printMap board = do
        print (maxX, maxY, minX, minY)
        putStrLn $ intercalate "\n" [[
            case Data.Map.lookup (x, y) board of 
                (Just b) -> head (show b)
                Nothing -> '.' 
            | y <- [minY..maxY]] | x <- [minX..maxX]]
    where maxX = maximum $ map fst $ keys board
          maxY = maximum $ map snd $ keys board
          minX = minimum $ map fst $ keys board
          minY = minimum $ map snd $ keys board

mulKey :: (Ord k, Num a) => Map k a -> Map k a -> Map k a -> k -> Map k a
mulKey m1 m2 out k = case do
    x <- Data.Map.lookup k m1
    y <- Data.Map.lookup k m2
    return $ x * y
    of
        Just z -> Data.Map.insert k z out
        Nothing -> out

mapMul :: Ord k => Map k Int -> Map k Int -> Map k Int
mapMul m1 m2 = foldl (mulKey m1 m2) Data.Map.empty . keys $ m1

ctos c = [c]

main = do
    fnRaw <- getArgs
    let fn = if null fnRaw then "day8.txt" else head fnRaw
    contents <- readFile fn
    let grid = map (map (read . ctos)) . lines $ contents
    -- printGrid grid
    m <- scanAllScenic Data.Map.empty (0, 0) (\(x, y) -> (x+1, y)) (\(x, y) -> (x, y+1)) grid
    -- printGrid $ map reverse grid
    m' <- scanAllScenic Data.Map.empty (0, length (head grid) - 1) (\(x, y) -> (x+1, y)) (\(x, y) -> (x, y-1)) . map reverse $ grid
    -- printGrid $ transpose grid
    m'' <- scanAllScenic Data.Map.empty (0, 0) (\(x, y) -> (x, y+1)) (\(x, y) -> (x+1, y)) . transpose $ grid
    -- printGrid $ map reverse $ transpose grid
    m''' <- scanAllScenic Data.Map.empty (length grid - 1, 0) (\(x, y) -> (x, y+1)) (\(x, y) -> (x-1, y)) . map reverse . transpose $ grid
    let finalM = m `mapMul` m' `mapMul` m'' `mapMul` m'''
    printMap finalM
    print . length $ finalM
    print . maximum . elems $ finalM