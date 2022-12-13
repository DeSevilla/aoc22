import System.Environment
import Data.Char
import Data.Map hiding (null, filter, map, foldl)
import Text.ParserCombinators.Parsec
import Text.Parsec.Char (endOfLine)
import Data.Either (fromRight)
import Data.Maybe ( fromMaybe, isJust )
import Distribution.Compat.Graph (neighbors)
import GHC.Real (infinity)
import Data.List (intercalate)
import GHC.IO (unsafePerformIO)

type Point = (Int, Int)
type Grid = Map Point Int
data Graph = Graph {verts :: Map Point Int, edges :: [(Point, Point)], source :: Point, target :: Point} deriving (Show, Eq)

isVertex :: Point -> Graph -> Bool
isVertex loc graph = member loc (verts graph)

canMove :: Grid -> (Point, Point) -> Bool
canMove grid (p1, p2) = p1 /= p2 && fromMaybe False (do
    -- unsafePerformIO (print (p1, p2) >> return Nothing)
    h1 <- Data.Map.lookup p1 grid
    h2 <- Data.Map.lookup p2 grid
    -- unsafePerformIO (print (p1, h1, p2, h2, h1 - h2, abs (h1 - h2) <= 1) >> return Nothing)
    return $ (h1 - h2) <= 1)

buildGraph :: Grid -> Point -> Point -> Graph
buildGraph grid s t = Graph {verts=grid, edges=buildEdges (keys grid) grid, source=s, target=t}
    where
        buildEdges ((x, y):xs) grid = let ns = [((x, y), (x+xa, y+ya)) | (xa, ya) <- [(1, 0), (-1, 0), (0, 1), (0, -1)]] in
            filter (canMove grid) ns ++ buildEdges xs grid
        buildEdges [] grid = []

buildGrid :: [[Int]] -> Grid
buildGrid = buildGridHelper Data.Map.empty (0, 0)
    where
        buildGridHelper grid (x, y) ((c:cs):rows) = buildGridHelper (insert (x, y) c grid) (x, y+1) (cs:rows)
        buildGridHelper grid (x, y) ([]:rows) = buildGridHelper grid (x+1, 0) rows
        buildGridHelper grid _ [] = grid

parseHeight :: GenParser Char st Int
parseHeight = (char 'E' >> return (-2)) <|> (char 'S' >> return (-1)) <|> fmap ord (oneOf "abcdefghijklmnopqrstuvwxyz")

findSourceTarget :: [[Int]] -> (Point, Point)
findSourceTarget = findSTHelper (0, 0) (0, 0) (0, 0)
    where
        findSTHelper :: Point -> Point -> Point -> [[Int]] -> (Point, Point)
        findSTHelper mS mT (x, y) ((-2:row):rows) = findSTHelper (x, y) mT (x, y+1) (row:rows)
        findSTHelper mS mT (x, y) ((-1:row):rows) = findSTHelper mS (x, y) (x, y+1) (row:rows)
        findSTHelper mS mT (x, y) ((r:row):rows) = findSTHelper mS mT (x, y+1) (row:rows)
        findSTHelper mS mT (x, y) ([]:rows) = findSTHelper mS mT (x+1, 0) rows
        findSTHelper mS mT _ [] = (mS, mT)

parseGrid :: GenParser Char st (Point, Point, Grid)
parseGrid = do
    heights <- sepBy (many parseHeight) endOfLine
    let (s, t) = findSourceTarget heights
    let g = buildGrid heights
    return (s, t, insert s (ord 'z') (insert t (ord 'a') g))

toSnd :: (b -> c) -> (a, b) -> (a, c)
toSnd f (a, b) = (a, f b)

infinite :: Int
infinite = 10000000000000000

insertSmaller :: (Ord k) => Int -> Map k Int -> k -> Map k Int
insertSmaller x m key = insert key (min x $ fromMaybe infinite $ Data.Map.lookup key m) m

minByMap :: Map Point Int -> [Point] -> Point
minByMap m [] = (-1, -1)
minByMap m (x:xs) = mbmHelper m (x:xs) infinite x
    where
        mbmHelper m (x:xs) curMin minIdx = let val = fromMaybe infinite (Data.Map.lookup x m) in
            if val < curMin
            then mbmHelper m xs val x
            else mbmHelper m xs curMin minIdx
        mbmHelper _ [] _ minIdx  = minIdx

getPath :: Map Point [Point] -> Map Point Int -> Point -> Point -> [Point]
getPath neighbors distances source target
    | source == target = []
    | otherwise = fromMaybe [] (do
            ns <- Data.Map.lookup target neighbors
            let next = minByMap distances ns
            unsafePerformIO (print ("next", next) >> return Nothing)
            return (next:getPath neighbors distances source next))

takeDefault :: a -> Int -> [a] -> [a]
takeDefault c 0 _ = []
takeDefault c n (x:xs) = x:takeDefault c (n-1) xs
takeDefault c n [] = c:takeDefault c (n-1) []

showMap :: Show b => Map (Int, Int) b -> String
showMap board = do
        intercalate "\n" [concat [
            case Data.Map.lookup (x, y) board of
                (Just b) -> takeDefault ' ' 3 (show b)
                Nothing -> ". "
            | y <- [minY..maxY]] | x <- [minX..maxX]]
    where maxX = maximum $ map fst $ keys board
          maxY = maximum $ map snd $ keys board
          minX = minimum $ map fst $ keys board
          minY = minimum $ map snd $ keys board

minuskeys :: Ord k => Map k a1 -> Map k a2 -> [k]
minuskeys xs ys = filter (not . flip member ys) (keys xs)

buildNeighbors :: Ord k => [(k, a)] -> Map k [a]
buildNeighbors edges = fromListWith (++) (map (toSnd return) edges)

buildDistances :: Graph -> IO (Map Point Int)
buildDistances Graph {verts=vs, edges=es, source=s, target=t} = do
    let neighbors = buildNeighbors es
    putStrLn "got neighbors"
    -- print neighbors
    bdHelper vs Data.Map.empty neighbors (Data.Map.singleton s 0) s t
    where
        bdHelper :: Map Point Int -> Map Point Bool -> Map Point [Point] -> Map Point Int -> Point -> Point -> IO (Map Point Int)
        bdHelper allNodes visited neighbors distances current targ = case
            fromMaybe ([], -100) (do
                ns <- Data.Map.lookup current neighbors
                d <- Data.Map.lookup current distances
                return (filter (not . flip member visited) ns, d)) of
                    (ns', d') -> (do
                        -- print ("current", current)
                        -- print (ns, d)
                        let visited' = insert current True visited
                        let dists' = foldl (insertSmaller (d'+1)) distances ns'
                        -- if (length (keys dists') - length (keys visited')) < 10 then print (length (keys dists') - length (keys visited')) else putStr ""
                        if keys visited' == keys dists' then print "problem" >> writeFile "distancesbad.txt" (showMap dists') else putStr ""
                        if all (`member` dists') ns' then putStr "" else putStrLn "wtf..." >> print ns'
                        let unvisited = filter (not . flip member visited') (keys allNodes)
                        -- if all (`member` dists') (keys visited') then putStr "" else putStrLn "wtf2..."
                        let next = minByMap dists' unvisited
                        if next `member` dists' then putStr "" else print ("wtf...", next, visited' `minuskeys` dists')

                        -- print ("dists", dists')
                        -- print ("next", minByMap dists' unvisited)
                        -- getLine
                        if any (\x -> Data.Map.lookup x allNodes == Just 97) (keys visited')
                            then writeFile "visited.txt" (showMap visited') >> return dists' 
                            else bdHelper allNodes visited' neighbors dists' next targ)



shortestPath :: Graph -> IO [Point]
shortestPath graph = do    -- print distances
    let neighbors = buildNeighbors (edges graph)
    distances <- buildDistances graph
    putStrLn "got distances"
    writeFile "distances.txt" . showMap $ distances
    return . reverse $ getPath neighbors distances (source graph) (target graph)

main = do
    fnRaw <- getArgs
    let fn = if null fnRaw then "input.txt" else head fnRaw
    contents <- readFile fn
    let (s, t, grid) = fromRight ((-1, -1), (-1, -1), Data.Map.empty) . parse parseGrid "unknown" $ contents
    writeFile "grid.txt" . showMap $ insert s 0 (insert t 0 grid)
    let graph = buildGraph grid s t
    print graph
    print (source graph, target graph)
    writeFile "verts.txt" . showMap . verts $ graph
    distances <- buildDistances graph
    writeFile "distances.txt" . showMap $ distances
    let as = map fst (filter (\(a, b) -> b == 97) (toList (verts graph)))
    print ("as", as)
    let trailStart = minByMap distances as
    print $ Data.Map.lookup trailStart distances
 