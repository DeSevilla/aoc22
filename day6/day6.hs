import Data.List (nub)

allUnique :: Eq a => [a] -> Bool
allUnique x = x == nub x

scan :: Eq a => [a] -> Maybe Int
scan xs = scanHelper 0 (take 14 xs) xs
    where 
        scanHelper :: Eq a => Int -> [a] -> [a] -> Maybe Int
        scanHelper i cur@(y:ys) (x:xs) = if allUnique cur then Just i else scanHelper (i + 1) (ys ++ [x]) xs
        scanHelper i cur@(y:ys) [] = if allUnique cur then Just i else Nothing
        scanHelper _ _ _ = Nothing

main = do
    contents <- readFile "day6.txt"
    print . scan $ contents