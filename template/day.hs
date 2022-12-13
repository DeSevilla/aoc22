import System.Environment

main = do
    fnRaw <- getArgs
    let fn = if null fnRaw then "input.txt" else head fnRaw
    contents <- readFile fn
    print . lines $ contents
 