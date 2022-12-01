import System.IO
import Control.Monad

countInc:: (Ord a) => [a] -> Int
countInc (x:[]) = 0
countInc (x:y:xs)
    | x < y = 1 + rest
    | otherwise = rest
    where
        rest = countInc (y:xs)

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let rows = lines contents
    print $ countInc $ map (read :: String -> Int) rows

    hClose handle
