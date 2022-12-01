import System.IO
import Control.Monad

countInc:: (Ord a) => [a] -> Int
countInc (x:_:_:[]) = 0
countInc (w:x:y:z:ws)
    | w < z = 1 + rest
    | otherwise = rest
    where
        rest = countInc (x:y:z:ws)

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let rows = lines contents
    print $ countInc $ map (read :: String -> Int) rows

    hClose handle
