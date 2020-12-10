import System.IO
import Control.Monad
import Data.List(sort, nub)

printArray:: (Show a, Eq a) => [a] -> String
printArray (x:xs)
    | null xs = show x
    | otherwise = show x ++ "\n" ++ printArray xs

countAdapters:: [Int] -> [Int]
countAdapters (x:xs)
    | null xs = [1]
    | otherwise = sum (zipWith const countTail (takeWhile (<= x+3) xs)) : countTail
    where countTail = countAdapters xs  

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let rows = lines contents
    let ad = 0 : sort (map (read::String->Int) rows)
    let adapters = ad ++ [maximum ad + 3]
    print (head $ countAdapters adapters)
    hClose handle
