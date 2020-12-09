import System.IO
import Control.Monad

printArray:: (Show a, Eq a) => [a] -> String
printArray (x:xs)
    | null xs = show x
    | otherwise = show x ++ "\n" ++ printArray xs

subsets:: Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs

slice:: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

checkSum:: [Int] -> Int -> Int
checkSum numbers index
    | numbers!!index `elem` sums = checkSum numbers (index+1)
    | otherwise = numbers!!index
    where 
        sums = map sum (subsets 2 (slice (index-25) (index-1) numbers))

findSum:: [Int] -> Int -> Int -> Int -> [Int]
findSum numbers low high target
    | sum run < target = findSum numbers low (high+1) target
    | target < sum run = findSum numbers (low+1) high target
    | otherwise = run
    where
        run = slice low high numbers

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let rows = lines contents
    let numbers = map (read::String->Int) rows
    let target = checkSum numbers 25
    let run = findSum numbers 0 0 target
    print(minimum run + maximum run)
    hClose handle
