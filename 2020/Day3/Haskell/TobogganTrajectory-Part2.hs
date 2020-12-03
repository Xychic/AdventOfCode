import System.IO
import Control.Monad

printArray:: (Show a, Eq a) => [a] -> String
printArray (x:xs)
    | xs == [] = show x
    | otherwise = (show x) ++ "\n" ++ printArray xs

subsets:: Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs

findTree_:: [[Char]] -> Int -> Int -> Int -> Int -> Int -> Int
findTree_ world dx dy x y total 
    | y >= (length world) = total
    | (world!!y!!x) == '#' = findTree_ world dx dy (mod (x+dx) (length (world!!0))) (y+dy) (total+1)
    | otherwise = findTree_ world dx dy (mod (x+dx) (length (world!!0))) (y+dy) total

findTree:: [[Char]] -> Int -> Int -> Int
findTree world dx dy = findTree_ world dx dy 0 0 0

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let world = (lines contents)
    print (foldl (*) 1 (map (\(dx, dy) -> findTree world dx dy) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]))

    hClose handle
