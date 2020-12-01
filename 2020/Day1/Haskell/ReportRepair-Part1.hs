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

checkSum:: (Num a, Eq a) => [a] -> a -> Bool
checkSum arr total = foldl (+) 0 arr == total

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let lines = (words contents)
    print (foldl (*) 1 ((filter (\n -> checkSum n 2020) (subsets 2 (map (read::String->Int) lines)))!!0))
    hClose handle