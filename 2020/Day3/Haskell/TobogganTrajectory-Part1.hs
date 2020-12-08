import System.IO
import Control.Monad

printArray:: (Show a, Eq a) => [a] -> String
printArray (x:xs)
    | null xs = show x
    | otherwise = show x ++ "\n" ++ printArray xs

findTree:: [[Char]] -> Int -> Int -> Int
findTree world dx dy = findTree_ world dx dy 0 0 0
    where 
        findTree_:: [[Char]] -> Int -> Int -> Int -> Int -> Int -> Int
        findTree_ world dx dy x y total 
            | length world <= y = total
            | (world!!y!!x) == '#' = findTree_ world dx dy (mod (x+dx) (length (head world))) (y+dy) (total+1)
            | otherwise = findTree_ world dx dy (mod (x+dx) (length (head world))) (y+dy) total

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let world = lines contents
    print (findTree world 3 1)

    hClose handle
