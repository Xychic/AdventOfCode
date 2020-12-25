import System.IO
import Control.Monad
import Data.List

printArray:: (Show a, Eq a) => [a] -> String
printArray (x:xs)
    | null xs = show x
    | otherwise = show x ++ "\n" ++ printArray xs

split:: Eq a => a -> [a] -> [[a]]
split onChar [] = []
split onChar toSplit = x : split onChar (drop 1 y) 
    where (x,y) = span (/= onChar) toSplit

groupQusetions:: [[Char]] -> [[Char]]
groupQusetions remainder
    | null remainder = []
    | otherwise = unwords pass : groupQusetions (drop (length pass + 1) remainder)
        where pass = takeWhile(not . null) remainder 

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let rows = lines contents
    let groups = map (length . foldl intersect ['a'..'z'] . split ' ') (groupQusetions rows)
    print (sum groups)

    hClose handle
