import System.IO
import Control.Monad
import Data.List

printArray:: (Show a, Eq a) => [a] -> String
printArray (x:xs)
    | null xs = show x
    | otherwise = show x ++ "\n" ++ printArray xs

subsets:: Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs

split:: Eq a => a -> [a] -> [[a]]
split onChar [] = []
split onChar toSplit = x : split onChar (drop 1 y) 
    where (x,y) = span (/= onChar) toSplit

replace:: (Eq a) => [a] -> a -> a -> [a]
replace [] _ _ = []
replace (x:xs) a b 
    | x == a = b : replace xs a b
    | otherwise = x : replace xs a b

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
