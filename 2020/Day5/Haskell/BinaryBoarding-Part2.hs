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

replaceAll:: (Eq a) => [a] -> [(a, a)] -> [a]
replaceAll toReplace [] = toReplace
replaceAll toReplace ((a,b):xs) = replaceAll (replace toReplace a b) xs

bin2Dec :: [Char] -> Integer
bin2Dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

replacements:: [(Char, Char)]
replacements = [('F', '0'), ('B', '1'), ('L', '0'), ('R', '1')]

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let rows = lines contents
    let seats = map (\row -> bin2Dec (replaceAll row replacements)) rows
    print (head $ filter (\x -> minimum seats < x && x < maximum seats) ([0..2^10] \\ seats))
    hClose handle