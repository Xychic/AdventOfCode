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

split:: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

replace:: (Eq a) => [a] -> a -> a -> [a]
replace [] _ _ = []
replace (x:xs) a b 
    | x == a = [b] ++ replace xs a b
    | otherwise = [x] ++ replace xs a b

parseData:: [[Char]] -> (Int, Int, Char, [Char])
parseData (a:b:c:d:as) = ((read a::Int), (read b::Int), (c!!0), d)

checkValid:: (Int, Int, Char, [Char]) -> Bool
checkValid (a, b, c, d) = (d!!(a-1) == c) /= (d!!(b-1) == c)

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- (hGetContents handle)
    let splitLines = map (words) (lines ((replace (filter (\n -> n /= ':') contents ) '-' ' ')))
    print (length (filter (checkValid) (map (parseData) splitLines)))

    hClose handle