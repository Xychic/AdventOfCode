import System.IO
import Control.Monad
import Data.List
import Data.Char(digitToInt)

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

splitStr :: Eq a => [a] -> [a] -> [[a]]
splitStr sub str = split' sub str [] []
    where
    split' _ [] subacc acc = reverse (reverse subacc:acc)
    split' sub str subacc acc
        | sub `isPrefixOf` str = split' sub (drop (length sub) str) [] (reverse subacc:acc)
        | otherwise = split' sub (tail str) (head str:subacc) acc

replace:: (Eq a) => [a] -> a -> a -> [a]
replace [] _ _ = []
replace (x:xs) a b 
    | x == a = b : replace xs a b
    | otherwise = x : replace xs a b

replaceStr:: (Eq a) => [a] -> [a] -> [a] -> [a]
replaceStr toReplace target sub = replaceStr' (splitStr target toReplace) sub
    where
    replaceStr' (x:xs) sub
        | null xs = x
        | otherwise = x ++ sub ++ replaceStr' xs sub

replaceAllStr:: (Eq a) => [a] -> [([a], [a])] -> [a]
replaceAllStr toReplace [] = toReplace
replaceAllStr toReplace ((a,b):xs) = replaceAllStr (replaceStr toReplace a b) xs

type BagData = ([Char], [[Char]])
parseLine:: [[Char]] -> BagData
parseLine (a:b:xs)
    | "no other" `isInfixOf` b = (a, [])
    | otherwise = (a, map(drop 2) splitBags)
    where 
        splitBags = splitStr ", " (replaceAllStr b [(".",""), (" bags",""), (" bag","")])

getParent:: [BagData] -> [Char] -> [[Char]]
getParent ((a,b):xs) target
    | null xs = []
    | target `elem` b = a : getParent xs target
    | otherwise = getParent xs target

getParentRec:: [BagData] -> [[Char]] -> [[Char]]
getParentRec bagData (x:xs)
    | null parents && null xs = []
    | null parents = getParentRec bagData xs
    | null xs = parents ++ getParentRec bagData parents
    | otherwise = parents ++ getParentRec bagData xs ++ getParentRec bagData parents
    where parents = getParent bagData x

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let rows = lines contents
    let parsed = map (parseLine . splitStr " bags contain " ) rows
    print (length $ nub $ getParentRec parsed ["shiny gold"])
    hClose handle
