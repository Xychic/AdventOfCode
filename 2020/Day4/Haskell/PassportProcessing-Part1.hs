{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Control.Monad
import qualified Data.Text as Text

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

groupPass:: [[Char]] -> [[Char]]
groupPass remainder
    | null remainder = []
    | otherwise = unwords pass : groupPass (drop (length pass + 1) remainder)
        where pass = takeWhile(not . null) remainder 
    
needed:: [Text.Text]
needed = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let rows = lines contents
    print (length (filter (\p -> all (\n -> Text.isInfixOf n (Text.pack p)) needed) (groupPass rows)))
    hClose handle
