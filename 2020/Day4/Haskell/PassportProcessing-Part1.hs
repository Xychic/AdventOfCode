{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Control.Monad
import qualified Data.Text as Text

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

groupPass:: [[Char]] -> [[Char]] -> [[Char]]
groupPass groups remainder
    | (length remainder) == 0 = []
    | otherwise = groups ++ [unwords x] ++ (groupPass [] (drop ((length x) + 1) remainder)) where x = takeWhile(\x -> 0 < (length x)) remainder 
    
needed = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let rows = (lines contents)
    print (length (filter (\p -> (all (\n -> Text.isInfixOf n (Text.pack p)) needed)) (groupPass [] rows)))
    hClose handle
