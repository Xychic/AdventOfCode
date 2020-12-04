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

validate:: [[Char]] -> Bool
validate (x:y:xs)
    | x == "byr" = 1920 <= val && val <= 2002 
    | x == "iyr" = 2010 <= val && val <= 2020 
    | x == "eyr" = 2020 <= val && val <= 2030 
    | x == "hgt" && unit == "cm" = 150 <= height && height <= 193
    | x == "hgt" && unit == "in" = 59 <= height && height <= 76
    | x == "hcl" = (length y == 7) && ((head y) == '#') && (all (\n -> elem n ("0123456789abcdef"::String)) (tail y))
    | x == "ecl" = elem y ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    | x == "pid" = (length y == 9) && (all (\n -> elem n ("0123456789"::String)) y)
    | x == "cid" = True
    | otherwise = False
        where 
            val = read y::Int
            unit = drop ((length y) - 2) y
            height = read (take ((length y) - 2) y)::Int
    
needed:: [Text.Text]
needed = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let rows = (lines contents)
    let filtered = filter (\p -> (all (\n -> Text.isInfixOf n (Text.pack p)) needed)) (groupPass [] rows)
    let splitPass = (map (\s -> (map (\x -> split ':' x) (split ' ' s))) filtered)
    print (length (filter (\p -> (all (\a -> validate a) p)) splitPass))
    hClose handle
