{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Control.Monad
import qualified Data.Text as Text

printArray:: (Show a, Eq a) => [a] -> String
printArray (x:xs)
    | null xs = show x
    | otherwise = show x ++ "\n" ++ printArray xs

split:: Eq a => a -> [a] -> [[a]]
split onChar [] = []
split onChar toSplit = x : split onChar (drop 1 y) 
    where (x,y) = span (/= onChar) toSplit

groupPass:: [[Char]] -> [[Char]]
groupPass remainder
    | null remainder = []
    | otherwise = unwords pass : groupPass (drop (length pass + 1) remainder)
        where pass = takeWhile(not . null) remainder 

validate:: [[Char]] -> Bool
validate (x:y:xs)
    | x == "byr" = 1920 <= val && val <= 2002 
    | x == "iyr" = 2010 <= val && val <= 2020 
    | x == "eyr" = 2020 <= val && val <= 2030 
    | x == "hgt" && unit == "cm" = 150 <= height && height <= 193
    | x == "hgt" && unit == "in" = 59 <= height && height <= 76
    | x == "hcl" = (length y == 7) && (head y == '#') && all (\char -> char `elem` ("0123456789abcdef"::String)) (tail y)
    | x == "ecl" = y `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    | x == "pid" = (length y == 9) && all (\char -> char `elem` ("0123456789"::String)) y
    | x == "cid" = True
    | otherwise = False
        where 
            val = read y::Int
            unit = drop (length y - 2) y
            height = read (take (length y - 2) y)::Int
    
needed:: [Text.Text]
needed = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let rows = lines contents
    let filtered = filter (\passport -> all (\need -> Text.isInfixOf need (Text.pack passport)) needed) (groupPass rows)
    let splitPass = map (map (split ':') . split ' ') filtered
    print (length (filter (all validate) splitPass))
    hClose handle
