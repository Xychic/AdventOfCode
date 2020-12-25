{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Control.Monad
import qualified Data.Text as Text

printArray:: (Show a, Eq a) => [a] -> String
printArray (x:xs)
    | null xs = show x
    | otherwise = show x ++ "\n" ++ printArray xs

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
