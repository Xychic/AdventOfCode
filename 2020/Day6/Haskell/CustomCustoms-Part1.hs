import System.IO
import Control.Monad
import Data.List

printArray:: (Show a, Eq a) => [a] -> String
printArray (x:xs)
    | null xs = show x
    | otherwise = show x ++ "\n" ++ printArray xs

groupQuestions:: [[Char]] -> [[Char]]
groupQuestions remainder
    | null remainder = []
    | otherwise = concat pass : groupQuestions (drop (length pass + 1) remainder)
        where pass = takeWhile(not . null) remainder 

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let rows = lines contents
    let groups = map (length . nub) (groupQuestions rows)
    print (sum groups)

    hClose handle