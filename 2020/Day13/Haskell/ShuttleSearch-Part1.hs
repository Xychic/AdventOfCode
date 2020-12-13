import System.IO
import Control.Monad

printArray:: (Show a, Eq a) => [a] -> String
printArray (x:xs)
    | null xs = show x
    | otherwise = show x ++ "\n" ++ printArray xs

split:: Eq a => a -> [a] -> [[a]]
split onChar [] = []
split onChar toSplit = x : split onChar (drop 1 y) 
    where (x,y) = span (/= onChar) toSplit

getSoonest:: [Int] -> Int -> Int -> Int -> Int
getSoonest [] _ bestDelay bestBus = bestDelay * bestBus
getSoonest (bus:xs) time bestDelay bestBus
    | delay < bestDelay = getSoonest xs time delay bus
    | otherwise = getSoonest xs time bestDelay bestBus
    where delay = bus - (time `mod` bus)

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let (t:b:xs) = lines contents
    let time = read t::Int
    let buses = map (read::[Char]->Int) $ filter (/= "x") $ split ',' b
    print $ getSoonest buses time (maxBound :: Int) 0
    hClose handle
