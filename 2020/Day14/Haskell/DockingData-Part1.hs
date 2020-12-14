import System.IO
import Control.Monad
import Data.List
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

printArray:: (Show a, Eq a) => [a] -> String
printArray (x:xs)
    | null xs = show x
    | otherwise = show x ++ "\n" ++ printArray xs

splitStr :: Eq a => [a] -> [a] -> [[a]]
splitStr sub str = split' sub str [] []
    where
    split' _ [] subacc acc = reverse (reverse subacc:acc)
    split' sub str subacc acc
        | sub `isPrefixOf` str = split' sub (drop (length sub) str) [] (reverse subacc:acc)
        | otherwise = split' sub (tail str) (head str:subacc) acc

slice:: Int -> Int -> [a] -> [a]
slice from to xs
    | to < 0 = slice from (length xs + to -1) xs
    | otherwise = take (to - from + 1) (drop from xs)

binToDec:: [Char] -> Integer
binToDec binString = foldr (\c s -> s * 2 + c) 0 (reverse (map charToInt binString))
    where charToInt c = if c == '0' then 0 else 1

decToBin:: Integer -> [Char]
decToBin num = replicate (36 - length str) '0' ++ str
    where str = showIntAtBase 2 intToDigit num ""

applyMask:: [Char] -> [Char] -> [Char] -> Integer
applyMask [] [] bStr = binToDec bStr
applyMask (x:xs) (y:ys) bStr
    | x == 'X' = applyMask xs ys (bStr ++ [y])
    | otherwise = applyMask xs ys (bStr ++ [x])

storeMem:: Memory -> [Char] -> Integer -> Int -> Memory
storeMem mem trg val pos
    | pos >= length mem = mem ++ [(trg, val)]
    | add == trg = take pos mem ++ [(trg, val)] ++ drop (pos+1) mem
    | otherwise = storeMem mem trg val (pos+1)
    where (add, _) = mem!!pos

type Memory = [([Char], Integer)]
readMem:: [Char] -> Memory -> [[[Char]]] -> Memory
readMem _ memory [] = memory
readMem mask memory (x:xs)
    | op == "mask" = readMem dat memory xs
    | otherwise = readMem mask (storeMem memory addr masked 0) xs
    where 
        (op:dat:_) = x
        addr = slice 4 (-1) op
        binData = decToBin (read dat::Integer)
        masked = applyMask mask binData ""

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let rows = lines contents
    let input = map (splitStr " = ") rows
    -- putStrLn (printArray (readMem "" [] input))
    print (sum (map snd (readMem "" [] input)))

    hClose handle
