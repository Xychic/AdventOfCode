import System.IO
import Control.Monad

printArray:: (Show a, Eq a) => [a] -> String
printArray (x:xs)
    | null xs = show x
    | otherwise = show x ++ "\n" ++ printArray xs

strip:: (Eq a) => [a] -> a -> [a]
strip [] _ = []
strip (x:xs) a
    | x == a = strip xs a
    | otherwise = x : strip xs a

runCode:: Int -> Int -> [Int] -> [([Char], Int)] -> Int
runCode acc ins seen code 
    | ins `elem` seen = acc
    | op == "nop" = runCode acc (ins+1) (ins:seen) code
    | op == "jmp" = runCode acc (ins+arg) (ins:seen) code
    | op == "acc" = runCode (acc+arg) (ins+1) (ins:seen) code
    where (op, arg) = code!!ins 

parseInstruction:: [Char] -> ([Char], Int)
parseInstruction ins = (op, read arg::Int)
    where (op:arg:_) = split ' ' (strip ins '+')

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let rows = lines contents
    print (runCode 0 0 [] (map parseIns rows))
    hClose handle