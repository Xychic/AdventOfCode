import System.IO
import Control.Monad

printArray:: (Show a, Eq a) => [a] -> String
printArray (x:xs)
    | null xs = show x
    | otherwise = show x ++ "\n" ++ printArray xs

solve:: [(Char, Int)] -> Int -> (Int, Int) -> Int
solve [] _ (x, y) = abs x + abs y
solve ((ins, amt):xs) dir (x, y) 
    | ins == 'F' = solve (("NESW"!!(dir `div` 90), amt) : xs) dir (x, y)
    | ins == 'R' = solve xs ((dir + amt) `mod` 360) (x, y)
    | ins == 'L' = solve xs ((dir - amt) `mod` 360) (x, y)
    | ins == 'N' = solve xs dir (x, y+amt)
    | ins == 'E' = solve xs dir (x+amt, y)
    | ins == 'S' = solve xs dir (x, y-amt)
    | ins == 'W' = solve xs dir (x-amt, y)

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let rows = lines contents
    let instructions = map(\x -> (head x, read (tail x)::Int)) rows
    print (solve instructions 90 (0, 0))
    hClose handle
