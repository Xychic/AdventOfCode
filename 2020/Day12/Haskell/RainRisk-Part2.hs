import System.IO
import Control.Monad

printArray:: (Show a, Eq a) => [a] -> String
printArray (x:xs)
    | null xs = show x
    | otherwise = show x ++ "\n" ++ printArray xs

rotate:: (Int, Int) -> Int -> (Int, Int)
rotate (x, y) amt
    | amt <= 0 = (x, y)
    | otherwise = rotate (-y, x) (amt-90)

solve:: [(Char, Int)] -> (Int, Int) -> (Int, Int) -> Int
solve [] _ (x, y) = abs x + abs y
solve ((ins, amt):xs) w@(wx, wy) p@(px, py) 
    | ins == 'F' = solve xs w (px + wx * amt, py + wy * amt)
    | ins == 'R' = solve xs (rotate w amt) p
    | ins == 'L' = solve xs (rotate w (360-amt)) p
    | ins == 'N' = solve xs (wx + amt, wy) p
    | ins == 'E' = solve xs (wx, wy + amt) p
    | ins == 'S' = solve xs (wx - amt, wy) p
    | ins == 'W' = solve xs (wx, wy - amt) p

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let rows = lines contents
    let instructions = map(\x -> (head x, read (tail x)::Int)) rows
    print (solve instructions (1, 10) (0, 0))
    hClose handle
