import System.IO
import Control.Monad

printArray:: (Show a, Eq a) => [a] -> String
printArray (x:xs)
    | null xs = show x
    | otherwise = show x ++ "\n" ++ printArray xs

countNeighbours:: [[Char]] -> Int -> Int -> Char
countNeighbours world x y 
    | seat == '.' = '.'
    | seat == 'L' && adjacent == 0 = '#'
    | seat == '#' && adjacent >= 4 = 'L'
    | otherwise = seat
    where     
        seat = world!!y!!x
        lowerX = if x == 0 then x else x-1
        upperX = if x == length (head world) -1 then x else x+1
        lowerY = if y == 0 then y else y-1
        upperY = if y == length world-1 then y else y+1
        pos = [(i,j) | i <- [lowerX..upperX], j <- [lowerY..upperY], (x,y) /= (i, j)]
        adjacent = sum (map (\x -> fromEnum (world!!snd x!!fst x == '#')) pos)

solve:: [[Char]] -> Int -> [Int]
solve world previous
    | previous == current = [previous]
    | otherwise = current : solve newWorld current
    where 
        current = sum $ map (length . filter (=='#')) world
        newWorld = [[countNeighbours world x y | x <- [0..(length (head world)-1)]] | y <- [0..length world -1]]

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let world = lines contents
    print (solve world 2)
    hClose handle
