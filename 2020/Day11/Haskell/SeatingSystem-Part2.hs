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
    | seat == '#' && adjacent >= 5 = 'L'
    | otherwise = seat
    where     
        dirs = [(i, j) | i <- [-1..1], j <- [-1..1], (i,j) /= (0,0)]
        adjacent = sum (map (\dir -> seeTaken [(x+dx*mult, y+dy*mult) | let (dx, dy) = dir, mult <- [1..]]) dirs)
        seat = world!!y!!x
        seeTaken [] = 0
        seeTaken ((x,y):xs)
            | y < 0 || y >= length world = 0
            | x < 0 || x >= length (head world) = 0
            | char == '.' = seeTaken xs
            | char == '#' = 1
            | otherwise = 0
            where 
                char = world!!y!!x

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