import System.IO
import Control.Monad

printArray:: (Show a, Eq a) => [a] -> String
printArray (x:xs)
    | null xs = show x
    | otherwise = show x ++ "\n" ++ printArray xs

replace:: (Eq a) => [a] -> a -> a -> [a]
replace [] _ _ = []
replace (x:xs) a b 
    | x == a = b : replace xs a b
    | otherwise = x : replace xs a b

parseData:: [[Char]] -> (Int, Int, Char, [Char])
parseData (a:b:c:d:as) = (read a::Int, read b::Int, head c, d)

checkValid:: (Int, Int, Char, [Char]) -> Bool
checkValid (a, b, c, d) = (d!!(a-1) == c) /= (d!!(b-1) == c)

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let splitLines = map words (lines (replace (filter (/= ':') contents ) '-' ' '))
    print (length (filter checkValid (map parseData splitLines)))

    hClose handle