import System.IO
import Control.Monad
import Data.List(sort, group)

printArray:: (Show a, Eq a) => [a] -> String
printArray (x:xs)
    | null xs = show x
    | otherwise = show x ++ "\n" ++ printArray xs

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let rows = lines contents
    let ad = 0 : sort (map (read::String->Int) rows)
    let adapters = ad ++ [maximum ad + 3]
    print (foldl (\x y -> x * length y) 1 $
        group $ -- Creates a 2D list of the form [[1,1,...], [3,3,...]]
            sort $ 
                zipWith (-) (tail adapters) adapters)   -- Creates a list of differences between one number and the next

    hClose handle
