import System.IO
import Control.Monad

prIntegerArray:: (Show a, Eq a) => [a] -> String
prIntegerArray (x:xs)
    | null xs = show x
    | otherwise = show x ++ "\n" ++ prIntegerArray xs

split:: Eq a => a -> [a] -> [[a]]
split onChar [] = []
split onChar toSplit = x : split onChar (drop 1 y) 
    where (x,y) = span (/= onChar) toSplit

modInv :: Integer -> Integer -> Integer
modInv a m = i
  where
    (i, _, g) = gcdExt a m
 
gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt a 0 = (1, 0, a)
gcdExt a b = (t, s - q * t, g)
    where
        (q, r) = a `quotRem` b
        (s, t, g) = gcdExt b r

parse:: [[Char]] -> Integer -> [(Integer, Integer)]
parse [] _ = []
parse (bus:xs) delay
    | bus == "x" = parse xs (delay+1) 
    | otherwise = (read bus::Integer, delay) : parse xs (delay+1)

crt:: [(Integer, Integer)] -> Integer -> Integer
crt [] _ = 0
crt ((bus, delay):xs) m = (ai * bi * biInv) + crt xs m
    where 
        ai = bus - (delay `mod` bus)
        bi = m `div` bus
        biInv = modInv bi bus

main:: IO()
main = do
    handle <- openFile "../input.txt" ReadMode
    contents <- hGetContents handle
    let (t:b:xs) = lines contents
    let time = read t::Integer
    let buses = parse (split ',' b) 0
    let m = product (map fst buses)
    print $ crt buses m `mod` m
    hClose handle
