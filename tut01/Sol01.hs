-- email: Tobias Denkinger <tobias.denkinger@tu-dresden.de>
-- github: https://github.com/denki/prog-s18

module Sol01 where

import Prelude hiding (rem)

fac, fac' :: Int -> Int
fac 0 = 1
fac n = fac (n - 1) * n

fac' n = product [1 .. n]

sumFacs, sumFacs' :: Int -> (Int -> Int)
sumFacs n m = sum [fac i | i <- [n .. m]]

sumFacs' n m = sum $ map fac [n .. m]

fib, fibFast :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibFast = fib' 1 1

fib' :: Int -> Int -> Int -> Int
fib' f1 _ 0 = f1
fib' _ f2 1 = f2
fib' f1 f2 n = fib' f2 (f1 + f2) (n - 1)

prod, prod' :: [Int] -> Int
prod [] = 1
prod (x : xs) = x * prod xs

prod' = foldl (*) 1

rev :: [a] -> [a]
rev [] = []
rev (x : xs) = rev xs ++ [x]

rem :: Eq a => a -> [a] -> [a]
rem _ [] = []
rem x (y : ys)
  | x == y    = rem x ys
  | otherwise = y : rem x ys

isOrd :: Ord a => [a] -> Bool
isOrd [] = True
isOrd [_] = True
isOrd (x : y: ys) = x <= y && isOrd (y : ys)

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x <= y    = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys
