module Sol02 where

import Prelude hiding (words, unwords)

bsp :: [Char]
bsp = ['a', 'a', 'b', 'b', 'b', 'a']

pack :: [Char] -> [[Char]]
pack [] = []
pack (x : xs) = pack' [x] x xs
  where pack' ys _ []                   = [ys]
        pack' ys y (z : zs) | y == z    = pack' (z : ys) y zs
                            | otherwise = ys : pack' [z] z zs

encode :: String -> [(Int, Char)]
encode = map (\ys -> (length ys, head ys)) . pack
-- encode xs = let xss = pack xs
--             in  map (\ys -> (length ys, head ys)) xss

decode :: [(Int, Char)] -> String
decode = concatMap (\(i, c) -> replicate i c)

rotate :: [a] -> Int -> [a]
rotate []        _                 = []
rotate xs@(y:ys) i | i < 0         = rotate xs (i + length xs)
                   | i > length xs = rotate xs (i - length xs)
                   | i > 0         = rotate (ys ++ [y]) (i - 1)
                   | i == 0        = xs
                   | otherwise     = undefined

unwords :: [String] -> String
unwords []     = ""
unwords [x]    = x
unwords (x:xs) = x ++ " " ++ unwords xs

words :: String -> [String]
words xs = words' "" xs
  where words' bs []         = [bs]
        words' [] (' ' : ys) = words ys
        words' bs (' ' : ys) = bs : words ys
        words' bs (y : ys)   = words' (bs ++ [y]) ys

maxLength :: [[a]] -> Int
maxLength = maximum . map length

data Queue a = Queue [a] [a] deriving Show

emptyQ :: Queue a
emptyQ = Queue [] []

enQ :: a -> Queue a -> Queue a
enQ x (Queue l r) = Queue (x : l) r

deQ :: Queue a -> (Queue a, a)
deQ (Queue l (x : r)) = (Queue l r, x)
deQ (Queue [] [])     = error "cannot deQ from empty Queue"
deQ (Queue l [])      = deQ $ Queue [] (reverse l)
