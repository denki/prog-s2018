module Sol03 where

data Tree a = Branch a (Tree a) (Tree a) | Leaf a deriving Show

exampleTree :: Tree Int
exampleTree
  = Branch 1 (Branch 2 (Leaf 3) (Leaf 4))
             (Branch 5 (Branch 6 (Leaf 7) (Leaf 8))
                       (Leaf 9)
             )

depth :: Tree a -> Int
depth (Leaf _) = 1
depth (Branch _ s t) = 1 + (depth s `min` depth t)

paths :: Tree a -> Tree [a]
paths = paths' []

paths' :: [a] -> Tree a -> Tree [a]
paths' as (Leaf x) = Leaf $ as ++ [x] -- Leaf (as ++ [x])
paths' as (Branch x s t) = Branch l (paths' l s) (paths' l t)
  where l = as ++ [x]

tmap :: (a -> b) -> Tree a -> Tree b
tmap f (Leaf a) = Leaf $ f a
tmap f (Branch a s t) = Branch (f a) (tmap f s) (tmap f t)

prodQuad :: [Int] -> Int
prodQuad = foldr (*) 1 . map (^2) . filter even

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f a bs = foldr (\ b g x -> g (f x b)) id bs a
-- die Typen (in diesem Kontext) aller Werte:
--            f :: a -> b -> a
--            a :: a
--            bs :: [b]
--            foldr :: (b -> (a -> a) -> (a -> a)) -> (a -> a) -> [b] -> (a -> a)
--            b :: b
--            g :: a -> a
--            x :: a
--            id :: a -> a

