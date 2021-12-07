{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
--Exercicio 1

--(a)
any' :: (a -> Bool) -> [a] -> Bool
any' f = foldr ((||) . f) False

--(b)

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f (h1:t1) (h2:t2) = f h1 h2 : zipWith' f t1 t2
zipWith' _ _ _ = []

--(c)

takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (h:t)
    |f h = h:takeWhile' f t
    |otherwise = takeWhile' f t

--(d)

dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (h:t)
    |f h = dropWhile f t
    |otherwise = t

--(e)

span' :: (a -> Bool) -> [a] -> ([a],[a])
span' _ [] = ([],[])
span' f (h:t)
    | f h = (h:s1,s2)
    |otherwise = ([],h:t)
        where (s1,s2) = span' f t

--(f)

deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy f n [] = []
deleteBy f n (h:t) 
    |f n h = t
    |otherwise = h:deleteBy f n t