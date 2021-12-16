{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Data.List.NonEmpty (insert)
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

--(g)

sortOn :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn f [] = []
sortOn f (h:t) = insere h (sortOn f t)
    where insere x [] = [x]
          insere x (a:b) = if f x > f a then a:insere x b else x:a:b

--Exercicio 2

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--(a)

selgrau :: Int -> Polinomio -> Polinomio
selgrau e = filter (\x -> snd x == e)

--(b)

conta :: Int -> Polinomio -> Int
conta e p = length $ filter (\x -> snd x == e) p

--(c)

grau :: Polinomio -> Int
grau = foldl (\acc x -> if acc > snd x then acc else snd x) 0

--(d)

deriv:: Polinomio -> Polinomio
deriv l = filter (/= (0,0)) $ map (\(b,e) -> if e > 0 then (b * fromIntegral e, e-1)else (0,0)) l

--(e)

calcula :: Float -> Polinomio -> Float
calcula n = foldl (\acc (b,e) -> acc + b * (n ^ e)) 0

--(f)

simp :: Polinomio -> Polinomio
simp = filter (\(b,e) -> b /= 0)

--(g)

mult :: Monomio -> Polinomio -> Polinomio
mult (bm,em) = map (\(b,e) -> (b*bm ,e + em))

--(h)

ordena :: Polinomio -> Polinomio
ordena = sortOn snd

--(i)

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((b,e):ps) = (sum [bs | (bs,es) <- selgrau e ps] + b,e):normaliza [(bo,eo) | (bo,eo) <- ps, eo /= e]

--(j)

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza $ (++) p1 p2

--(k)

produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = foldl (\acc x -> soma (mult x p2) acc) [] p1

--(l)

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = null (simp (soma p1 (mult (-1,0) p2)))

--Exercicio 3

type Mat a = [[a]]

--(a)

dimOK :: Mat a -> Bool
dimOK [] = True
dimOK (h:t) = all (\x -> length h == length x) t

--Forma Simples
dimOK2 :: Mat a -> Bool
dimOK2 l = length l == (length . head) l

--(b)

dimMat :: Mat a -> (Int,Int)
dimMat m = (length m, (length . head) m)

--(c)

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat = zipWith'(zipWith' (+))

--(d)

transpose :: Mat a -> Mat a
transpose m = [[(m !! j) !! i | j <- [0..l-1] ] | i <- [0..c-1]]
    where (l,c) = dimMat m

--Esta função usa a função (!!) que procura x elemento de uma lista, ou seja (m!!j) resulta da procura do elemento "j" da lista m

--[[1,2,3],[4,5,6],[7,8,9]] = [[1,4,7],[2,5,7],[3,6,9]]

--(e)

multMat :: Num a => Mat a -> Mat a -> Mat a
multMat m1 m2 = [ [ sum (zipWith (*) (m1 !! j) [ x !! i | x <- m2 ]) | i <- [0..c-1] ] | j <- [0..l-1] ]
    where (l,_) = dimMat m1
          (_,c) = dimMat m2

