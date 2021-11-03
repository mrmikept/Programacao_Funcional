{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Data.Array.Base (listArrayST)
import Text.Read (prec)
--Exercicio 1
deAde::Int->Int->[Int]
deAde x y
    | x > y = []
    | otherwise = x:deAde (x+1) y

--Exercicio 2
enumFromTHenTo2::Int->Int->Int->[Int]
enumFromTHenTo2 x y z
    |x > z = []
    |otherwise = x:enumFromTHenTo2 (x+y-1) y z

--Exercicio 3
juntarListas::[a]->[a]->[a]
juntarListas [] a = a
juntarListas (a:b) l = a:juntarListas b l

--Exercicio 4
posicao::[a]->Int->a
posicao (h:t) p
    |p == 0 = h
    |otherwise = posicao t (p-1)

--Exercicio 5
inverso::[a]->[a]
inverso [] = []
inverso (h:t)= inverso t ++ [h]

--Exercicio 6
tirar::Int->[a]->[a]
tirar n [] = []
tirar n (h:t)
    |n == 1 = [h]
    |otherwise = h:tirar (n-1) t

--Exercicio 7
cair::Int->[a]->[a]
cair n [] = []
cair n (h:t)
    |n == 0 = h:t
    |otherwise = cair (n-1) t

--Exercicio 8
myzip::[a]->[b]->[(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (h1:t1) (h2:t2) = (h1,h2) : myzip t1 t2

--Exercicio 9

myreplicate::Int->a->[a]
myreplicate 0 _ = []
myreplicate a y = y:myreplicate (a-1) y

--Exercicio 10

myintersperce::a->[a]->[a]
myintersperce n [] = []
myintersperce n [t] = [t]
myintersperce n (h:t)= h:n:myintersperce n t

--Exercicio 11

mygroup::Eq a => [a] -> [[a]]
mygroup [] = []
mygroup [x] = [[x]]
mygroup (h:t)
    |h == head hr = (h:hr):tr
    |otherwise = [h]:hr:tr
    where (hr:tr) = mygroup t

--Exercicio 12

myconcat :: [[a]] -> [a]
myconcat = concat

--Exercicio 13

myinits :: [a] -> [[a]]
myinits [] = [[]]
myinits [x] = [[],[x]]
myinits (h:t) = [] : help h (myinits t)
    where help h (x:xs) = (h:x):help h xs
          help _ [] = []

--Exercicio 14

mytails :: [a] -> [[a]]
mytails [] = [[]]
mytails [x] = [[x],[]]
mytails l = l : mytails (tail l)

--Exercicio 15

myheads :: [[a]] -> [a]
myheads [] = []
myheads ((x:y):t) = x:myheads t

--Exercicio 16

mytotal :: [[a]] -> Int
mytotal [[]] = 0
mytotal [[x]] = 1
mytotal l = sum(map length l)

--Exercicio 17

fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,_,c):t) = (a,c): fun t

--Exercicio 18

cola :: [(String,b,c)] -> String
cola [] = []
cola ((strg,b,c):t) = strg ++ cola t

--Exercicio 19

idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade a i ((strg,n):t) = if a - n >= i then strg : idade a i t else idade a i t

--Exercicio 20

mypowerEnumFrom :: Int -> Int -> [Int]
mypowerEnumFrom n m
    |m == 1 = [1]
    |m > 1 = mypowerEnumFrom n (m-1) ++ [n^(m-1)]
    |otherwise = []

--Exercicio 21 

myisPrime :: Int -> Bool
myisPrime 1 = False
myisPrime 2 = True
myisPrime n | not (null ([x | x <- [2..n-1], mod n x == 0])) = False
                        |otherwise = True


--Exercicio 22

myisPrefixOf :: Eq a => [a] -> [a] -> Bool
myisPrefixOf [] _ = True
myisPrefixOf _ [] = True
myisPrefixOf (h1:t1) (h2:t2)
    |h1 == h2 = myisPrefixOf t1 t2
    |h1 /= h2 = False
    |otherwise = True

--Exercicio 23

myisSuffixOf :: Eq a => [a] -> [a] -> Bool
myisSuffixOf [] _ = False
myisSuffixOf _ [] = False
myisSuffixOf l1 l2
    |l1 == l2 = True
    |l1 /= l2 = myisSuffixOf l1 (tail l2)
    |otherwise = False

--Exercicio 24

myisSubsequenceOf :: Eq a => [a] -> [a] -> Bool
myisSubsequenceOf [] _ = True
myisSubsequenceOf _ [] = False
myisSubsequenceOf (h1:t1) (h2:t2)
    |h1 == h2 = myisSubsequenceOf t1 t2
    |h1 /= h2 = myisSubsequenceOf (h1:t1) t2
    |otherwise = False

--Exercicio 25

myelemIndices :: Eq a => a -> [a] -> [Int]
myelemIndices _ [] = []
myelemIndices n (h:t)
    |n == h = 0 : map (+1) (myelemIndices n t)
    |otherwise = map (+1) (myelemIndices n t)

--Exercicio 26

mynub :: Eq a => [a] -> [a]
mynub [] = []
mynub (h:t) = if h `elem` t then mynub t else h:mynub t

--Exercicio 27

mydelete :: Eq a => a -> [a] -> [a]
mydelete _ [] = []
mydelete n (h:t)
    |n == h = t
    |n /= h = h:mydelete n t

--Exercicio 28

remv :: Eq a => [a] -> [a] -> [a]
remv [] _ = []
remv l [] = l
remv (h1:t1) (h2:t2)
    |h1 == h2 = remv t1 t2
    |h1 /= h2 = h1:remv t1 (h2:t2)

--Exercicio 29

myunion :: Eq a => [a] -> [a] -> [a]
myunion l [] = l
myunion [] _ = []
myunion (h1:t1) (h2:t2)
    |h2 `elem` (h1:t1) = myunion (h1:t1) t2
    |otherwise = myunion ((h1:t1) ++ [h2]) t2

--Exercicio 30

myintersect :: Eq a => [a] -> [a] -> [a]
myintersect [] _ = []
myintersect l [] = l
myintersect (h1:t1) l
    |h1 `elem` l = h1:myintersect t1 l
    |otherwise = myintersect t1 l

--Exercicio 31

myinsert :: Ord a => a -> [a] -> [a]
myinsert n [] = [n]
myinsert n (h:t)
    |n > h = h:myinsert n t
    |otherwise = n:h:t

--Exercicio 32

myunwords :: [String] -> String
myunwords [] = ""
myunwords (h:t) = h ++ (if null t then "" else " ") ++ myunwords t

--Exercicio 33

myunlines :: [String] -> String 
myunlines [] = ""
myunlines (h:t) = h ++ "\n" ++ myunlines t

--Exercicio 34

pMaior :: Ord a => [a] -> Int
pMaior [a] = 0 
pMaior (h:t)
    |h > (t !! pMaior t) = 0
    |otherwise = 1 + pMaior t

--Exercicio 35 

mylookup :: Eq a => a -> [(a,b)] -> Maybe b
mylookup _ [] = Nothing 
mylookup x ((a,b):t)
    |x == a = Just b
    |otherwise = mylookup x t

--Exercicio 36

preCrescente :: Ord a => [a] -> [a]
preCrescente [] = [] 
preCrescente [x] = [x]
preCrescente (h:t)
    |h < head t = h:preCrescente t
    |otherwise = preCrescente t