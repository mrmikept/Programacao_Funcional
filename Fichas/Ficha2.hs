module Ficha2 where
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Char (ord)
--Exercicio 2

--(a)

dobros :: [Float] -> [Float]
dobros = map (2 *)

--(b)

num0Corre::Char -> String -> Int
num0Corre _ [] = 0
num0Corre x (h:strg) = if x == h then 1 + num0Corre x strg else num0Corre x strg

--(c)

positivos::[Int]->Bool
positivos [] = False
positivos [x] = x >= 0
positivos (h:t)
    |h < 0 = False
    |otherwise = positivos t

--(d)

soPos::[Int]->[Int]
soPos [] = []
soPos (h:t) = if h > 0 then h:soPos t else soPos t

--(e)

somaNeg::[Int] -> Int
somaNeg [] = 0
somaNeg (h:t)
    |h < 0 = h + somaNeg t
    |otherwise = somaNeg t

--(f)

tresUlt::[a]->[a]
tresUlt (_:a:b:c:xs) = tresUlt(a:b:c:xs)
tresUlt l = l

--(g)

segundos::[(a,b)]->[b]
segundos [] = []
segundos ((_,b):t) = b:segundos t

--(h)

nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros n ((a,b):t)
    |n == a = True
    |otherwise = nosPrimeiros n t

--(i)

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a , b , c)
sumTriplos [x] = x
sumTriplos ((a,b,c):t) = (a + sumI, b + sumM , c + sumF)
    where (sumI , sumM , sumF) = sumTriplos t

--Exercicio 3

--(a)

soDigitos :: String -> String
soDigitos [] = []
soDigitos (h:t)
    |h `elem` ['0'..'9'] = h:soDigitos t
    |otherwise = soDigitos t

--(b)

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t)
    |h `elem` ['a'..'z'] = 1 + minusculas t
    |otherwise = minusculas t

--(c)

nums :: String -> [Int]
nums [] = []
nums (h:t)
    |h `elem` ['0'..'9'] = (ord h - ord '0'):nums t
    |otherwise = nums t

--Exercicio 4

type Polinomio = [Monomio]
type Monomio = (Float,Int)

--(a)

conta:: Int -> Polinomio -> Int
conta n [] = 0
conta n ((a,b):t)
    |n == b = 1 + conta n t
    |otherwise =conta n t

--(b)

grau :: Polinomio -> Int
grau [] = 0
grau p = maiorPol 0 p

--Funcao auxiliar

maiorPol :: Int -> Polinomio -> Int
maiorPol m [] = m
maiorPol m ((a,b):t)
    |m < b = maiorPol b t
    |otherwise = maiorPol m t

{-Para calcular qual o maior grau do polinomio dado recorremos a uma funcao auxiliar com um acumulador, temos que a lista de polinomios é constituida por um par
(coeficiente, expoente). O que a funcao maiorPol faz é verificar para cada monomio da lista do Polinomio qual é o de maior expoente usando acumulacao do maior logo
maiorPol m p onde m é o maior polinomio encontrado e p a lista de monomios constituintes do polinomio. Logo na funcao grau usamos 0 como o maior polinomio já encontrado-}

--(c)

selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau g ((c,e):t)
    |g == e = (c,e):selgrau g t
    |otherwise = selgrau g t

--(d)

deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((c,e):t)
    |e > 0 = (c*fromIntegral e , e-1):deriv t
    |otherwise = deriv t

--(e)

calcula :: Float -> Polinomio -> Float
calcula x [] = 0
calcula x ((c,e):t) = c * (x^e) + calcula x t

--(f)

simp :: Polinomio -> Polinomio
simp [] = []
simp ((c,e):t)
    |c < 1 = simp t
    |c > 1 = (c,e):simp t

--(g)

mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (cm,em) ((cp,ep):t) = (cm * cp,em + ep):mult (cm,em) t

--(h)

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza [(c,e)] = [(c,e)]
normaliza ((c1,e1):(c2,e2):t)
    |e1 == e2 = normaliza ((c1+c2,e1):t)
    |conta e1 t == 0 = (c1,e1) : normaliza ((c2,e2):t)
    |otherwise = normaliza ((c1,e1):t ++ [(c2,e2)])

--(i)

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)

--(j)

produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto (p:p1) p2 = soma (mult p p2) (produto p1 p2)

--(k)

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((c,e):t) = ordena (maiores t) ++ [(c,e)] ++ ordena (menores t)
    where maiores [] = []
          maiores ((cx,ex):tx)
            |ex > e || (ex == e && cx >= c) = (cx,ex):maiores tx
            |otherwise = maiores tx
          menores [] = []
          menores ((cx,ex):tx)
            |ex < e || (ex == e && cx < c) = (cx,ex):menores tx
            |otherwise = menores tx