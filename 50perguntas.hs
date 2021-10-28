{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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

--mygroup::Eq a => [a] -> [[a]]
--mygroup [] = []
--mygroup (h1:h2:t)
--    |h1 == h2 = (h1:h2):mygroup t