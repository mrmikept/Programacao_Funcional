{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Ficha1 where

import Data.Char ( ord, chr )
--Exercício 1

--(a)

perimetro::Float->Float
perimetro r = 2*pi*r

--(b)

dist::(Double , Double)->(Double , Double)->Double
dist (x1,y1) (x2,y2) = sqrt((x1-x2)^2+(y1-y2)^2)

--(c)

primUlt::[a]->(a , a)
primUlt l = (head l , last l)

--(d)

multiplo::Int->Int->Bool
multiplo m n = mod m n == 0

--(e)

truncaImpar::[a]->[a]
truncaImpar l
    |even (length l) = l
    |otherwise = tail l

--(f)

max2::Int-> Int ->Int
max2 x y
    |x > y = x
    |otherwise = y

--(g)

max3::Int->Int->Int->Int
max3 x y z
    |max2 x y > z = max2 x y
    |otherwise = z

--Exercicio 2

--(a)

nRaizes::Double->Double->Double->Int
nRaizes a b c
    | delta > 0 = 2
    | delta == 0 = 1
    | delta < 0 = 0
    where delta = b^2 - 4*a*c

--(b)

raizes::Double->Double->Double->[Double]
raizes a b c
    | n == 2 = [x1, x2]
    | n == 1 = [x1]
    | n == 0 = []
    where n = nRaizes a b c
          delta = b^2 - 4*a*c
          (x1,x2) = (((-b) + sqrt delta)/ (2*a), ((-b) - sqrt delta)/ (2*a))

--Exercicio 3

type Hora = (Int,Int)

--(a)

horaValida::Hora->Bool
horaValida (h , m) = elem h [0..23] && elem m [0..59]

--(b)

horaMaior::Int->Int->Int
horaMaior h1 h2 = if h1 > h2 then h1 else h2

horaMaior2::Hora->Hora->Hora
horaMaior2 (h1 , m1) (h2 , m2) = if horaValida(h1 , m1) && horaValida(h2 , m2)
                                then if h1 > h2 || h1 == h2 && m1 > m2
                                    then (h1 , m1)
                                    else (h2 , m2)
                                else error "hora invalida"

--(c)

horapmin::Hora->Int
horapmin (h , m)
    |horaValida (h , m) = h*60+m

--(d)

minphora::Int->Hora
minphora m = (div m 60 , mod m 60)

--(e)

difhora::Hora->Hora->Int
difhora (h1,m1) (h2,m2) = abs (horapmin (h1,m1) - horapmin(h2 , m2))

--(f)

addmin::Hora->Int->Hora
addmin (h,m1) m2 = minphora(horapmin(h,m1) + m2)

--Exercicio 4

data Hora' = H Int Int deriving (Show,Eq)

--(a)

horaValida' :: Hora' -> Bool
horaValida' (H h m) = elem h [0..23] && elem m [0..59]

--(b)

horaMaior'::Hora' -> Hora' -> Hora'
horaMaior' (H h1 m1) (H h2 m2) = if horaValida'(H h1 m1) && horaValida'(H h2 m2)
                                then if h1 > h2 || h1 == h2 && m1 > m2
                                    then H h1 m1
                                    else H h2 m2
                                else error "hora invalida"

--(c)

--horapmin'::Hora'->Int
horapmin' (H h m)
    |horaValida' (H h m) = h*60+m

--(d)

--minphora' :: Int -> Hora'
minphora' m = H (div m 60) (mod m 60)

--(e)

--difhora' :: Hora' -> Hora' -> Int
difhora' h1 h2 = minphora' $ abs $ horapmin' h1 - horapmin' h2

--(f)

--addmin' :: Hora' -> Int -> Hora'
addmin' h m = minphora' $ mod (horapmin' h + m) 1440 

--Exercicio 5

data Semafro = Verde | Amarelo | Vermelho deriving (Show,Eq)

--(a)

next:: Semafro -> Semafro
next cor
    |cor == Verde = Amarelo
    |cor == Vermelho = Verde
    |otherwise = Vermelho

--(b)

stop:: Semafro -> Bool
stop cor = cor /= Verde


--(c)

safe::Semafro->Semafro->Bool
safe cor1 cor2 = not (cor1 == Vermelho || cor2 == Vermelho)

--Exercicio 6

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

--(a)

posx::Ponto -> Double
posx (Cartesiano x _) = x
posx (Polar dist ang) = if ang == pi/2 || ang == -pi/2 then 0 else dist * cos ang

--(b)

posy::Ponto -> Double
posy (Cartesiano _ y) = y
posy (Polar dist ang) = if ang == 0 || ang == pi then 0 else dist * sin ang

--(c)

raio::Ponto -> Double
raio (Cartesiano x y) = sqrt (x^2 + y^2)
raio (Polar d _) = d

--(d)

angulo::Ponto -> Double
angulo (Cartesiano x y)
    |x < 0 && y == 0 = pi
    |x < 0 = pi + atan(y/x)
    |otherwise = atan(y/x)
angulo (Polar _ ang) = ang

--(e)

dist'::Ponto->Ponto->Double
dist' p1 p2 = sqrt((posx p1 - posx p2)^2 + (posy p1 - posy p2)^2)

--Exercicio 7

data Figura = Circulo Ponto Double | Retangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)

--(a)

poligono::Figura -> Bool
poligono (Circulo _ _) = False
poligono (Retangulo p1 p2) = posx p1 /= posx p2 && posy p1 /= posy p2
poligono (Triangulo p1 p2 p3) = (posy p1 - posy p2) / (posx p1 - posx p2) /= (posy p2 - posy p3) / (posx p2 - posy p3)

--(b)

vertices::Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Retangulo p1 p2) = if poligono (Retangulo p1 p2) then [p1, Cartesiano (posx p2) (posy p1), Cartesiano (posx p1) (posy p2) ,p2] else []
vertices (Triangulo p1 p2 p3) = if poligono (Triangulo p1 p2 p3) then [p1 , p2 , p3] else []

--(c)

area:: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = dist' p1 p2
        b = dist' p2 p3
        c = dist' p3 p1
        s = (a+b+c) / 2 --semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) --formula de Heron
area (Retangulo p1 p2) = abs(posx p1 - posx p2) * abs(posy p1 - posy p2) -- valor do x do ponto 1 - valor x do ponto 2 = comprimento ^ valor do y do ponto 1 - valor do y do ponto 2 = altura
area (Circulo _ r) = pi*r^2

--(d)

perimetro'::Figura -> Double
perimetro' (Circulo _ r) = 2*pi*r
perimetro' (Retangulo p1 p2) = abs(posx p1 - posx p2)*2 + abs(posy p1 - posy p2)*2
perimetro' (Triangulo p1 p2 p3) = dist' p1 p2 + dist' p2 p3 * dist' p3 p1

--Exercicio 8

--(a)

myisLower::Char->Bool
myisLower c = c `elem` ['a'..'z']

--(b)

myisDigit::Char->Bool 
myisDigit c = c `elem` ['0'..'9']

--(c)

myisAlpha::Char->Bool 
myisAlpha c = myisLower c || c `elem` ['A'..'Z']

--(d)

mytoUpper::Char->Char 
mytoUpper c = if myisLower c then chr (ord c - 32) else c

--(e)

myintToDigit::Int->Char 
myintToDigit c = chr (c + 48)

--(f)

mydigitToInt::Char->Int 
mydigitToInt c = ord c - 48