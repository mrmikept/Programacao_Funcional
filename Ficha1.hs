{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Distribution.Simple.Utils (xargs)
import Control.Exception (BlockedIndefinitelyOnMVar)
--Exercício 1
perimetro::Float->Float
perimetro r = 2*pi*r

dist::(Double , Double)->(Double , Double)->Double
dist (x1,y1) (x2,y2) = sqrt((x1-x2)^2+(y1-y2)^2)

primUlt::[a]->(a , a)
primUlt l = (head l , last l)

multiplo::Int->Int->Bool
multiplo m n = mod m n == 0

truncaImpar::[a]->[a]
truncaImpar l
    |even (length l) = l
    |otherwise = tail l

max2::Int-> Int ->Int
max2 x y
    |x > y = x
    |otherwise = y

max3::Int->Int->Int->Int
max3 x y z
    |max2 x y > z = max2 x y
    |otherwise = z

--Exercicio 2

nRaizes::Double->Double->Double->Int
nRaizes a b c
    | delta > 0 = 2
    | delta == 0 = 1
    | delta < 0 = 0
    where delta = b^2 - 4*a*c

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

horaValida::Hora->Bool
horaValida (h , m) = elem h [0..23] && elem m [0..59]

horaMaior::Int->Int->Int
horaMaior h1 h2 = if h1 > h2 then h1 else h2

horaMaior2::Hora->Hora->Hora
horaMaior2 (h1 , m1) (h2 , m2) = if horaValida(h1 , m1) && horaValida(h2 , m2)
                                then if h1 > h2 || h1 == h2 && m1 > m2
                                    then (h1 , m1)
                                    else (h2 , m2)
                                else error "hora invalida"

horapmin::Hora->Int
horapmin (h , m)
    |horaValida (h , m) = h*60+m

minphora::Int->Hora
minphora m = (div m 60 , mod m 60)

difhora::Hora->Hora->Int
difhora (h1,m1) (h2,m2) = abs (horapmin (h1,m1) - horapmin(h2 , m2))

addmin::Hora->Int->Hora
addmin (h,m1) m2 = minphora(horapmin(h,m1) + m2)

--Exercicio 4

data Hora' = H Int Int deriving (Show,Eq)

horaValida2 :: Hora' -> Bool
horaValida2 (H h m) = elem h [0..23] && elem m [0..59]

horaMaior'::Hora' -> Hora' -> Hora'
horaMaior' (H h1 m1) (H h2 m2) = if horaValida2(H h1 m1) && horaValida2(H h2 m2)
                                then if h1 > h2 || h1 == h2 && m1 > m2
                                    then H h1 m1
                                    else H h2 m2
                                else error "hora invalida"

horapmin2::Hora'->Int
horapmin2 (H h m)
    |horaValida2 (H h m) = h*60+m

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
stop cor = cor == Vermelho

--(c)
safe::Semafro->Semafro->Bool
safe cor1 cor2 = not (cor1 == Vermelho || cor2 == Vermelho)

--Exercicio 6

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

--(a)
