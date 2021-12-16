{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Char
import Data.List

--Exercicio 1

digitAlpha :: String -> (String,String)
digitAlpha = foldl(\(c,d) x -> if isDigit x then (c,d++[x]) else if isAlpha x then (c++[x],d) else (c,d)) ("","")

--Exericio 2

nzp :: [Int] -> (Int,Int,Int)
nzp = foldl (\(n,z,p) x -> if x < 0 then (n+1,z,p) else if x == 0 then (n,z+1,p) else if x > 0 then (n,z,p+1) else (n,z,p)) (0,0,0)

--Exercicio 3

mydivMod :: Integral a => a -> a -> (a, a)
mydivMod x y = foldl (\(a,b) n -> (a+1,b-y)) (0,x) [y,2*y..x]

--Exercicio 4
fromDigits :: [Int] -> Int
fromDigits l = fromDigitsAc l 0

--Func. Auxiliar
fromDigitsAc :: [Int] -> Int -> Int
fromDigitsAc t a = foldl (\ a h -> h + 10 * a) a t

--Versão sem usar uma auxiliar
fromDigits2 :: [Int] -> Int
fromDigits2 = foldl (\acc x -> x + 10 * acc) 0

--Exercicio 5

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = foldl (\acc x -> max (sum x) acc) (sum l) (inits l)

--Exercicio 6

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib' 0 1 n
    where fib' a b n | n <= 1 = b
                     | otherwise = fib' b (a+b) (n-1)

{- fib n usa a função auxiliar fib' a b n onde a é o e-nesimo numero da sucessão e b o e-nesimo + 1. Se n for maior ou igual a 1 então chamamos recursivamente a função
fib' mas agora o a é igual ao b e o valor de b é igual ao valor de a + b e retiramos 1 ao valor de n até chegarmos ao fib' a b 1(ou 0) onde devolvemos o valor de b onde
b é o acumulardor de n+n+1-}

--Exercicio 7

{-intToStr :: Integer -> String
intToStr n = intToStrAc n ""
    where intToStrAc n s -}

--Exercicio 8

--(a)

{-[x | x <- [1..20], mod x 2 == 0, mod x 3 == 0] = [6,12,18]
Esta lista por compressão é gerada se o resto da divisão de x por 2 e por 3 for igual a 0, ou seja mostra todos os valores entre 1 e 20 que são divisiveis por 2 e por 3-}

--Outra forma de escrever esta lista: [x | x <- [1..20], even x, mod x 3 == 0]

--(b)

{-[x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0] = [6,12,18]
Esta lista por compreensão é formada por outra lista de compreensão onde y pertence de 1 a 20 e y tem de ser divisivel por 2, ou seja os numeros de 1 a 20 que são par.
Dessa lista x é os numeros que são par de 1 a 20 e são divisiveis por 3, ou seja que são multiplos de 3.

Outra froma de escrever esta lista temos: [x | x <-[y | y <- [1..20], even y], mod x 3 == 0]
-}