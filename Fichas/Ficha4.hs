{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Char
import GHC.Unicode (isDigit)

--Exercicio 1

digitAlpha :: String -> (String,String)
digitAlpha = foldl(\(c,d) x -> if isDigit x then (c,d++[x]) else if isAlpha x then (c++[x],d) else (c,d)) ("","")

--Exericio 2

nzp :: [Int] -> (Int,Int,Int)
nzp = foldl (\(n,z,p) x -> if x < 0 then (n+1,z,p) else if x == 0 then (n,z+1,p) else if x > 0 then (n,z,p+1) else (n,z,p)) (0,0,0)

--Exercicio 3

--divMod :: Integral a => a -> a -> (a, a)
--divMod x y = foldl(\(a,b) n)
