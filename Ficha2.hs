--Exercicio 2

--(a)

dobros :: [Float] -> [Float]
dobros t = map (2 *) t

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
{-}
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros ((a,_):t) x = (a == x) || nosPrimeiros t
nosPrimeiros [] x = False -}