--Exercicio 1

data ExpInt = Const Int
    | Simetrico ExpInt
    | Mais ExpInt ExpInt
    | Menos ExpInt ExpInt
    | Mult ExpInt ExpInt

--(a)

calcula :: ExpInt -> Int
calcula (Const n) = n
calcula (Simetrico exp) = -calcula exp
calcula (Mais x y) = calcula x + calcula y
calcula (Menos x y) = calcula x - calcula y
calcula (Mult x y) = calcula x * calcula y

--(b)

infixa :: ExpInt -> String
infixa (Const nm) = show nm
infixa (Simetrico nm) = "(-" ++ infixa nm ++ ")"
infixa (Mais x y) = '(':infixa x ++ "+" ++ infixa y ++ ")"
infixa (Menos x y) = '(':infixa x ++ "-" ++ infixa y ++ ")"
infixa (Mult x y) = '(':infixa x ++ "*" ++ infixa y ++ ")"

--(c)

posfixa :: ExpInt -> String
posfixa (Const nm) = show nm
posfixa (Simetrico nm) = "-" ++ infixa nm
posfixa (Mais x y) = posfixa x ++ " " ++ posfixa y ++ " " ++ "+"
posfixa (Menos x y) = posfixa x ++ " " ++ posfixa y ++ " " ++ "-"
posfixa (Mult x y) = posfixa x ++ " " ++  posfixa y ++ " " ++ "*"

--Exercicio 2

data RTree a = R a [RTree a] deriving Show

--Considere a seguinte árvore
rt1 = R 1 [R 2 [R 4 [],R 5 []],R 3 [R 6 [], R 7 []]]
--(a)

soma :: Num a => RTree a -> a
soma (R a []) = a
soma (R a xs) = a + sum (map soma xs)

--(b)

altura :: RTree a -> Int
altura (R a []) = 1
altura (R a l) = 1 + maximum (map altura l)

--(c)

prune :: Int -> RTree a -> RTree a
prune 0 (R a _) = R a []
prune x (R a l) = R a (map (prune (x-1)) l)

--(d)

mirror :: RTree a -> RTree a
mirror (R a l) = R a (map mirror (reverse l))

--(e)

postorder :: RTree a -> [a]
postorder (R a []) = [a]
postorder (R a l) = concatMap postorder l ++ [a]