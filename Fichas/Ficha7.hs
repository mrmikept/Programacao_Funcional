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

--Exercicio 3

data LTree a = Tip a | Fork (LTree a) (LTree a) deriving Show

--Seja lt1 uma LTree

lt1 = Fork (Fork (Tip 3) (Fork (Tip 2) (Tip 4))) (Fork (Tip 5) (Fork (Tip 6) (Fork (Tip 7) (Tip 1))))

--(a)

ltSum :: Num a => LTree a -> a
ltSum (Tip x) = x
ltSum (Fork e d) = ltSum e + ltSum d

--(b)

listaLT :: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork e d) = listaLT e ++ listaLT d

--(c)

ltHeight :: LTree a -> Int
ltHeight (Tip _) = 0
ltHeight (Fork e d) = 1 + max(ltHeight e) (ltHeight d)

--Exercicio 4

data FTree a b = Leaf b | No a (FTree a b) (FTree a b) deriving Show

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

--Consideremos ft1 como uma FTree

ft1 = No 5 (No 3 (Leaf 1) (Leaf 4)) (No 10 (Leaf 7) (No 15 (Leaf 13) (Leaf 20)))

a4 = Node 5 (Node 3 (Node 1 Empty Empty) (Node 4 (Node 2 Empty Empty) Empty)) (Node 10 (Node 7 Empty Empty) (Node 15 Empty (Node 20 Empty Empty)))


--(a)

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf n) = (Empty,Tip n)
splitFTree (No n e d) = (Node n (fst (splitFTree e)) (fst (splitFTree d)), Fork (snd (splitFTree e)) (snd (splitFTree d)))

--(b)

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Tip x) = Just (Leaf x)
joinTrees (Node r e d) (Fork a b) = Just (No r aux aux')
    where Just aux = joinTrees e a
          Just aux' = joinTrees d b
joinTrees _ _ = Nothing