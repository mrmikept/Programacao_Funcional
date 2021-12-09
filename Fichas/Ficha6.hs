{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
--Exercicio 1

data BTree a = Empty
             | Node a (BTree a) (BTree a)
        deriving Show

a1 = Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 (Node 6 Empty Empty) (Node 7 Empty Empty)) --Arvore Binaria de teste de altura 3.
a2 = Node 7 (Node 6 (Node 3 Empty Empty) (Node 5 Empty Empty)) (Node 4 (Node 2 Empty Empty) (Node 1 Empty Empty))
--(a)

altura :: BTree a -> Int
altura Empty = 0
altura (Node r e d) = 1 + max (altura e) (altura d)

{-A função max retorna o valor "maximo" de dois argumentos, logo como a função altura adiciona +1 ao valor a cada nível da árvore então a função max vê qual é o lado
maior da árvore(lado esquerdo e lado direito). Sem a função max o valor da altura ia dar o mal pois a função ia contar a altura do mesmo nível duas vezes.-}

--(b)

contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node r e d) = 1 + contaNodos e + contaNodos d

--(c)

folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node r Empty (Node r2 e d)) = 0
folhas (Node r (Node r2 e d) Empty) = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ e d) = folhas e + folhas d

--(d)

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune x (Node r e d) = Node r (prune (x-1) e) (prune (x-1) d)

--(e)

path :: [Bool] -> BTree a -> [a]   --False -> Esquerda / True -> Direita
path _ Empty = []
path [] (Node r e d) = [r]
path (h:t) (Node r e d)
    |h = r:path t d
    |not h = r:path t e

--(f)

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node r e d) = Node r (mirror d) (mirror e)

--(g)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node r1 e1 d1) (Node r2 e2 d2) = Node (f r1 r2) (zipWithBT f e1 e2) (zipWithBT f d1 d2)
zipWithBT _ _ _ = Empty

--(h)

unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (a,b,c) e d) = (Node a unzipD1 unzipE1,Node b unzipD2 unzipE2,Node c unzipD3 unzipE3)
    where (unzipD1,unzipD2,unzipD3) = unzipBT d
          (unzipE1,unzipE2,unzipE3) = unzipBT e

--Exercicio 2

--Consederemos a3 e a4 como árvores binarias de procura
a3 = Node 10 (Node 5 (Node 2 Empty Empty) (Node 8 Empty Empty)) (Node 42 (Node 30 Empty Empty) (Node 150 (Node 50 Empty Empty) Empty))
a4 = Node 5 (Node 3 (Node 1 Empty Empty) (Node 4 (Node 2 Empty Empty) Empty)) (Node 10 (Node 7 Empty Empty) (Node 15 Empty (Node 20 Empty Empty)))

--(a)

minimo :: Ord a => BTree a -> a  --O valor valor minimo está sempre no ramo da esquerda.
minimo (Node r Empty _) = r
minimo (Node r e _) = minimo e

--(b)

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node r Empty Empty) = Empty
semMinimo (Node r e d) = Node r (semMinimo e) d

--(c)

minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node r Empty _) = (r,Empty)
minSmin (Node r e d) = (a,Node r b d)
    where (a,b) = minSmin e

--(d)

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node r e d)
    |x < r = Node r (remove x e) d
    |x > r = Node r e (remove x d)
    |otherwise = aux x (Node r e d)
    where aux n (Node a b c) = case b of Empty -> c
                                         otherwise -> case c of Empty -> b
                                                                otherwise -> Node g b h
          (g,h) = minSmin d

--Exercicio 3

type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show {-Ord: ALuno "ordinario" / TE: Aluno trabalhado estudante / MEL: Melhoria-}
data Classificacao = Aprov Int
                    | Rep
                    | Faltou
    deriving Show
type Turma = BTree Aluno -- arvore binaria de procura (ordenada por numero)

--Exemplo do tipo aluno
--(15,"Mike",ORD,Aprov 20)

--Consideremos t1 como uma Turma
t1 :: Turma
t1 = Node (5,"Miguel",ORD,Aprov 14) (Node (3,"Clara",TE,Rep) (Node (1,"Antonio",MEL,Faltou) Empty Empty) (Node (4,"Marco",ORD,Aprov 10) (Node (2,"Filipe",TE,Aprov 18) Empty Empty) Empty)) (Node (10,"Silvia",ORD,Faltou) (Node (7,"Helder",TE,Rep) Empty Empty) (Node (15,"Mike",ORD,Aprov 20) Empty (Node (20,"Ana",MEL,Rep) Empty Empty)))
--(a)

inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum x (Node (n,nm,rg,cl) e d)
    |x == n = True
    |x < n = inscNum x e
    |x > n = inscNum x d

--(b)

inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome n (Node (_,nm,_,_) e d)
    |n == nm = True
    |otherwise = inscNome n e || inscNome n d

--(c)

trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (n,nm,rg,_) e d) = case rg of TE -> [(n,nm)] ++ trabEst e ++ trabEst d; otherwise -> trabEst e ++ trabEst d

--(d)

nota :: Numero -> Turma -> Maybe Classificacao
nota x (Node (n,_,_,cl) e d)
    |x == n = Just cl
    |x < n = nota x e
    |x > n = nota x d
nota _ _ = Nothing

--(e)
--Para termos a percentagem temos de saber o numero de alunos que faltou, dividir pelo numero total de alunos e multiplicar por 100 
percFaltas :: Turma -> Float
percFaltas turma = (somaFalta turma / totalAlunos turma) * 100
    where somaFalta Empty = 0 
          somaFalta (Node (_,_,_,cl) e d) = case cl of Faltou -> 1 + somaFalta e + somaFalta d; otherwise -> somaFalta d + somaFalta e
          totalAlunos Empty = 0
          totalAlunos (Node (_,_,_,_) e d) = 1 + totalAlunos e + totalAlunos d

--(f)
--Media das Notas é dado pela soma total de todas as notas a dividir pelo numero total de notas
mediaAprov :: Turma -> Float
mediaAprov turma = somaNotas turma / numNotas turma
    where somaNotas :: Turma -> Float
          somaNotas Empty = 0
          somaNotas (Node (_,_,_,Aprov nota) e d) = fromIntegral nota + somaNotas e + somaNotas d
          somaNotas (Node al e d) = somaNotas e + somaNotas d
          numNotas :: Turma -> Float
          numNotas (Node (_,_,_,cl) e d) = (case cl of Aprov nota -> 1; otherwise -> 0) + numNotas e + numNotas d
          numNotas _ = 0

--(g)
--Para calcular o racio de alunos aprovados por alunos avaliados temos de dividir o numero total de alunos aprovados pelo numero total de alunos avaliados(ou seja alunos aprovados e reprovados)
aprovAv :: Turma -> Float
aprovAv Empty = 0
aprovAv turma = alAP / alAV
    where (alAP,alAV) = aux turma
          aux Empty = (0,0)
          aux (Node (_,_,_,clas) l r) = case clas of Aprov nota -> (x+1,y) ; Rep -> (x,y+1) ; otherwise -> (x,y)
            where (x,y) = (c+e,d+f)
                  (c,d) = aux l
                  (e,f) = aux r