import Data.Char

--(1)

--(a)

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices n = auxElemIndices (n,0,0)

--Função aux.

auxElemIndices :: Eq a => (a,Int,Int) -> [a] -> [Int]
auxElemIndices _ [] = []
auxElemIndices (n,pa,pn) (h:t)
    |n == h = pa:auxElemIndices (n,pa+1,pa) t
    |otherwise = auxElemIndices (n,pa+1,pn) t

--(b)

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (h:t)
    |x == h = isSubsequenceOf xs t
    |x /= h = isSubsequenceOf (x:xs) t
    |otherwise = False

--(2)

data BTree a = Empty | Node a (BTree a) (BTree a)

--(a)

lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAP _ Empty = Nothing
lookupAP n (Node (a,b) e d)
    |n == a = Just b
    |n < a = lookupAP n d
    |otherwise = lookupAP n e

--(b)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ _ Empty = Empty
zipWithBT _ Empty _ = Empty
zipWithBT f (Node a e1 d1) (Node b e2 d2) = Node (f a b) (zipWithBT f e1 e2) (zipWithBT f d1 d2)

--(3)

digitAlpha :: String -> (String,String)
digitAlpha = foldr (\x (ns,ls) -> if isDigit x then (x:ns,ls) else if isAlpha x then (ns,x:ls) else (ns,ls)) ([],[])

--(4)

data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)

--(a)

firstSeq :: Seq a -> a 
firstSeq (Cons a seq) = a
firstSeq (App Nil seq) = firstSeq seq
firstSeq (App seq _) = firstSeq seq

--(b)
{-
dropSeq :: Int -> Seq a -> Seq a
dropSeq _ Nil = Nil
dropSeq n (Cons a seq) = dropSeq (n-1) seq
dropSeq n (App seq1 seq1)
    | -}