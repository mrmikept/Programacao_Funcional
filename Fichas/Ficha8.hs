import Data.List
import Data.Char

--Exercicio 1

data Frac = F Integer Integer

--(a)

mdc :: Integer -> Integer -> Integer
mdc x y = last [ n | n <- [1..(min x y)] , x `mod` n == 0, y `mod` n == 0]

normaliza :: Frac -> Frac
normaliza (F x y) = F (xX `div` a) (yY `div` a)
    where a = mdc (abs x) (abs y) * (if x * y < 0 then (-1) else 1)
          xX | x < 0 = -x
             |otherwise = x
          yY | a < 0 = -y
             |otherwise = y

--(b)

instance Eq Frac where
    (F x y) == (F w z) = x * y == w * z

--(c)

instance Ord Frac where
    (F x y) <= (F w z) = x * y <= w * z

--(d)

instance Show Frac where
    show (F x y) =  "(" ++ show x ++ "/" ++ show y ++ ")"

--(e)

instance Num Frac where
    (F x y) + (F w z)
        |y == z = normaliza $ F (x + w) y
        |otherwise = normaliza $ F (z * z + y * w) (y * z)
    x - y = x + negate y
    (F x y) * (F a b) = F (x * a) (y * b)
    negate (F x y) = F (-x) y
    abs (F x y) = F (abs x) (abs y)
    signum (F x y)
        |x == 0 = 0
        |x * y > 0 = 1
        |otherwise = -1
    fromInteger x = F x 1

--(f)

dobro :: Frac -> [Frac] -> [Frac]
dobro = filter . (<) . (2 *)

--Exercicio 2

data Exp a = Const a
    | Simetrico (Exp a)
    | Mais (Exp a) (Exp a)
    | Menos (Exp a) (Exp a)
    | Mult (Exp a) (Exp a)

--(a)

instance Show a => Show (Exp a) where
    show (Const a) = show a
    show (Simetrico a) = "(-" ++ show a ++ ")"
    show (Mais x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (Menos x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
    show (Mult x y) = "(" ++ show x ++ " * " ++ show y ++ ")"

--(b)

valor :: (Num a) => Exp a -> a
valor (Const x) = x
valor (Simetrico x) = - (valor x)
valor (Mais x y) = valor x + valor y
valor (Menos x y) = valor x - valor y
valor (Mult x y) = valor x * valor y

instance (Num a,Eq a) => Eq (Exp a) where
    x == y = valor x == valor y

--(c)

instance (Num a, Eq a) => Num (Exp a) where
  x + y = Const (valor x + valor y)
  x - y = Const (valor x - valor y)
  x * y = Const (valor x * valor y)
  negate (Const a) = Const (-a)
  negate (Simetrico a) = a
  negate (Mais x y) = Mais (-x) (-y)
  negate (Menos x y) = Menos y x
  negate (Mult x y) = Mult (-x) y
  abs (Const a) = Const (abs a)
  abs (Simetrico a) = abs a
  abs (Mais x y) = abs (x + y)
  abs (Menos x y) = abs (x - y)
  abs (Mult x y) = abs (x * y)
  signum (Const a) = Const (if abs a == a then if a == 0 then 0 else 1 else (-1))
  signum (Simetrico a) = - signum a
  signum (Mais x y) = Const (if (abs x+y) == (x+y) then if (x+y) == 0 then 0 else 1 else (-1))
  signum (Menos x y) = Const (if (abs x-y) == (x-y) then if (x-y) == 0 then 0 else 1 else (-1))
  signum (Mult x y) = Const (if (abs x*y) == (x*y) then if (x*y) == 0 then 0 else 1 else (-1))
  fromInteger x = Const (fromInteger x)

  --Exercicio 3

data Movimento = Credito Float | Debito Float
data Data = D Int Int Int deriving Eq
data Extracto = Ext Float [(Data, String, Movimento)]

--(a)

instance Ord Data where
    compare (D dia1 mes1 ano1) (D dia2 mes2 ano2) 
        | ano1 > ano2 || ano1 == ano2 && (mes1 > mes2 || mes1 == mes2 && dia1 > dia2) = GT
        | ano1 == ano2 && mes1 == mes2 && dia1 == dia2 = EQ
        | otherwise = LT

--(b)

instance Show Data where
    show (D dia mes ano) = show dia ++ "/" ++ show mes ++ "/" ++ show ano

--(c)

ordena :: Extracto -> Extracto
ordena (Ext n l) = Ext n (sortBy (\(data1,_,_) (data2,_,_) -> compare data1 data2) l)

--(d)

instance Show Extracto where
    show (Ext n l) = "Saldo anterior: " ++ show n ++
                     "\n---------------------------------------" ++
                     "\nData       Descricao   Credito   Debito" ++
                     "\n---------------------------------------\n" ++ concatMap (\(dat,str,_) -> show dat ++ replicate (11 - length (show dat)) ' ' ++ map toUpper str ++ "    \n") l ++
                     "---------------------------------------" ++
                     "\nSaldo actual: " ++ show (saldo (Ext n l))


saldo :: Extracto -> Float
saldo (Ext x lm) = foldl (\acc (_,_,mov) -> case mov of Credito n -> acc + n
                                                        Debito n -> acc - n) x lm