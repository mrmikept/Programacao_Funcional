{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Ficha1

--Exercicio 1


type Etapa = (Ficha1.Hora',Ficha1.Hora')
type Viagem = [Etapa]

--Exemplo Hora = (H Int Int) = (H 9 30)
--Exemplo Etapa = ((Hora Partida),(Hora Chegada)) = (H 9 30 , H 10 25)
--Exemplo: Viagem = [(Etapa1),(Etapa2),(...)] = [(H 9 30, H 10 25), (H 11 20, H 12 45), (H 13 30, H 14 45)]

--Consideremos v1 como a seguinte viagem:

v1 = [(H 9 30, H 10 25), (H 11 20, H 12 45), (H 13 30, H 14 45)]

--(a)

etapaBCons :: Etapa -> Bool
etapaBCons (hp,hc) = Ficha1.horaValida' hp && Ficha1.horaValida' hc  && horaDepois hp hc

--Função Auxiliar

horaDepois :: Hora' -> Hora' -> Bool
horaDepois (H hp mp) (H hc mc)
        |hp < hc || hp == hc && mp < mc = True
        |otherwise = False

--(b)

viagemBCons :: Viagem -> Bool
viagemBCons [] = True
viagemBCons [e] = etapaBCons e
viagemBCons ((H hp1 mp1,H hc1 mc1):(H hp2 mp2,H hc2 mc2):t)
        |etapaBCons (H hp1 mp1,H hc1 mc1) && etapaBCons (H hp2 mp2,H hc2 mc2) && horaDepois (H hc1 mc1) (H hp2 mp2) && viagemBCons ((H hp2 mp2,H hc2 mc2):t) = True
        |otherwise = False

--(c)

duracaoViagem :: Viagem -> Etapa
duracaoViagem [(h1,h2)] = (h1,h2)
duracaoViagem ((h1,_):(_,h4):t) = duracaoViagem ((h1,h4):t)

--(d)

tempoViagem :: Viagem -> Hora'
tempoViagem [(h1,h2)] = difhora' h1 h2
tempoViagem ((h1,h2):t) = addmin' (difhora' h1 h2) (horapmin' (tempoViagem t))


--(e)

tempoEspera :: Viagem -> Hora'
tempoEspera [(h1,h2)] = H 0 0
tempoEspera ((h1,h2):(h3,h4):t) = addmin' (difhora' h2 h3) (horapmin' (tempoEspera ((h3,h4):t)))

--(f)

tempoTViagem :: Viagem -> Hora'
tempoTViagem [] = H 0 0
tempoTViagem v = minphora'(horapmin'(tempoViagem v) + horapmin' (tempoEspera v))

--Exercicio 2

--data Figura = Circulo Ponto Double | Retangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)
--data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)
type Poligonal = [Ponto]

--(a)

comprimentoLinha :: Poligonal -> Double
comprimentoLinha p
        |null p || length p == 1 = 0
        |otherwise = dist' (head p) (head (tail p)) + comprimentoLinha (tail p)

--(b)

linhaFechada :: Poligonal -> Bool
linhaFechada p = head p == last p

--(c)

triangula :: Poligonal -> [Figura]
triangula [p1,p2,p3] = [Triangulo p1 p2 p3]
triangula (p1:p2:p3:t) = Triangulo p1 p2 p3 : triangula (p1:p3:t)

--(d)

areaPolg :: Poligonal -> Double
areaPolg = sum . map area . triangula

--(e)

mover :: Poligonal -> Ponto -> Poligonal
mover = flip ( : )


--(f)

zoom :: Double -> Poligonal -> Poligonal
zoom z [p1,Cartesiano a b] = [p1,Cartesiano (z*a) (z*b)]
zoom z (p1:(Cartesiano a b):pol) = p1:zoom z (p3:pol)
    where p3 = Cartesiano (z*a) (z*b)
zoom _ p = p

--Exercicio 3

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]

--(a)

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email agenda = agenda ++ [(nome,[Email email])]

--(b)

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails nome [(n,cont)] = if nome == n then Just (map (\ (Email e) -> e) cont) else Nothing
verEmails nome ((n,cont):t) = if nome == n then verEmails nome [(n,cont)] else verEmails nome t

--(c)

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (h:t) = case h of Casa n -> n:consTelefs t
                             Trab n -> n:consTelefs t
                             Tlm n -> n:consTelefs t
                             Email n -> consTelefs t

--(d)

casa :: Nome -> Agenda -> Maybe Integer
casa nome [(n,c:t)] = if nome == n then case c of Casa n -> Just n
                                                  _ -> casa nome [(n,t)]
                                    else Nothing
casa nome ((n,c):t) = if nome == n then casa nome [(n,c)] else casa nome t

--Exercicio 4

type Dia = Int
type Mes = Int
type Ano = Int
--type Nome = String

data Data = D Dia Mes Ano deriving Show

type TabDN = [(Nome,Data)]

--Consideremos a seguinte TabDN
tbDN = [("Mike",D 15 07 1999),("Renata",D 30 10 1997),("Filipe",D 03 07 1999)]
--(a)

procura :: Nome -> TabDN -> Maybe Data
procura _ [] = Nothing
procura nome ((n,d):t) = if nome == n then Just d else procura nome t

--(b)

idade :: Data -> Nome -> TabDN -> Maybe Int
idade _ _ [] = Nothing
idade (D da ma aa) nome ((n,D dn mn an):t) = if nome == n then if ma > mn || ma == mn && da > dn then Just (aa - an) else Just ((aa - an) - 1) else idade (D da ma aa) nome t

--(c)

anterior :: Data -> Data -> Bool
anterior (D d1 m1 a1) (D d2 m2 a2)
        |a1 < a2 || a1 == a2 && m1 < m2 || a1 == a2 && m1 == m2 && d1 < d2 = True
        |otherwise = False

--(d)

ordena :: TabDN -> TabDN
ordena [] = []
ordena ((n,d):t) = insere (n,d) (ordena t)
        where insere (n,d) [] = [(n,d)]
              insere (n,d) ((nx,dx):t) | anterior dx d = (nx,dx):insere (n,d) t
                                       | otherwise = (n,d):(nx,dx):t

--(e)

porIdade :: Data -> TabDN -> [(Nome,Int)]
porIdade _ [] = []
porIdade (D d m a) tab = (n,idade) : porIdade (D d m a) t
        where ((n,D dx mx ax):t) = ordena tab
              idade = if m > mx || mx == m && d > dx then a - ax else ( a - ax ) - 1

--Exercicio 5

data Movimento = Credito Float | Debito Float deriving Show

--data Data = D Int Int Int (Ja esta defenido em cima!)

data Extracto = Ext Float [(Data, String, Movimento)] deriving Show

--(a)

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext _ []) _ = []
extValor (Ext v ((d, nome, mov):t)) valor = case mov of Credito x -> if x >= valor then mov : extValor (Ext v t) valor else extValor (Ext v t) valor
                                                        Debito x -> if x >= valor then mov : extValor (Ext v t) valor else extValor (Ext v t) valor

--(b)

filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext _ []) _ = []
filtro (Ext v ((d, desc,mov):t)) str = if desc `elem` str then (d,mov):filtro (Ext v t) str else filtro (Ext v t) str

--(c)

creDeb :: Extracto -> (Float,Float)
creDeb ext = (credito ext , debito ext)
        where credito (Ext v ((d, desc,mov):t)) = case mov of Credito x -> x + credito (Ext v t)
                                                              Debito x -> credito (Ext v t)
              debito (Ext v ((d, desc,mov):t)) = case mov of Credito x -> debito (Ext v t)
                                                             Debito x -> x + debito (Ext v t)

--(d)

saldo :: Extracto -> Float
saldo (Ext x lm) = foldl (\acc (_,_,mov) -> case mov of Credito n -> acc + n
                                                        Debito n -> acc - n) x lm
