{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{--Exercicio 1

data Hora = H Int Int
        deriving Show

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

--(a)

etapaBemConstruida :: Etapa -> Bool
etapaBemConstruida (hi,hf) = horaValida hi && horaValida hf && hf `horaMaior` hi 

--Funcções auxiliares

horaValida :: Hora -> Bool
horaValida (H h m) = elem h [0..23] && elem m [0..59]

horaMaior:: Hora -> Hora -> Bool
horaMaior h1 h2
    |h1 < h2 = True
    |otherwise = False -}