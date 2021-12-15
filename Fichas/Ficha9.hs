{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.Random
import Data.Char
import Data.List
import Data.Maybe
import Data.String

--Exercicio 1

--(a)

bingo :: IO ()
bingo = do acc <- sorteio []
           print acc

sorteio :: [Int] -> IO [Int]
sorteio l
    |length l == 90 = return l
    |otherwise = do x <- randomRIO (1,90)
                    print x
                    getChar
                    let acc = if x `elem` l then l else x:l in sorteio acc

--(b)

mastermind :: IO ()
mastermind = do (n1,n2,n3,n4) <- key
                print "Insere uma sequencia de 4 digitos!"
                compGKey (n1,n2,n3,n4)
                return ()

key :: IO (Int,Int,Int,Int)
key = do n1 <- randomRIO (0,9)
         n2 <- randomRIO (0,9)
         n3 <- randomRIO (0,9)
         n4 <- randomRIO (0,9)
         return (n1,n2,n3,n4)

myGuess :: IO (Int,Int,Int,Int)
myGuess = do x <- getLine
             if length x /= 4 || not (all isDigit x)
             then do
                print "Eu disse uma sequencia de QUATRO DIGITOS, DUMB ASS..."
                myGuess
             else return(let (a:b:c:d:xs) = x in (read [a],read [b],read [c],read [d]))

compGKey :: (Int,Int,Int,Int) -> IO ()
compGKey (n1,n2,n3,n4) = do
    let listnumbs = [n1,n2,n3,n4]
    (g1,g2,g3,g4) <- myGuess
    let numCPC = 0 + (if n1 == g1 then 1 else 0) + (if n2 == g2 then 1 else 0) + (if n3 == g3 then 1 else 0) + (if n4 == g4 then 1 else 0)
    let numCPE = 0 + (if n1 /= g1 && g1 `elem` listnumbs \\ [g2,g3,g4] then 1 else 0) +
                     (if n2 /= g2 && g2 `elem` listnumbs \\ [g3,g4] then 1 else 0) +
                     (if n3 /= g3 && g3 `elem` listnumbs \\ [g4] then 1 else 0) +
                     (if n4 /= g4 && g4 `elem` listnumbs \\ [] then 1 else 0)
    if numCPC == 4 then print "Finalmente ganhaste... Tava a ficar super aborrecido... Parabens tho!" else print $ "Valores Corretos na posicao Correta: " ++ show numCPC ++ "  Valores Corretos na posicao Errada: " ++ show numCPE
    if numCPC == 4 then return () else do print "Try again LOSER"
                                          compGKey (n1,n2,n3,n4)

{-Para fazer o mastermind temos de fazer "4" funcoes: uma função "main" que corre as outras 3 funcoes(mastermind), uma função que gera a sequencia de 4 numeros que 
queres adivinhar(key), outra que le a sequencia de digitos que o utilizador insere e que verifica se e uma funcao valida(ou seja se tem 4 elementos e se todos sao digitos)(myguess)
e outra funcao que compara a key gerada com o guess do utilizador dando como output: se a sequancia for igual entao diz que venceste, se for diferente verifica se existem
numeros pertencentes a chave na posicao correta e se existem numeros pertencentes a chave na possicao errada.

Para a funcao key usamos a funcao randomRIO para gerar numeros de 0 a 9 quatro vezes para gerar a chave.

Na funcao myGuess verificamos se a chave inserida(atraves da funcao getLine) se ela tem 4 elementos e se todos esses elementos sao Digitos

Na funcao compGKey(Comparar Guess do utilizador com a key gerada) usamos outras duas funcoes, numCPC(numero correto possicao correta) para somar quantos numeros estao
na posicao correta e a funcao numCPE(numero correto possicao errada) para somar quantos numeros inseridos pertencem a chave mas estao em posicoes erradas.-}

--Exercicio 2

data Aposta = Ap [Int] (Int,Int)

--(a)

valida :: Aposta -> Bool
valida (Ap l (e1,e2)) =(length l == 5 && length (e1 : [e2]) == 2) && verificaNum l [1..50] && verificaEst ([e1] ++ [e2]) [1..7]
                        where verificaNum [] _ = True
                              verificaNum (h:t) l = h `elem` l && verificaNum t (delete h l)
                              verificaEst [] _ = True
                              verificaEst (h:t) l = h `elem` l && verificaEst t (delete h l)

--(b)

comuns :: Aposta -> Aposta -> (Int,Int)
comuns (Ap l1 (e1,e2)) (Ap l2 (e3,e4)) = (ncomum l1 l2 , ecomum [e1,e2] [e3,e4])
    where ncomum [] _ = 0
          ncomum (h:t) l2 = if h `elem` l2 then 1 + ncomum t l2 else ncomum t l2
          ecomum [] _ = 0
          ecomum (h:t) l2 = if h `elem` l2 then 1 + ecomum t l2 else ncomum t l2

--(c)

--(i)

instance Eq Aposta where
    (==) a b = comuns a b == (5,2)

--(ii)

premio :: Aposta -> Aposta -> Maybe Int
premio ap ch = case comuns ap ch of (5,n) -> Just (3-n)
                                    (4,n) -> Just (6-n)
                                    (3,n) -> Just (10 - n - (if n == 2 then 1 else 0))
                                    (2,2) -> Just 8
                                    (2,n) -> Just (13-n)
                                    (1,2) -> Just 11
                                    _ -> Nothing

--(d)

--(i)
leAposta :: IO Aposta
leAposta = do
    print "insira os 5 numeros da aposta: "
    numeros <- getLine
    print "insira as 2 estrelas da sua aposta: "
    estrelas <- getLine
    let aposta = Ap (map read (words numeros)) (let (a:b:r) = words estrelas in (read a, read b))
    if valida aposta then do return aposta
    else do
        print "Aposta com formato invalido, tente novamente!"
        leAposta

--(ii)

joga :: Aposta -> IO ()
joga ch = do
    ap <- leAposta
    print ((++) "Premio: " $ show $ fromMaybe 0 (premio ap ch))

--(e)

geraChave :: IO Aposta
geraChave = do
    numeros <- gera 'N' []
    [e1,e2] <- gera 'E' []
    return (Ap numeros (e1,e2))

gera :: Char -> [Int] -> IO [Int]
gera c l = do
    n <- randomRIO (1, if c == 'N' then 50 else 12)
    if length l == 5 && c == 'N' || length l == 2 && c == 'E' then return l
    else if n `elem` l then gera c l else gera c (n:l)

--(f)

mainEuro :: IO ()
mainEuro = do ch <- geraChave
              ciclo ch

ciclo :: Aposta -> IO ()
ciclo ch = do
    menuOpc <- menu
    case menuOpc of "1" -> do joga ch; ciclo ch
                    "2" -> do putStrLn "Nova chave generada"; mainEuro
                    "0" -> return ()

menu :: IO String
menu = do { putStrLn menutxt
          ; putStr "Opcao: "
          ; c <- getLine
          ; return c
          }
    where menutxt = unlines ["",
                             "Apostar ........... 1",
                             "Gerar nova chave .. 2",
                             "",
                             "Sair .............. 0"]
