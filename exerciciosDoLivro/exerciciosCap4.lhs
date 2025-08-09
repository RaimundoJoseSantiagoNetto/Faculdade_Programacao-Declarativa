-----------------------------------------------------
-- Exercicios do capitulo 4 do livro
-----------------------------------------------------

\begin{code}

import Data.Char(ord)
import Data.Char(chr)

-----------------------------------------------------
-- Q.04
-- Objetivo: Retorna a cauda de uma lista de inteiros
-----------------------------------------------------
caudaLista :: [Int] -> [Int]
caudaLista (cabeca:cauda) = cauda

-----------------------------------------------------
-- Q.05
-- Objetivo: Retorna a soma dos numeros inteiros de uma lista
-----------------------------------------------------
somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (cabeca:cauda) = cabeca + somaLista cauda 

-----------------------------------------------------
-- Q.06
-- Objetivo: Retorna o produto dos numeros de uma lista de inteiros
-----------------------------------------------------
produtoLista :: [Int] -> Int
produtoLista [] = 1
produtoLista (cabeca:cauda) = cabeca * produtoLista cauda 

-----------------------------------------------------
-- Q.07
-- Objetivo: Retorna o maior numero de uma lista de inteiros
-----------------------------------------------------
maiorLista :: [Int] -> Int
maiorLista lista = maiorSent (-1) lista
maiorSent :: Int -> [Int] -> Int
maiorSent sent [] = sent
maiorSent sent (cabeca:cauda) 
  | cabeca >= sent = maiorSent cabeca cauda
  | otherwise = maiorSent sent cauda

-----------------------------------------------------
-- Q.08
-- Objetivo: Verifica se um numero esta ou nao na lista 
-----------------------------------------------------
membro :: [Int] -> Int -> Bool
membro l num = membroBuscar False l num
membroBuscar :: Bool -> [Int] -> Int -> Bool
membroBuscar controle [] num = False
membroBuscar controle (cabeca:cauda) num 
  | cabeca == num = True
  | otherwise = membroBuscar False cauda num

-----------------------------------------------------
-- Q.09
-- Objetivo: Retorna o ultimo caracter de uma String
-----------------------------------------------------
ultimo :: [Char] -> Char
ultimo (cabeca:cauda)  
  | length cauda == 0 = cabeca
  | otherwise = ultimo cauda

-----------------------------------------------------
-- Q.10
-- Objetivo: Calcula a quantidade de ocorrencia de um numero de uma lista de inteiros
-----------------------------------------------------
membroNum :: [Int] -> Int -> Int
membroNum l num = membroBuscarNum 0 l num
membroBuscarNum :: Int ->[Int] -> Int -> Int
membroBuscarNum cont [] num = cont
membroBuscarNum cont (cabeca:cauda) num 
  | cabeca == num = membroBuscarNum (cont + 1) cauda num
  | otherwise = membroBuscarNum cont cauda num

-----------------------------------------------------
-- Q.11
-- Objetivo: Descobrir a funcao do metodo da questao (soma os elemtos de uma lista de inteiros)
-----------------------------------------------------
misterio :: [Int] -> Int
misterio xs = misterio2 0 xs
misterio2 :: Int -> [Int] -> Int
misterio2 a [] = a
misterio2 a (x:xs) = misterio2 (a + x) xs

-----------------------------------------------------
-- Q.12
-- Objetivo: Retorna a conjuncao de uma lista de booleanos
-----------------------------------------------------
operadorE :: [Bool] -> Bool
operadorE lista = operadorE2 False lista 
operadorE2 :: Bool -> [Bool] -> Bool
operadorE2 operador [] = operador
operadorE2 operador (cabeca:cauda) 
  | cabeca == True = operadorE2 True cauda
  | otherwise = False

-----------------------------------------------------
-- Q.13
-- Objetivo: Concatenar lista de listas de inteiros
-----------------------------------------------------
concatenar :: [[Int]] -> [Int]
concatenar listaDeListas = concatenar2 [] listaDeListas
concatenar2 :: [Int] -> [[Int]] -> [Int]
concatenar2 listaConcatenada [] = listaConcatenada
concatenar2 listaConcatenada (cabeca:cauda) = concatenar2 (listaConcatenada ++ cabeca) cauda

-----------------------------------------------------
-- Q.14
-- Objetivo: Verifica se uma lista de inteiros esta ordenada 
-----------------------------------------------------
ordenada :: [Int] -> Bool
ordenada (cabeca:cauda) = ordenada2 cabeca cauda
ordenada2 :: Int -> [Int] -> Bool
ordenada2 num [] = True
ordenada2 num (cabeca:cauda)
  | cabeca < num = False
  | otherwise = ordenada2 cabeca cauda

-----------------------------------------------------
-- Q.15
-- Objetivo: Converte todos os caracteres de uma String para minuscula
-----------------------------------------------------
paraMinuscula :: String -> String
paraMinuscula string = paraMinuscula2 "" string
paraMinuscula2 :: String -> String -> String
paraMinuscula2 transformada [] = transformada
paraMinuscula2 transformada (cabeca:cauda) = paraMinuscula2 (transformada ++ [paraMinusculaChar cabeca]) cauda

-- codigos das letras maiusculas 65 - 90
-- codigos das letras minusculas 97 - 122

paraMinusculaChar :: Char -> Char
paraMinusculaChar char = do
  let incremento = ord(char) - 65
  chr(97 + incremento)

-----------------------------------------------------
-- Q.16
-- Objetivo: Substitui todas as ocorrencias de um caracter por outro
-----------------------------------------------------
substituir :: String -> Char -> Char -> String
substituir string caracterAntigo caracterNovo = substituir2 "" string caracterAntigo caracterNovo
substituir2 :: String -> String -> Char -> Char -> String
substituir2 transformada [] caracterAntigo caracterNovo = transformada
substituir2 transformada (cabeca:cauda) caracterAntigo caracterNovo
  | cabeca == caracterAntigo = substituir2 (transformada ++ [caracterNovo]) cauda caracterAntigo caracterNovo
  | otherwise = substituir2 (transformada ++ [cabeca]) cauda caracterAntigo caracterNovo

-----------------------------------------------------
-- Q.17
-- Objetivo: Retorna uma lista com todos os dividores de um numero
-----------------------------------------------------
divisores :: Int -> [Int]
divisores num = divisores2 [] num num
divisores2 :: [Int] -> Int -> Int -> [Int]
divisores2 listaDeDivisores 0 num = listaDeDivisores 
divisores2 listaDeDivisores divisor num 
  | mod num divisor == 0 = divisores2 (listaDeDivisores ++ [divisor]) (divisor - 1) num 
  | otherwise = divisores2 (listaDeDivisores) (divisor - 1) num 

-----------------------------------------------------
-- Q.18
-- Objetivo: Retorna uma lista de inteiros ordenada com todos os elementos intercalados
-----------------------------------------------------
intercala :: [Int] -> [Int] -> [Int]
intercala lista1 lista2 = intercala2 [] 1 lista1 lista2
intercala2 :: [Int] -> Int -> [Int] -> [Int] -> [Int]
intercala2 listaAuxiliar cont lista1 [] = listaAuxiliar ++ lista1
intercala2 listaAuxiliar cont [] lista2 = listaAuxiliar ++ lista2
intercala2 listaAuxiliar cont (cabeca1:cauda1) (cabeca2:cauda2)
  | mod cont 2 == 1 = intercala2 (listaAuxiliar ++ [cabeca1]) (cont + 1) cauda1 (cabeca2:cauda2)
  | mod cont 2 == 0 = intercala2 (listaAuxiliar ++ [cabeca2]) (cont + 1) (cabeca1:cauda1) cauda2

-----------------------------------------------------
-- Q.19a
-- Objetivo: Ordena uma lista de inteiros, por Ordenacao por Insercao
-- (feito pelo GPT)
-----------------------------------------------------
ordenarInsercao :: [Int] -> [Int]
ordenarInsercao [] = []
ordenarInsercao (x:xs) = insere x (ordenarInsercao xs)
  where
    insere n [] = [n]
    insere n (y:ys)
      | n <= y    = n : y : ys
      | otherwise = y : insere n ys

-----------------------------------------------------
-- Q.19b
-- Objetivo: Ordena uma lista de inteiros, por Ordenacao por Selecao
-- (feito pelo GPT)
-----------------------------------------------------
ordenarSelecao :: [Int] -> [Int]
ordenarSelecao [] = []
ordenarSelecao xs = menor : ordenarSelecao (remove menor xs)
  where
    menor = minimum xs
    remove _ [] = []
    remove n (y:ys)
      | n == y    = ys
      | otherwise = y : remove n ys

-----------------------------------------------------
-- Q.19c
-- Objetivo: Ordena uma lista de inteiros, por Merge Sort
-- (feito pelo GPT)
-----------------------------------------------------
ordenarMergeSort :: [Int] -> [Int]
ordenarMergeSort []  = []
ordenarMergeSort [x] = [x]
ordenarMergeSort xs  = merge (ordenarMergeSort esquerda) (ordenarMergeSort direita)
  where
    metade   = length xs `div` 2
    esquerda = take metade xs
    direita  = drop metade xs

    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y    = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

-----------------------------------------------------
-- Q.19d
-- Objetivo: Ordena uma lista de inteiros, por Quick Sort
-- (feito pelo GPT)
-----------------------------------------------------
ordenarQuickSort :: [Int] -> [Int]
ordenarQuickSort [] = []
ordenarQuickSort (x:xs) =
  ordenarQuickSort [a | a <- xs, a <= x]
  ++ [x] ++
  ordenarQuickSort [a | a <- xs, a > x]

-----------------------------------------------------
-- Q.20a
-- Objetivo: Dobra os numeros de uma lista de inteiros
-----------------------------------------------------
dobrar :: [Int] -> [Int]
dobrar lista = [2*x | x <- lista, True] -- o True pode ser omitido, se tirar a virgula

-----------------------------------------------------
-- Q.20b
-- Objetivo: Filtra os digitos de uma String
-----------------------------------------------------

-- codigos dos digitos (0 a 9) 48 - 57
filtrar :: String -> String
filtrar string = [char | char <- string, not(ord char >= 48 && ord char <= 57)] 

-----------------------------------------------------
-- Q.20c
-- Objetivo: Remove os numeros negativos de uma lista de inteiros
-----------------------------------------------------
removeNegativos :: [Int] -> [Int]
removeNegativos lista = [x | x <- lista, x >= 0]

\end{code}
