import Data.Char 

-----------------------------------------------------
-- Q.01
-- Objetivo: Ordena uma lista de inteiros, por Ordenacao por Selecao
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
-- Q.02
-- Objetivo: Ordena uma lista de inteiros, por Merge Sort
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
-- Q.03
-- Objetivo: Ordena uma lista de inteiros, por Quick Sort
-----------------------------------------------------
ordenarQuickSort :: [Int] -> [Int]
ordenarQuickSort [] = []
ordenarQuickSort (x:xs) = ordenarQuickSort [a | a <- xs, a <= x] ++ [x] ++ ordenarQuickSort [a | a <- xs, a > x]

--------------------------------------------------
-- Q.04
-- Objetivo: Conta as vogais de uma string
--------------------------------------------------
ehVogal :: Char -> Bool
ehVogal c = toLower c `elem` "aeiou"

contarVogais :: String -> Int
contarVogais [] = 0
contarVogais (cabeca:cauda)
    | ehVogal cabeca = 1 + contarVogais cauda
    | otherwise = contarVogais cauda

--------------------------------------------------
-- Q.05
-- Objetivo: Criptografar e descriptografar uma mensagem
--------------------------------------------------
criptografarMensagem :: String -> String
criptografarMensagem string = criptografarMensagem2 "" string
criptografarMensagem2 :: String -> String -> String
criptografarMensagem2 mensagem [] = mensagem
criptografarMensagem2 mensagem (cabeca:cauda) = criptografarMensagem2 (mensagem ++ [(cifra !! (elementIndex cabeca alfabeto))]) cauda
  where
    alfabeto = ['A', 'B' ..'Z'] ++ ['a', 'b' ..'z'] ++ [' ']
    cifra = ['D', 'E' .. 'Z'] ++ ['A', 'B', 'C'] ++ ['d', 'e' .. 'z'] ++ ['a', 'b', 'c'] ++ [' ']

descriptografarMensagem :: String -> String
descriptografarMensagem string = descriptografarMensagem2 "" string
descriptografarMensagem2 :: String -> String -> String
descriptografarMensagem2 mensagem [] = mensagem
descriptografarMensagem2 mensagem (cabeca:cauda) = descriptografarMensagem2 (mensagem ++ [(alfabeto !! (elementIndex cabeca cifra))]) cauda
  where
    alfabeto = ['A', 'B' ..'Z'] ++ ['a', 'b' ..'z'] ++ [' ']
    cifra = ['D', 'E' ..'Z'] ++ ['A', 'B', 'C'] ++ ['d', 'e' ..'z'] ++ ['a', 'b', 'c'] ++ [' ']

elementIndex :: Char -> [Char] -> Int
elementIndex char lista = elementIndex2 0 char lista
elementIndex2 :: Int -> Char -> [Char] -> Int
elementIndex2 cont char [] = (-1)
elementIndex2 cont char (cabeca:cauda)
  | cabeca == char = cont
  | otherwise = elementIndex2 (cont + 1) char cauda

--------------------------------------------------
-- Q.06
-- Objetivo: Inverte uma string
--------------------------------------------------
inverte :: String -> String
inverte [] = []
inverte (cabeca:cauda) = inverte cauda ++ [cabeca]

--------------------------------------------------
-- Q.07
-- Objetivo: Retorna uma string com as letras iniciais maiusculas
--------------------------------------------------

-- Objetivo: Retorna uma lista de nomes, com base na String de um nome completo de uma pessoa
splitString :: String -> [String]
splitString nome = splitString2 "" [] nome
splitString2 :: String -> [String] -> String -> [String]
splitString2 stringAuxiliar lista [] = lista ++ [stringAuxiliar]
splitString2 stringAuxiliar lista (cabeca:cauda)
  | cabeca == ' ' = splitString2 "" (lista ++ [stringAuxiliar]) cauda 
  | otherwise = splitString2 (stringAuxiliar ++ [cabeca]) lista cauda 

-- Objetivo: Deixa as letras inicias de uma lista de strings em maiusculo
initCharsUpper :: String -> String
initCharsUpper listaNomes = initChars2Upper "" (splitString listaNomes)
initChars2Upper :: String -> [String] -> String
initChars2Upper stringAuxiliar [] = stringAuxiliar
initChars2Upper stringAuxiliar (cabeca:cauda) = initChars2Upper (stringAuxiliar ++ [toUpper (head cabeca)] ++ tail cabeca ++ " ") cauda

--------------------------------------------------
-- Q.08
-- Objetivo: Faz a busca binaria em uma lista de inteiros
--------------------------------------------------
buscaBinaria :: [Int] -> Int -> Bool
buscaBinaria [] _ = False
buscaBinaria xs alvo
  | alvo == meio = True
  | alvo < meio  = buscaBinaria esquerda alvo
  | otherwise    = buscaBinaria direita alvo
  where
    meio    = xs !! (length xs `div` 2)
    esquerda = take (length xs `div` 2) xs
    direita  = drop (length xs `div` 2 + 1) xs

--------------------------------------------------
-- Q.09
-- Objetivo: Retorna o maior inteiro de uma lista de inteiros
--------------------------------------------------
maxIntLista :: [Int] -> Int
maxIntLista [] = 0
maxIntLista (cabeca:cauda) = maxIntLista2 cabeca (cabeca:cauda)
maxIntLista2 :: Int -> [Int] -> Int
maxIntLista2 max [] = max
maxIntLista2 max (cabeca:cauda)
  | cabeca >= max = maxIntLista2 cabeca cauda
  | otherwise = maxIntLista2 max cauda

--------------------------------------------------
-- Q.10
-- Objetivo: Retorna o maior e o menor valor de uma lista de inteiros
--------------------------------------------------
minEmax :: [Int] -> (Int, Int)
minEmax [] = (0, 0)
minEmax (cabeca:cauda) = minEmax2 (cabeca, cabeca) (cabeca:cauda)
minEmax2 :: (Int, Int) -> [Int] -> (Int, Int)
minEmax2 (min, max) [] = (min, max)
minEmax2 (min, max) (cabeca:cauda)
  | cabeca >= max = minEmax2 (min, cabeca) cauda
  | cabeca >= min = minEmax2 (cabeca, max) cauda

--------------------------------------------------
-- Q.11
-- Objetivo: Conta numeros e letras de uma string
--------------------------------------------------
contDigitLet :: String -> (Int, Int)
contDigitLet [] = (0, 0)
contDigitLet (cabeca:cauda) = contDigitLet2 (0, 0) (cabeca:cauda)
contDigitLet2 :: (Int, Int) -> String -> (Int, Int)
contDigitLet2 (digit, letras) [] = (digit, letras)
contDigitLet2 (digit, letras) (cabeca:cauda)
  | isDigit cabeca = contDigitLet2 (digit + 1, letras) cauda
  | isAlpha cabeca = contDigitLet2 (digit, letras + 1) cauda
  | otherwise = contDigitLet2 (digit, letras) cauda

--------------------------------------------------
-- Q.12
-- Objetivo: Verifica se eh palindromo
--------------------------------------------------
verificarPalindromo :: String -> Bool
verificarPalindromo str = str == (inverte str)

--------------------------------------------------
-- Q.13
-- Objetivo: Mostra o nome de uma pessoa resumido
--------------------------------------------------

-- Objetivo: Retira o primeiro e o último nome de uma string
popCantos :: String -> String
popCantos str = primeiroNome ++ " " ++ iniciaisIntermediarias ++ " " ++ ultimoNome
  where 
    lista = splitString str
    iniciaisIntermediarias = initChars nomesIntermediarios
    nomesIntermediarios = initString (dropString (splitString str))
    ultimoNome = lastString lista
    primeiroNome = firstString lista

-- Objetivo: Retorna uma lista de nomes sem o primeiro nome
dropString :: [String] -> [String]
dropString nome = tail nome

-- Objetivo: Retorna uma lista de nomes sem o ultimo nome
initString :: [String] -> [String]
initString listaNomes = initString2 [] listaNomes
initString2 :: [String] -> [String] -> [String]
initString2 lista [e] = lista
initString2 lista (cabeca:cauda) = initString2 (lista ++ [cabeca]) cauda

-- Objetivo: Retorna as iniciais dos nomes de uma lista de nomes
initChars :: [String] -> String
initChars listaNomes = initChars2 "" listaNomes
initChars2 :: String -> [String] -> String
initChars2 stringAuxiliar [] = stringAuxiliar
initChars2 stringAuxiliar (cabeca:cauda) = initChars2 (stringAuxiliar ++ [head (cabeca)] ++ ['.']) cauda

-- Objetivo: Retorna o ultimo nome de uma lista de nomes
lastString :: [String] -> String
lastString [] = ""
lastString (cabeca:cauda) 
  | cauda == [] = cabeca
  | otherwise = lastString cauda

-- Objetivo: Retorna o primeiro nome de uma lista de nomes
firstString :: [String] -> String
firstString (cabeca:cauda) = cabeca


--------------------------------------------------
-- Q.14
-- Objetivo: Intercala duas listas de inteiros
--------------------------------------------------

intercala :: [Int] -> [Int] -> [Int]
intercala lista1 lista2 = intercala2 0 lista1 lista2

intercala2 :: Int -> [Int] -> [Int] -> [Int]
intercala2 _ lista1 [] = lista1
intercala2 _ [] lista2 = lista2
intercala2 cont (cabeca1:cauda1) (cabeca2:cauda2) 
  | mod cont 2 == 0 = cabeca1 : intercala2 (cont + 1) (cauda1) (cabeca2:cauda2) 
  | otherwise = cabeca2 : intercala2 (cont + 1) (cabeca1:cauda1) (cauda2) 

--------------------------------------------------
-- Q.15
-- Objetivo: Soma os elementos de uma lista de inteiros
--------------------------------------------------

somarElementos :: [Int] -> Int
somarElementos [] = 0
somarElementos (cabeca:cauda) = cabeca + somarElementos cauda

--------------------------------------------------
-- Q.16
--------------------------------------------------

-- A) 2

-- B) 1

-- C) 2

-- D) 1

-- E) 0

-- F) 1

-- G) 2

--------------------------------------------------
-- Q.17
-- Objetivo: Retorna uma lista com todos os quadrados dos numeros impares de 1 a n (inclusivo)
--------------------------------------------------

quadradosImpares' :: Int -> [Int]
quadradosImpares' (-1) = []
quadradosImpares' num 
  | mod num 2 == 1 = (num ^ 2) : quadradosImpares' (num - 2) 
  | otherwise = quadradosImpares' (num - 1) 

quadradosImpares :: Int -> [Int]
quadradosImpares num = [x ^ 2 | x <- [1..num], mod x 2 == 1]

--------------------------------------------------
-- Q.18
-- Objetivo: Retorna uma lista com todos os divisores proprios de um numero
--------------------------------------------------

divisoresProprios :: Int -> [Int]
divisoresProprios num = [x | x <- [1..(num - 1)], mod num x == 0]

--------------------------------------------------
-- Q.19
-- Objetivo: Retorna uma string sem as vogais
--------------------------------------------------

semVogais :: String -> String
semVogais str = [x | x <- str, not(ehVogal x)]

--------------------------------------------------
-- Q.20
-- Objetivo: Retorna uma lista com o produto cartesianos de duas listas de inteiros seguindo o teste x < y
--------------------------------------------------

produtoFiltrado :: [Int] -> [Int] -> [(Int, Int)]
produtoFiltrado (x:s1) (y:s2) = [(x,y) | x <- (x:s1), y <- (y:s2) , x < y]

--------------------------------------------------
-- Q.21
-- Objetivo: Retorna uma lista com pares de elementos com os seus respectivos indices
--------------------------------------------------

indexar :: [a] -> [(Int, a)]
indexar lista = [(i, lista !! i) | i <- [0 .. (length lista - 1)]]

--------------------------------------------------
-- Q.22
-- Objetivo: Retorna uma lista com todos os numeros perfeitos de 1 a n
--------------------------------------------------

numerosPerfeitos :: Int -> [Int]
numerosPerfeitos num = [x | x <- [1..num], somarElementos(divisoresProprios x) == x]

--------------------------------------------------
-- Q.23
--------------------------------------------------

-- A) 
quadradosPares1a20 :: [Int]
quadradosPares1a20 = [x ^ 2 | x  <- [1 .. 20], mod x 2 == 0]

-- B) 
impares1a50 :: [Int]
impares1a50 = [x | x <- [1 .. 50], mod x 2 == 1]

-- C) 
multiplosDe3e5de1a100 :: [Int]
multiplosDe3e5de1a100 = [x | x <- [1 .. 100], mod x 3 == 0 && mod x 5 == 0] -- poderia ser mod x 15

-- D) 
divisiveisPor7de1a100 :: [Int]
divisiveisPor7de1a100 = [x | x <- [1 .. 100], mod x 7 == 0]

-- E) 
numerosPalindromos1a1000 :: [Int]
numerosPalindromos1a1000 = [x | x <- [1 .. 1000], verificarPalindromo(show x)]