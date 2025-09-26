-----------------------------------------------------
-- Q.01
-----------------------------------------------------

-- A)
dobrar :: [Int] -> [Int]
dobrar lista = map (*2) lista

-- B) 
ehPar :: Int -> Bool
ehPar valor = (mod valor 2) == 0

filtrarPares :: [Int] -> [Int]
filtrarPares lista = filter ehPar lista

-- C)
somarElementos :: [Int] -> Int
somarElementos lista = foldl (+) 0 lista

-- D)
filtrarParesEDobrar :: [Int] -> [Int]
filtrarParesEDobrar lista = dobrar (filtrarPares lista)

-- E)
multiplicarElementos :: [Int] -> Int
multiplicarElementos lista = foldl (*) 1 lista

-- F)
somaUmAosMaioresQueCinco :: [Int] -> [Int]
somaUmAosMaioresQueCinco lista = map (+1) (filter (>5) lista)

-----------------------------------------------------
-- Q.02
-- Objetivo: Verifica se todos os elementos de uma lista satisfazem um predicado
-----------------------------------------------------

myAll :: [a] -> (a -> Bool) -> Bool
myAll lista predicado = null (filter (not . predicado) lista)

-----------------------------------------------------
-- Q.03
-----------------------------------------------------

-- A)
-- Inverte uma lista
inverterLista :: [a] -> [a]
inverterLista [] = []
inverterLista (cabeca:cauda) = inverterLista cauda ++ [cabeca]

-- B) 
-- Remove o ultimo elemento da lista
removerUltimoElemento :: [a] -> [a]
removerUltimoElemento [e] = []
removerUltimoElemento (cabeca:cauda) = cabeca : removerUltimoElemento cauda

-- C)
-- Obter segundo elemento
obterSegundoElemento :: [a] -> a
obterSegundoElemento lista = lista !! 1

-----------------------------------------------------
-- Q.04
-- Objetivo: 
-----------------------------------------------------

-- A) [True,False,True,False,True]

-- B) [1,3,5]

-- C) [9,11,13,15]

-- D) [[7,2,3],[7,1,5,3]]

-- E) [[1],[2],[3],[4],[5]]

-- F) [2,4,6,8,10,12,14,16,18,20]

-- G) [3,5,7,9,11,13,15,17,19,21]

-- H) 2

-- I) -26

-- J) [2,4,6,8,10,12,14,16,18]

-- K) "lleksahavajnohtyp"

-- L) [[3,4],[5,6,7,8],[9,10]]

-----------------------------------------------------
-- Q.05
-- Objetivo: Concatena todas as listas de uma lista de listas
-----------------------------------------------------

concatenarListas :: [[a]] -> [a]
concatenarListas lista = foldl (++) [] lista

-----------------------------------------------------
-- Q.06
-- Objetivo: Aplica uma sequencia de operacoes a um unico valor
-----------------------------------------------------

mapIsh :: [(a -> a)] -> a -> [a]
mapIsh [] _ = []
mapIsh (cabeca:cauda) valor = cabeca valor : mapIsh cauda valor

-----------------------------------------------------
-- Q.07
-- Objetivo: Aplica uma funcao duas vezes a um valor
-----------------------------------------------------

aplicarDuasVezes :: (a -> a) -> a -> a
aplicarDuasVezes f x = f (f x)

-----------------------------------------------------
-- Q.08
-- Objetivo: Aplica a funcao sob a funcao
-----------------------------------------------------

compor :: (b -> c) -> (a -> b) -> a -> c
compor f g x = f(g x)

-----------------------------------------------------
-- Q.09
-- Objetivo: Recria a funcao map
-----------------------------------------------------

meuMap :: (a -> b) -> [a] -> [b]
meuMap _ [] = []
meuMap f (cabeca:cauda) = f cabeca : meuMap f cauda

-----------------------------------------------------
-- Q.10
-- Objetivo: Recria a funcao filter
-----------------------------------------------------

meuFilter :: (a -> Bool) -> [a] -> [a]
meuFilter _ [] = []
meuFilter predicado (cabeca:cauda)
  | predicado cabeca = cabeca : meuFilter predicado cauda
  | otherwise = meuFilter predicado cauda

-----------------------------------------------------
-- Q.11
-- Objetivo: Recria a funcao foldl
-----------------------------------------------------

meuFoldl :: (b -> a -> b) -> b -> [a] -> b
meuFoldl _ acc [] = acc
meuFoldl f acc (cabeca:cauda) = meuFoldl f (f acc cabeca) cauda

-----------------------------------------------------
-- Q.12
-- Objetivo: Recria a funcao zipWith
-----------------------------------------------------

zipWithFunc :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithFunc op lista1 [] = [] 
zipWithFunc op [] lista2 = []
zipWithFunc op (cabeca1:cauda1) (cabeca2:cauda2) = op cabeca1 cabeca2 : zipWithFunc op cauda1 cauda2

-----------------------------------------------------
-- Q.13
-- Objetivo: Reaplica uma funcao n vezes (e retorna uma funcao)
-----------------------------------------------------

repetirNVezes :: Int -> (a -> a) -> (a -> a)
repetirNVezes 0 f = id
repetirNVezes n f = f . repetirNVezes (n - 1) f

-----------------------------------------------------
-- Q.14
-- Objetivo: Ordena uma lista segundo uma regra passada como parametro
-- (Made with GPT)
-----------------------------------------------------

minhaOrdenacao :: (a -> a -> Ordering) -> [a] -> [a]
minhaOrdenacao _ []     = []
minhaOrdenacao cmp (x:xs) =
    let menoresOuIguais = [y | y <- xs, cmp y x /= GT]
        maiores         = [y | y <- xs, cmp y x == GT]
    in minhaOrdenacao cmp menoresOuIguais ++ [x] ++ minhaOrdenacao cmp maiores

-----------------------------------------------------
-- Q.15
-- Objetivo: Recria a funcao takeWhile
-----------------------------------------------------

meuTakeWhile :: (a -> Bool) -> [a] -> [a]
meuTakeWhile _ [] = []
meuTakeWhile predicado (cabeca:cauda)
  | predicado cabeca = cabeca : meuTakeWhile predicado cauda
  | otherwise = []

-----------------------------------------------------
-- Q.16
-- Objetivo: Recria a funcao dropWhile
-----------------------------------------------------

meuDropWhile :: (a -> Bool) -> [a] -> [a]
meuDropWhile _ [] = []
meuDropWhile predicado (cabeca:cauda)
    | predicado = meuDropWhile predicado cauda
    | otherwise = cabeca:cauda
