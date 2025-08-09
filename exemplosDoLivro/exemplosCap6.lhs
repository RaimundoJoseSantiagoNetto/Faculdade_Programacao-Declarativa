-----------------------------------------------------
-- Exemplos do capitulo 6 do livro
-----------------------------------------------------

\begin{code}

-----------------------------------------------------
-- Ex.6-1
-- Objetivo: Duplica os valores de uma lista
-----------------------------------------------------
duplica :: Int -> Int
duplica n = n + n
duplicaLst :: [Int] -> [Int]
duplicaLst [] = []
duplicaLst (este:aqueles) = (duplica este) : (duplicaLst aqueles)

-----------------------------------------------------
-- Ex.6-2
-- Objetivo: Eleva ao quadrado os elementos de uma lista
-----------------------------------------------------
quadra :: Int -> Int
quadra n = n * n

quadraTodos :: [Int] -> [Int]
quadraTodos [] = []
quadraTodos (este:aqueles) = (quadra este) : (quadraTodos aqueles)

-----------------------------------------------------
-- Ex.6-3
-- Objetivo: Funcao de Alta ordem mapInt
-----------------------------------------------------
mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt f [] = []
mapInt f (este : aqueles) = (f este) : (mapInt f aqueles)

-----------------------------------------------------
-- Ex.6-4
-- Objetivo: Duplica os valores de uma lista
-----------------------------------------------------
duplicaTodos :: [Int] -> [Int]
duplicaTodos lista = mapInt duplica lista

-----------------------------------------------------
-- Ex.6-5
-- Objetivo: Retira os numeros pares de uma lista de inteiros
-----------------------------------------------------
ehPar :: Int -> Bool
ehPar n = (mod n 2 == 0)

pares :: [Int] -> [Int]
pares [] = []
pares (h:c) = 
  if(ehPar h) then h:(pares c)
  else pares c

-----------------------------------------------------
-- Ex.6-6
-- Objetivo: Retira os numeros impares de uma lista de inteiros
-----------------------------------------------------
ehImpar :: Int -> Bool
ehImpar n = (mod n 2 == 1)

impares :: [Int] -> [Int]
impares [] = []
impares (h:c) =
  if(ehImpar h) then h:(impares c)
  else impares c
 
-----------------------------------------------------
-- Ex.6-7
-- Objetivo: Funcao de Alta ordem filtro
-----------------------------------------------------
filtro :: (Int -> Bool) -> [Int] -> [Int]
filtro p [] = []
filtro p (h:c) = 
  if(p h) then h:(filtro p c)
  else filtro p c

-----------------------------------------------------
-- Ex.6-8
-- Objetivo: Modificacao das funcoes pares e impares
-----------------------------------------------------
pares2 :: [Int] -> [Int]
pares2 lst = filtro ehPar lst

impares2 :: [Int] -> [Int]
impares2 lst = filtro ehImpar lst

-----------------------------------------------------
-- Ex.6-9
-- Objetivo: Retorna o primeiro elemento de um par de inteiros
-- e Retina o primeiro elemento de um par de caracteres
-----------------------------------------------------
intPrim :: (Int, Int) -> Int
intPrim (l, _) = l

carPrim :: (Char, Char) -> Char
carPrim (l, _) = l
-----------------------------------------------------
-- Ex.6-10
-- Objetivo: Retorna o primeiro elemento de um par
-----------------------------------------------------
prim :: (a, b) -> a
prim (valor, _) = valor

-----------------------------------------------------
-- Ex.6-11
-- Objetivo: Calcula o numero de elementos de uma lista de inteiros
-----------------------------------------------------
tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (h:c) = 1 + tamanho c

-----------------------------------------------------
-- Ex.6-12
-- Objetivo: Calcula o numero de elementos de uma lista
-----------------------------------------------------
tamanho :: [a] -> Int
tamanho [] = 0
tamanho (h:c) = 1 + tamanho c

\end{code}