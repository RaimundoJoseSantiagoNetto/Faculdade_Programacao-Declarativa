-----------------------------------------------------
-- Exemplos do capitulo 1 do livro
-----------------------------------------------------

\begin{code}

-----------------------------------------------------
-- Q.1-1
-- Objetivo: Calcula o quadrado de um numero
-----------------------------------------------------
quadrado :: Int -> Int
quadrado numero = numero * numero

-----------------------------------------------------
-- Q.1-2
-- Objetivo: Calcula o maior de dois numeros inteiros
-----------------------------------------------------
maior :: Int -> Int -> Int
maior a b 
  | a >= b = a
  | b > a = b

-----------------------------------------------------
-- Q.1-3
-- Objetivo: Calcula o maior de dois numeros inteiros (versao 2)
-----------------------------------------------------
maior2 :: Int -> Int -> Int
maior2 a b 
  | a >= b = a
  | otherwise = b

\end{code}