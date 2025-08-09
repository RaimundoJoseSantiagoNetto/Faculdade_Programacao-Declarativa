-----------------------------------------------------
-- Exercicios do capitulo 1 do livro
-----------------------------------------------------

\begin{code}

-----------------------------------------------------
-- Q.03
-- Objetivo: Retorna o cubo de um numero inteiro
-----------------------------------------------------
cubo :: Int -> Int
cubo numero = numero * numero * numero

-----------------------------------------------------
-- Q.04
-- Objetivo: Retorna o cub de um numero inteiro, usando a funcao quadrado
-----------------------------------------------------
quadrado :: Int -> Int
quadrado numero = numero * numero

cubo2 :: Int -> Int
cubo2 numero = quadrado numero * numero

-----------------------------------------------------
-- Q.05
-- Objetivo: Retorna o menor valor de dois inteiros
-----------------------------------------------------
menor :: Int -> Int -> Int
menor a b 
  | a <= b = a
  | b < a = b

-----------------------------------------------------
-- Q.06
-- Objetivo: Retorna o maior valor de tres inteiros
-----------------------------------------------------
maior3 :: Int -> Int -> Int -> Int
maior3 a b c 
  | a > b && a > c = a
  | b > a && b > c = b
  | c > a && c > b = c

-----------------------------------------------------
-- Q.07
-- Objetivo: Retorna o maior valor de tres inteiros, usando a funcao maior
-----------------------------------------------------
maior :: Int -> Int -> Int
maior a b 
  | a >= b = a
  | b > a = b

maior3' :: Int -> Int -> Int -> Int
maior3' a b c 
  | maior a b > c = maior a b
  | otherwise = c

-----------------------------------------------------
-- Q.08
-- Objetivo: Muda o sinal de um numero inteiro
-----------------------------------------------------
negar :: Int -> Int
negar numero = (-1) * numero

\end{code}