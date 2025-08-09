-----------------------------------------------------
-- Exercicios do capitulo 2 do livro
-----------------------------------------------------

\begin{code}

import Data.Char(ord)
import Data.Char(chr)

nOr :: Bool -> Bool -> Bool
nOr p q = not(p || q)

tresDiferentes :: Int -> Int -> Int -> Bool
tresDiferentes num1 num2 num3 = (num1 /= num2) && (num1 /= num3) && (num3 /= num2)

areaT :: Float -> Float -> Float
areaT b h = b * h / 2

areaC :: Float -> Float
areaC r = pi * r ^ 2

converterTemperatura :: Float -> Float 
converterTemperatura f | f == 32 = 0 | otherwise = 5 / 9 * (f - 32)

-- codigos das letras maiusculas 65 - 90
-- codigos das letras minusculas 97 - 122

paraMinuscula :: Char -> Char
paraMinuscula char = do
  let incremento = ord(char) - 65
  chr(97 + incremento)

paraMaiuscula :: Char -> Char
paraMaiuscula char = do 
  let incremento = ord(char) - 97
  chr(65 + incremento)

-----------------------------------------------------
-- Q.09
-- Objetivo: Calcula a media de tres valores
-----------------------------------------------------
media :: Float -> Float -> Float -> Float
media nota1 nota2 nota3 = (nota1 + nota2 + nota3) / 3 

-----------------------------------------------------
-- Q.10
-- Objetivo: Retorna a posicao cartesiana com base em duas coordenadas
-----------------------------------------------------
posicao :: Float -> Float -> String
posicao x y = "(" ++ show x ++ ", " ++ show y ++ ")"

-----------------------------------------------------
-- Q.11
-- Objetivo: Devolve o digito das unidades de um numero inteiro
-----------------------------------------------------
unidades :: Int -> Int
unidades num = mod num 10

-----------------------------------------------------
-- Q.12
-- Objetivo: Devolve o digito das dezenas de um numero inteiro
-----------------------------------------------------
dezenas :: Int -> Int
dezenas num = div ((mod num 100) - unidades num) 10

-----------------------------------------------------
-- Q.13
-- Objetivo: Devolve o digito das centenas de um numero inteiro
-----------------------------------------------------
centenas :: Int -> Int
centenas num = div ((mod num 1000) - (dezenas num) - (unidades num)) 100

-----------------------------------------------------
-- Q.14
-- Objetivo: Soma dos algarismos de um numero inteiro (entrada menor ou igual a 999)
-----------------------------------------------------
somaAlgarismos :: Int -> Int
somaAlgarismos num = (centenas num) + (dezenas num) + (unidades num) 

-----------------------------------------------------
-- Q.15
-- Objetivo: Verifica o tipo do triangulo, com base nos lados
-----------------------------------------------------
tipoTriangulo :: Int -> Int -> Int -> String
tipoTriangulo lado1 lado2 lado3 | lado1 == lado2 && lado2 == lado3 = "Triangulo Equilatero"
  | lado1 == lado2 || lado2 == lado3 || lado1 == lado3 = "Triangulo Isoiceles"
  | tresDiferentes lado1 lado2 lado3 == True = "Truangulo Escaleno"

-----------------------------------------------------
-- Q.16
-- Objetivo: Verifica se um numero eh par
-----------------------------------------------------
ehPar :: Int -> Bool
ehPar num | mod num 2 == 0 = True
  | otherwise = False

\end{code}





