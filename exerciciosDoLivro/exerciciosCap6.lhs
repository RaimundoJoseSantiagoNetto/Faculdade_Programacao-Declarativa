-----------------------------------------------------
-- Exercicios do capitulo 6 do livro
-----------------------------------------------------

\begin{code}

import Data.Char(ord)
import Data.Char(chr)

-----------------------------------------------------
-- Q.03a
-- Objetivo: Retorna o ultimo elemento de uma lista
-----------------------------------------------------
ultimo :: [a] -> a
ultimo [x] = x
ultimo (_:cauda) = ultimo cauda

-----------------------------------------------------
-- Q.03b
-- Objetivo: Retorna a lista sem o ultimo elemento
-----------------------------------------------------
semUltimo :: [a] -> [a]
semUltimo [] = []
semUltimo [x] = []
semUltimo (cabeca:cauda) = [cabeca] ++ semUltimo cauda

-----------------------------------------------------
-- Q.03c
-- Objetivo: Retorna o inverso de uma lista
-----------------------------------------------------
inverte :: [a] -> [a]
inverte [] = []
inverte (cabeca:cauda) = inverte cauda ++ [cabeca]

-----------------------------------------------------
-- Q.04
-- Objetivo: Recebe duas listas e retorna uma lista de pares
-----------------------------------------------------
zip' :: [a] -> [a] -> [(a, a)] -- zip ja eh uma funcao nativa do haskell
zip' lista1 [] = []
zip' [] lista2 = []
zip' (cabeca1:cauda1) (cabeca2:cauda2) = (cabeca1, cabeca2) : zip' cauda1 cauda2

-----------------------------------------------------
-- Q.06
-- Objetivo: Descobrir o objetivo da funcao misteriosa da questao
-----------------------------------------------------
umaFuncao :: [t] -> Int
umaFuncao x = foldl (+) 0 (map um x)
  where um y = 1

-----------------------------------------------------
-- Q.07
-- Objetivo: Recria a funcao map (do tipo inteiro) utilizando a compreensao de listas
-----------------------------------------------------
mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt f lista = [f x | x <- lista]

-----------------------------------------------------
-- Q.08
-- Objetivo: Recria a funcao filter utilizando a compreensao de listas
-----------------------------------------------------
filter' :: (a -> Bool) -> [a] -> [a]
filter' condicao lista = [x | x <- lista, condicao x]

-----------------------------------------------------
-- Q.09a
-- Objetivo: Dobra os elementos de uma lista de inteiros
-----------------------------------------------------
dobrar :: [Int] -> [Int]
dobrar lista = map (*2) lista

-----------------------------------------------------
-- Q.09b
-- Objetivo: Transforma em minusculas os elementos de uma lista de caracteres (String)
-----------------------------------------------------

-- codigos das letras maiusculas 65 - 90
-- codigos das letras minusculas 97 - 122

paraMinuscula :: Char -> Char
paraMinuscula char = 
  if(ord char >= 65 && ord char <= 90) then chr(97 + incremento)
  else char
  where incremento = ord(char) - 65

transformaMinuscula :: [Char] -> [Char]
transformaMinuscula string = map (paraMinuscula) string

-----------------------------------------------------
-- Q.09c
-- Objetivo: Retorna o tamanho de cada String em uma lista de Strings
-----------------------------------------------------
tamanhoStrings :: [String] -> [Int]
tamanhoStrings lista = [tamanhoString x | x <- lista]
  where 
    transformaUm x = 1
    tamanhoString string = foldl (+) 0 (map (transformaUm) string)

-----------------------------------------------------
-- Q.10a
-- Objetivo: Retorna o "ou logico" de uma lista de valores booleanos
-----------------------------------------------------
ouLogico :: [Bool] -> Bool
ouLogico lista = foldl (||) False lista

-----------------------------------------------------
-- Q.10b
-- Objetivo: Concatena Strings em ums lista de Strings
-----------------------------------------------------
concatenarStrings :: [String] -> String
concatenarStrings lista = foldl (++) "" lista

-----------------------------------------------------
-- Q.10c
-- Objetivo: Retorna o menor elemento de uma lista de inteiros
-----------------------------------------------------
menorInt :: [Int] -> Int
menorInt [] = 0
menorInt (cabeca:cauda) = foldl min cabeca cauda

-----------------------------------------------------
-- Q.10d
-- Objetivo: Computa o produto de uma lista de numeros reais
-----------------------------------------------------
produtoFloat :: [Float] -> Float
produtoFloat lista = foldl (*) 1 lista

-----------------------------------------------------
-- Q.11a
-- Objetivo: Seleciona os numeros positivos de uma lista de inteiros
-----------------------------------------------------
selecionarPositivos :: [Int] -> [Int]
selecionarPositivos lista = filter (>=0) lista

-----------------------------------------------------
-- Q.11b
-- Objetivo: Seleciona os numeros primos de uma lista de inteiros
-----------------------------------------------------

ehPrimo :: Int -> Bool
ehPrimo n = n > 1 && null [x | x <- [2 .. floor(sqrt(fromIntegral n))], n `mod` x == 0] -- null verifica se a lista esta vazia e retorna True, se estiver - significa que o numero so pode ser dividido por um e por ele mesmo, neste caso

selecionarPrimos :: [Int] -> [Int]
selecionarPrimos lista = filter (ehPrimo) lista

-----------------------------------------------------
-- Q.11c
-- Objetivo: Seleciona os digitos de uma lista de caracteres (String)
-----------------------------------------------------

-- codigos dos digitos (0 a 9) 48 - 57
ehDigito :: Char -> Bool
ehDigito char = ord char >= 48 && ord char <= 57

selecionarDigitos :: [Char] -> [Char]
selecionarDigitos lista = filter (ehDigito) lista

-----------------------------------------------------
-- Q.13
-- Objetivo: Calcula a soma de dois vetores
-----------------------------------------------------
type Vetor = [Float]

somaDeVetores :: Vetor -> Vetor -> Vetor
somaDeVetores vetor1 vetor2 = zipWith (+) vetor1 vetor2

-----------------------------------------------------
-- Q.14
-- Objetivo: Calcula o produto escalar entre dois vetores
-----------------------------------------------------
produtoEscalar :: Vetor -> Vetor -> Float
produtoEscalar vetor1 vetor2 = foldl (+) 0 (zipWith (*) vetor1 vetor2)

\end{code}