-----------------------------------------------------
-- Exemplos do capitulo 4 do livro
-----------------------------------------------------

\begin{code}

-----------------------------------------------------
-- Ex.00
-- Objetivo: Computar o numero de elementos de uma lista
-----------------------------------------------------
comp :: [Int] -> Int -- tem um erro de digitacao nessa linha, no livro - ta faltando um ":"
comp [] = 0
comp (cabeca:cauda) = 1 + comp cauda

-----------------------------------------------------
-- Ex.00
-- Objetivo: Computar o cubo de cada elemento de uma lista
-----------------------------------------------------
cadaAoCubo :: [Int] -> Int
cadaAoCubo [] = []
cadaAoCubo (cabeca:cauda) = cubo cabeca : cadaAoCubo cauda
cubo :: Int -> Int
cubo x = x * x * x

-----------------------------------------------------
-- Ex.4-5 a
-- Objetivo: Obter todos os pares de uma lista
-----------------------------------------------------
pares :: [Int] -> [Int]
pares l = [x | x <- l, ehPar x]
-----------------------------------------------------
-- Ex.4-5 b
-- Objetivo: Verificar se um numero eh par
-----------------------------------------------------
ehPar :: Int -> Bool
ehPar num = mod num 2 == 0

\end{code}