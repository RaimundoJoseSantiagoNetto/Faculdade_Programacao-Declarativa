-----------------------------------------------------
-- Exemplos do capitulo 3 do livro
-----------------------------------------------------

\begin{code}

-----------------------------------------------------
-- Ex.00
-- Objetivo: Calcula o fatorial de um numero
-----------------------------------------------------
fat :: Int -> Int
fat n | (n == 0) = 1
  | (n > 0) = n * fat(n - 1)

-----------------------------------------------------
-- Ex.00
-- Objetivo: Retorna o numero de acesso em cada semana
-----------------------------------------------------
numAcesso :: Int -> Int
numAcesso n | n == 0 = 15
  | n == 1 = 5
  | n == 2 = 7
  | n == 3 = 18
  | n == 4 = 7
  | n == 5 = 0
  | n == 6 = 5
  | otherwise = 0

-----------------------------------------------------
-- Ex.00
-- Objetivo: Calcula o total de acessos
-----------------------------------------------------
acessoTotal :: Int -> Int
acessoTotal n | n == 0 = numAcesso(0)
  | otherwise = acessoTotal(n-1) + numAcesso(n)

-----------------------------------------------------
-- Ex.3-2
-- Objetivo: Calcula o fatorial de um numero usando
-- casamento de padrao
-----------------------------------------------------
fatCasamento :: Int -> Int
fatCasamento 0 = 1
fatCasamento n = n * fatCasamento(n - 1)

-----------------------------------------------------
-- Ex.3-3
-- Objetivo: Testa se o numero eh zero
-- casamento de padrao - sublinhado
-----------------------------------------------------
ehZero :: Int -> Bool
ehZero 0 = True
ehZero _ = False -- equivalente a um case default

\end{code}