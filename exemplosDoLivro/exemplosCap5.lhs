-----------------------------------------------------
-- Exemplos do capitulo 5 do livro
-----------------------------------------------------

\begin{code}

-----------------------------------------------------
-- Ex.5-2
-- Objetivo: Define o tipo Aluno
-----------------------------------------------------
type Pnome = String
type Unome = String
type Matricula = Int
type Anuno = (Pnome, Unome, Matricula)

-----------------------------------------------------
-- Ex.5-3 (1)
-- Objetivo: Retorna o primeiro nome do aluno
-----------------------------------------------------
primeiro :: Aluno -> Pnome
primeiro (pn, _, _) = pn

-----------------------------------------------------
-- Ex.5-3 (2)
-- Objetivo: Retorna o ultimo nome do aluno
-----------------------------------------------------
ultimo :: Aluno -> Unome
ultimo (_, ult, _) = ult

-----------------------------------------------------
-- Ex.Ex.5-3 (3)
-- Objetivo: Retorna o numero de matricula do aluno
-----------------------------------------------------
mat :: Aluno -> Matricula
mat (_, _, m) = m

-----------------------------------------------------
-- Ex.Ex.5-3 (4)
-- Objetivo: Imprime os dados de um aluno
-----------------------------------------------------
imprime :: Aluno -> String
imprime (p, u, m) = "Nome:" ++ p ++ " " ++ u ++ " Matricula:" ++ show(m)

-----------------------------------------------------
-- Ex.5-4
-- Objetivo: Calcular a distancia entre dois pontos
-----------------------------------------------------
type Ponto = (Float, Float)
distancia :: Ponto -> Ponto -> Float
distancia (x1, y1) (x2, y2) = sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

-----------------------------------------------------
-- Ex.5-5
-- Objetivo: Calcular a distancia entre dois pontos, com uma funcao local
-----------------------------------------------------
distancia2 :: Ponto -> Ponto -> Float
distanci2 (x1, y1) (x2, y2) = sqrt((sqrtdif x1 x2) + (sqrtdif y1 y2))
  where sqrtdif a b = (b - a) * (b - a)

-----------------------------------------------------
-- Ex.5-6
-- Objetivo: Resolver uma equacao usando let
-----------------------------------------------------
f :: Double -> Double
f x = let a = x ^ 2
  in a ^ 2 + 2 * a + 1 -- equivale a (x^2)^2 + 2 * (x^2) + 1

-----------------------------------------------------
-- Ex.5-7
-- Objetivo: Calcula o fatorial de um numero e trata erros de entrada
-----------------------------------------------------
fat :: Int -> Int
fat n 
  | (n < 0) = error "Fatorial invalido!"
  | (n == 0) = 1
  | otherwise = n * fat(n - 1)

\end{code}