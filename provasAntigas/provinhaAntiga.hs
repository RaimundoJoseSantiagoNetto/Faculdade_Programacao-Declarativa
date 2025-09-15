-----------------------------------------------------
-- Q.01
-- Objetivo: Verifica se uma lista esta ordenada em ordem decrescente
-----------------------------------------------------

isSorted :: [Int] -> Bool
isSorted [b] = True
isSorted (a:b:cauda)
  | a <= b = isSorted (b:cauda)
  | otherwise = False

-----------------------------------------------------
-- Q.02
-- Objetivo: Calcula a quantidade de bilhetes premiados da mega-sena
-----------------------------------------------------

type Resultado = [Int]
type Jogos = [[Int]]

premiados :: Resultado -> Jogos -> Int
premiados _ [] = 0
premiados resultado (pessoa:restante)
  | resultado == pessoa = 1 + premiados resultado restante
  | otherwise = premiados resultado restante

-----------------------------------------------------
-- Q.03
-- Objetivo: Manipula um "banco de dados" de vendas
-----------------------------------------------------

vendas :: [(String, String, Int)]
vendas = [("SP", "São Paulo", 100),
          ("RJ", "Rio de Janeiro", 50),
          ("SP", "Campinas", 30),
          ("RJ", "Petropolis", 70)]


-- A)

quantidadeVendidaEstado :: String -> [(String, String, Int)] -> Int
quantidadeVendidaEstado estado [] = 0
quantidadeVendidaEstado estado ((est, _, _):cauda)
  | est == estado = 1 + quantidadeVendidaEstado estado cauda
  | otherwise = quantidadeVendidaEstado estado cauda

-- B)

quantidadeTotalVendida :: [(String, String, Int)] -> Int
quantidadeTotalVendida [] = 0
quantidadeTotalVendida ((_, _, tot):cauda) = tot + quantidadeTotalVendida cauda

-- C)

cidadesNoEstado :: String -> [(String, String, Int)] -> [String]
cidadesNoEstado estado [] = []
cidadesNoEstado estado ((est, cid, _):cauda)
  | est == estado = cid : cidadesNoEstado estado cauda
  | otherwise = cidadesNoEstado estado cauda

-----------------------------------------------------
-- Q.04
-----------------------------------------------------

-- fibo 5 => fibo 4 e fibo 3 = 2
  -- fibo 4 => fibo 3 e fibo 2 = 2
    -- fibo 3 => fibo 2 e fibo 1 = 2
      -- fibo 2 => fibo 1 e fibo 0 = 2
      -- fibo 1 = 1
  -- fibo 3 => fibo 2 e fibo 1 = 2
    -- fibo 2 => fibo 1 e fibo 0 = 2
      -- fibo 1 = 1
      -- fibo 0 = 1

-- 2 + 2 + 2 + 2 + 1 + 2 + 2 + 1 + 1 = 15 chamadas


fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n - 1) + fibo (n - 2)

-----------------------------------------------------
-- Q.05
-----------------------------------------------------

-- h' 4 => 4 * k'3 ==> 4 * 7 = 28
-- k' 3 => 3 + h' 2 ==> 3 + 4 = 7
-- h' 2 => 2 * k' 1 ==> 2 * 2 = 4
-- k' 1 => 1 + h' 0 ==> 1 + 1 = 2
-- h' 0 = 1


h' :: Int -> Int
h' 0 = 1
h' n = n * k' (n - 1)

k' :: Int -> Int
k' n = n + h' (n - 1)

-----------------------------------------------------
-- Q.06
-----------------------------------------------------

-- A função sp soma os pares de 0 a n, isto é:
-- sp 13 0 => 12 + 10 + 8 + 6 + 4 + 2 => 42

sp :: Int -> Int -> Int
sp n acumulador
  | n <= 0 = acumulador
  | even n = sp (n - 1) (acumulador + n)
  | otherwise = sp (n - 1) acumulador

-----------------------------------------------------
-- Q.07
-----------------------------------------------------

-- h 27 => h (h(div 27 3)) => h(h(9)) => h(9 * 2) => h(18)
-- h 18 => h (h(div 18 3)) => h(h(6)) => h(6 * 2) => h(12)
-- h 12 => h (h(div 12 3)) => h(h(4)) => h(4 * 2) => h(8)
-- h 8 => 16

h :: Int -> Int
h n
  | n <= 10 = n * 2
  | otherwise = h (h (div n 3))

-----------------------------------------------------
-- Q.08
-----------------------------------------------------

-- ackermann = ak
-- ak 1 2 => ak 0 (ak 1 1) ==> ak 0 3 ===> 4
-- ak 1 1 => ak 0 (ak 1 0) ==> ak 0 2 ==> 3
-- ak 1 0 => ak 0 1 ==> 2
-- ak 0 1 => 2
-- ak 0 2 => 3
-- ak 0 3 => 4

ackermann :: Int -> Int -> Int
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1)) 
