-----------------------------------------------------
-- Exercicios do capitulo 3 do livro
-----------------------------------------------------

\begin{code}

-----------------------------------------------------
-- Q.02
-- Objetivo: Testa a funcao f
-----------------------------------------------------
f :: Int -> Int
f 0 = 0
f n = n + f n

-----------------------------------------------------
-- Q.03
-- Objetivo: Calcula o fatorial de um numero de uma forma mais eficiente
-----------------------------------------------------
fat2 :: Int -> Int
fat2 n = fatAcum 1 n
fatAcum :: Int -> Int -> Int
fatAcum ac n
  | n == 0 = ac
  | n > 0 = fatAcum (ac * n) (n - 1)

-----------------------------------------------------
-- Q.04
-- Objetivo: Calcula a potencia de um numero
-----------------------------------------------------
pot :: Int -> Int -> Int
pot k n = potAcum 1 k n
potAcum :: Int -> Int -> Int -> Int
potAcum ac k n 
  | n == 0 = ac
  | n > 0 = potAcum (ac * k) (k) (n - 1)

-----------------------------------------------------
-- Q.05
-- Objetivo: Soma todos os pares de 0 a n
-----------------------------------------------------
somaPares :: Int -> Int
somaPares n = somaParesAcum 0 n
somaParesAcum :: Int -> Int -> Int
somaParesAcum ac n 
  | n <= 0 = ac
  | mod n 2 == 0 = somaParesAcum (ac + n) (n - 2)
  | otherwise = somaParesAcum (ac) (n - 1)

-----------------------------------------------------
-- Q.06
-- Objetivo: Soma dois numeros naturais
-----------------------------------------------------
successor :: Int -> Int
successor x = x + 1

soma :: Int -> Int -> Int
soma x y = somaAcum 0 x y
somaAcum :: Int -> Int -> Int -> Int
somaAcum ac x y 
  | x == 0 && y == 0 = ac
  | y > 0 = somaAcum (successor ac) (x) (y - 1)
  | x > 0 = somaAcum (successor ac) (x - 1) (y)

-----------------------------------------------------
-- Q.07
-- Objetivo: Calcula a soma de uma serie harmonica
-----------------------------------------------------
sh :: Int -> Float
sh n = shAcum 0 n
shAcum :: Float -> Int -> Float
shAcum ac n
  | n == 1 = ac
  | n > 0 = shAcum (ac + 1 / fromIntegral n) (n - 1)

-----------------------------------------------------
-- Q.08
-- Objetivo: Calcula a soma de e elevado a x
-----------------------------------------------------
se :: Int -> Int -> Float
se x precisao = seAcum 1 x precisao
seAcum :: Float -> Int -> Int -> Float
seAcum ac x precisao
  | precisao == 0 = ac
  | precisao > 0 = seAcum(ac + ((fromIntegral(x ^ precisao) / fromIntegral(fat2 precisao)))) (x) (precisao - 1) 


-----------------------------------------------------
-- Q.09
-- Objetivo: Calcula a soma de e elevado a x e imprimir um tabela com as iteracoes
-----------------------------------------------------
se2 :: Int -> Int -> IO Float
se2 x precisao = seAcum2 1 x precisao
seAcum2 :: Float -> Int -> Int -> IO Float
seAcum2 ac x precisao
  | precisao == 0 = do
    putStrLn(show precisao ++ " | " ++ show ac)
    return ac
  | precisao > 0 = do
    putStrLn(show precisao ++ " | " ++ show ac)
    seAcum2(ac + ((fromIntegral(x ^ precisao) / fromIntegral(fat2 precisao)))) (x) (precisao - 1) 

-----------------------------------------------------
-- Q.10
-- Objetivo: Calcula o mdc de dois numeros
-----------------------------------------------------
mdc :: Int -> Int -> Int
mdc a b
  | b == 0 = a
  | b > 0 = mdc (b) (a `mod` b)

-----------------------------------------------------
-- Q.11a
-- Objetivo: Calcula a soma de 0 a n ao quadrado
-----------------------------------------------------
sumQuadrados :: Int -> Int
sumQuadrados n = sumQuadradosAcum 0 n
sumQuadradosAcum :: Int -> Int -> Int
sumQuadradosAcum ac n
  | n == 0 = ac
  | n > 0 = sumQuadradosAcum (ac + n ^ 2) (n - 1)

-----------------------------------------------------
-- Q.11b
-- Objetivo: Calcula a soma dos fatoriais de 0 a n
-----------------------------------------------------
sumFatoriais :: Int -> Int
sumFatoriais n = sumFatoriaisAcum 0 n
sumFatoriaisAcum :: Int -> Int -> Int
sumFatoriaisAcum ac n
  | n == 0 = ac
  | n > 0 = sumFatoriaisAcum (ac + fat2 n) (n - 1)

-----------------------------------------------------
-- Q.12
-- Objetivo: Retorna a tabuada de um numero
-----------------------------------------------------
tabuada :: Int -> IO ()
tabuada n = putStrLn(tabuadaAcum "" 1 n)
tabuadaAcum :: String -> Int -> Int -> String
tabuadaAcum ac min n
  | min <= 10 && min > 0 = tabuadaAcum (ac ++ (show n ++ " x " ++ show min ++ " = " ++ show (min*n) ++ "\n")) (min + 1) (n)
  | min == 11 = "Tabuada de " ++ show n ++ "\n" ++ ac 

-----------------------------------------------------
-- Q.13
-- Objetivo: Resolve todos os outros problemas com casamento
-----------------------------------------------------
fat2CP :: Int -> Int
fat2CP n = fatAcumCP 1 n
fatAcumCP :: Int -> Int -> Int
fatAcumCP ac 0 = ac
fatAcumCP ac n = fatAcumCP (ac * n) (n - 1)

potCP :: Int -> Int -> Int
potCP k n = potAcumCP 1 k n
potAcumCP :: Int -> Int -> Int -> Int
potAcumCP ac k 0 = ac
potAcumCP ac k n = potAcumCP (ac * k) (k) (n - 1)

somaParesCP :: Int -> Int
somaParesCP n = somaParesAcumCP 0 n
somaParesAcumCP :: Int -> Int -> Int
somaParesAcumCP ac 0 = ac 
somaParesAcumCP ac n 
  | mod n 2 == 0 = somaParesAcumCP (ac + n) (n - 2)
  | otherwise = somaParesAcumCP (ac) (n - 1)

successorCP :: Int -> Int
successorCP x = x + 1
somaCP :: Int -> Int -> Int
somaCP x y = somaAcum 0 x y
somaAcumCP :: Int -> Int -> Int -> Int
somaAcumCP ac 0 0 = ac
somaAcumCP ac x 0 = somaAcumCP (successor ac) (x) (y - 1)
somaAcumCP ac 0 y = somaAcumCP (successor ac) (x - 1) (y)

shCP :: Int -> Float
shCP n = shAcumCP 0 n
shAcumCP :: Float -> Int -> Float
shAcumCP ac 1 = ac
shAcumCP ac n = shAcumCP (ac + 1 / fromIntegral n) (n - 1)

seCP :: Int -> Int -> Float
seCP x precisao = seAcumCP 1 x precisao
seAcumCP :: Float -> Int -> Int -> Float
seAcumCP ac x 0 = ac
seAcumCP ac x precisao = seAcumCP(ac + ((fromIntegral(x ^ precisao) / fromIntegral(fat2 precisao)))) (x) (precisao - 1) 

se2CP :: Int -> Int -> IO Float
se2CP x precisao = seAcum2CP 1 x precisao
seAcum2CP :: Float -> Int -> Int -> IO Float
seAcum2CP ac 0 precisao do
  putStrLn(show precisao ++ " | " ++ show ac)
  return ac
seAcum2CP ac x precisao do
  putStrLn(show precisao ++ " | " ++ show ac)
  seAcum2CP(ac + ((fromIntegral(x ^ precisao) / fromIntegral(fat2 precisao)))) (x) (precisao - 1) 

mdcCP :: Int -> Int -> Int
mdcCP a 0 = a 
mdcCP a b = mdc (b) (a `mod` b)

sumQuadradosCP :: Int -> Int
sumQuadradosCP n = sumQuadradosAcumCP 0 n
sumQuadradosAcumCP :: Int -> Int -> Int
sumQuadradosAcumCP ac 0 = ac
sumQuadradosAcumCP ac n = sumQuadradosAcumCP (ac + n ^ 2) (n - 1)

sumFatoriaisCP :: Int -> Int
sumFatoriaisCP n = sumFatoriaisAcumCP 0 n
sumFatoriaisAcumCP :: Int -> Int -> Int
sumFatoriaisAcumCP ac 0 = ac
sumFatoriaisAcumCP ac n = sumFatoriaisAcumCP (ac + fat2 n) (n - 1)

tabuadaCP :: Int -> IO ()
tabuadaCP n = putStrLn(tabuadaAcumCP "" 1 n)
tabuadaAcumCP :: String -> Int -> Int -> String
tabuadaAcumCP ac min n
  | min <= 10 && min > 0 = tabuadaAcum (ac ++ (show n ++ " x " ++ show min ++ " = " ++ show (min*n) ++ "\n")) (min + 1) (n)
tabuadaAcumCP ac min 11 = "Tabuada de " ++ show n ++ "\n" ++ ac 

\end{code}
