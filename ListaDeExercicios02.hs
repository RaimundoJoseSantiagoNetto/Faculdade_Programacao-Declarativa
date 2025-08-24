--------------------------------------------------
-- Q.01
-- Objetivo: Soma todos os pares de 1 a N
--------------------------------------------------
ehPar :: Int -> Bool
ehPar n = mod n 2 == 0

somaPares :: Int -> Int
somaPares 0 = 0
somaPares n 
    | ehPar n = n + somaPares(n-1)
    | otherwise = somaPares(n-1)

--------------------------------------------------
-- Q.02
-- Objetivo: Calcula x elevado a y
--------------------------------------------------
pow :: Int -> Int -> Int
pow x 1 = x * 1
pow x y = x * pow x (y-1)

--------------------------------------------------
-- Q.03
-- Objetivo: Soma os numeros entre A e B
--------------------------------------------------
somaIntervalo :: Int -> Int -> Int
somaIntervalo a b 
    | a == b = a
    | a < b = a + somaIntervalo (a + 1) b
    | otherwise = b + somaIntervalo a (b + 1)

--------------------------------------------------
-- Q.04
-- Objetivo: Soma os algarismos de um numero inteiro de 0 a 999
--------------------------------------------------
centenas :: Int -> Int
centenas num = div (mod num 1000 - dezenas num - unidades num) 100

dezenas :: Int -> Int
dezenas num = div (mod num 100 - unidades num) 10

unidades :: Int -> Int
unidades num = mod num 10

somaAlgarismos :: Int -> Int
somaAlgarismos num = centenas num + dezenas num + unidades num

--------------------------------------------------
-- Q.05
-- Objetivo: Soma os quadrados dos inteiros de 0 a n
--------------------------------------------------
somaQuadrados :: Int -> Int
somaQuadrados 0 = 0
somaQuadrados n = n^2 + somaQuadrados(n-1)

--------------------------------------------------
-- Q.06
-- Objetivo: Soma o fatorial dos inteiros de 0 a n
--------------------------------------------------
fat :: Integer -> Integer
fat 0 = 1
fat n = n * fat(n-1)

somaFatorial :: Integer -> Integer
somaFatorial 0 = 1
somaFatorial n = fat n + somaFatorial(n-1)

--------------------------------------------------
-- Q.07
-- Objetivo: Calcula a aproximacao de pi
--------------------------------------------------

approxPi :: Integer -> Double
approxPi 0 = 4 
approxPi n = 4 * (fromIntegral((-1) ^ n )/ fromIntegral(2 * n + 1)) + approxPi(n - 1)

--------------------------------------------------
-- Q.08
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

--------------------------------------------------
-- Q.09
-- Objetivo: Multiplica dois numeros naturais
--------------------------------------------------
multiplica :: Int -> Int -> Int
multiplica _ 0 = 0
multiplica x y = soma x (multiplica x (y-1))

--------------------------------------------------
-- Q.10
-- Objetivo: Conta a quantidade de um digito de um numero 
--------------------------------------------------
contarDigitos :: Int -> Int -> Int
contarDigitos digito num =
    length (filter (== head (show digito)) (show num))

--------------------------------------------------
-- Q.11 
-- Objetivo: Converte um numero decimal para binario
--------------------------------------------------
converterBinario :: Integer -> String
converterBinario num = reverse (converterBinarioAux num)

converterBinarioAux :: Integer -> String
converterBinarioAux 0 = ""
converterBinarioAux num = show (mod num 2) ++ converterBinarioAux (div num 2)

--------------------------------------------------
-- Q.12
-- Objetivo: Converte um numero decimal para hexadecimal
--------------------------------------------------


--------------------------------------------------
-- Q.13
-- Objetivo: Concatena uma palavra N vezes
--------------------------------------------------
concatenaNvezes :: String -> Int -> String
concatenaNvezes string 0 = ""
concatenaNvezes string num = string ++ concatenaNvezes string (num - 1)

--------------------------------------------------
-- Q.14
--------------------------------------------------
-- Em Haskell, tail recursion (recursao de cauda) eh quando a chamada recursiva eh
-- a ultima operacao executada pela funcao, sem deixar calculos pendentes apos ela, 
-- o que permite ao compilador otimizar a execucao reutilizando o mesmo espaco de 
-- memoria em vez de empilhar chamadas sucessivas; isso torna o processo mais eficiente 
-- e evita problemas como stack overflow, sendo comum o uso de acumuladores para 
-- transformar funcoes recursivas simples em versoes de cauda

--------------------------------------------------
-- Q.15
-- Objetivo: Calcula o n-esimo termo da sequencia de Fibonacci 
--------------------------------------------------
fibonacci :: Int -> Int
fibonacci n = fibAux n 0 1
  where
    fibAux 0 a _ = a
    fibAux n a b = fibAux (n - 1) b (a + b)

--------------------------------------------------
-- Q.16
-- Objetivo: Calcula o mdc de dois numeros
-----------------------------------------------------
mdc :: Int -> Int -> Int
mdc a b
  | b == 0 = a
  | b > 0 = mdc (b) (a `mod` b)

--------------------------------------------------
-- Q.17
-- Objetivo: Retorna a tabuada de um numero
--------------------------------------------------
tabuadaCP :: Int -> IO ()
tabuadaCP n = putStrLn(tabuadaAcumCP "" 1 n)
tabuadaAcumCP :: String -> Int -> Int -> String
tabuadaAcumCP ac min n
    | min <= 10 && min > 0 = tabuadaAcumCP (ac ++ (show n ++ " x " ++ show min ++ " = " ++ show (min*n) ++ "\n")) (min + 1) (n)
    | otherwise = "Tabuada de " ++ show n ++ "\n" ++ ac 


--------------------------------------------------
-- Q.18
-- Objetivo: 
--------------------------------------------------

--------------------------------------------------
-- Q.19
-- Objetivo: 
--------------------------------------------------

--------------------------------------------------
-- Q.20
-- Objetivo: 
--------------------------------------------------

--------------------------------------------------
-- Q.21
-- Objetivo: 
--------------------------------------------------

--------------------------------------------------
-- Q.22
-- Objetivo: 
--------------------------------------------------
somavendas :: Int -> Int
somavendas 0 = vendas 0
somavendas n = vendas n + somavendas (n - 1)

mediaVendas :: Int -> Float
mediaVendas num = fromIntegral(somavendas num) / fromIntegral(num)

vendas :: Int -> Int
vendas n 
   | n == 0 = 23
   | n == 1 = 34
   | n == 2 = 58
   | n == 3 = 12
   | n == 4 = 56
   | otherwise = 0

--------------------------------------------------
-- Q.23
-- Objetivo: 
--------------------------------------------------
-- A) 

-- B)

-- C) 

-- D) 