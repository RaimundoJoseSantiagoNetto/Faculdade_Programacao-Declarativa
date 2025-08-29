--------------------------------------------------
-- Q.01
-- Objetivo: Calcula a distancia entre dois pontos
--------------------------------------------------
type Ponto = (Double, Double)

distanciaPontos :: Ponto -> Ponto -> Double
distanciaPontos (x1, y1) (x2, y2) = sqrt((quadradoDiferenca x2 x1) + (quadradoDiferenca y2 y1))
    where 
        quadradoDiferenca a b = (a - b)^2

--------------------------------------------------
-- Q.02
-- Objetivo: Verifica o estado de um ponto em relacao a uma circunferencia
--------------------------------------------------
posicaoPonto :: Ponto -> Ponto -> Double -> String
posicaoPonto ponto centroCircunferencia raio
    | distanciaPontos ponto centroCircunferencia < raio = "Dentro"
    | distanciaPontos ponto centroCircunferencia == raio = "Na borda"
    | distanciaPontos ponto centroCircunferencia > raio = "Fora"

--------------------------------------------------
-- Q.03
-- Objetivo: Calcula o minimo e o maximo de tres numeros inteiros
--------------------------------------------------
minmax :: Int -> Int -> Int -> (Int, Int)
minmax a b c = (vMin, vMax)
    where 
        vMin = min a (min b c)
        vMax = max a (max b c)

--------------------------------------------------
-- Q.04
-- Objetivo: Calcula o maximo de tres numeros e a ocorrencia
--------------------------------------------------
maxocorre :: Int -> Int -> Int -> (Int, Int)
maxocorre a b c = (maior, ocorrencia)
    where 
        maior = max a (max b c)
        ocorrencia 
            | maior == a && maior == b && maior == c = 3
            | maior == a && maior == b || maior == a && maior == c || maior == b && maior == c = 2
            | otherwise = 1

maxocorre' :: Int -> Int -> Int -> (Int, Int)
maxocorre' a b c = (max a (max b c), length (filter (== max a (max b c)) [a, b, c]))

--------------------------------------------------
-- Q.05
-- Objetivo: Manpular um banco de dados
--------------------------------------------------
type Matricula = Int
type Nome = String
type Titulacao = String
type Sexo = Char

banco :: Int -> (Matricula, Nome, Titulacao, Sexo)
banco matricula
    | matricula >= 1 && matricula <= 6 = dados !! (matricula - 1)
    | otherwise = (0, "", "", ' ')
    where 
        dados = [(1, "Roque", "Doutor", 'M'), (2, "Alzira", "Doutor", 'F'), (3, "Helio", "Doutor", 'M'), (4, "Maisa", "Doutor", 'F'), (5, "Carlos", "Mestre", 'M'), (6, "Rita", "Mestre", 'F')]

-- A) 
contDoutores :: Int -> Int
contDoutores intervalo = contarDoutoresAux 0 1
    where
        contarDoutoresAux cont intervalo = cont
        contarDoutoresAux cont n
            | titulacao == "Doutor" = contarDoutoresAux (cont + 1) (n+1)
            | otherwise = contarDoutoresAux cont (n+1)
            where 
                (_, _, titulacao, _) = banco n

-- B)
contMulheres :: Int -> Int
contMulheres intervalo = contMulheresAux 0 1
    where
        contMulheresAux cont intervalo = cont
        contMulheresAux cont n
            | sexo == 'F' = contMulheresAux (cont + 1) (n+1)
            | otherwise = contMulheresAux cont (n+1)
            where 
                (_, _, _, sexo) = banco n

-- C)
contMulheresMestre :: Int -> Int
contMulheresMestre intervalo = contMulheresMestreAux 0 1
    where
        contMulheresMestreAux cont intervalo = cont
        contMulheresMestreAux cont n
            | sexo == 'F' && titulacao == "Mestre" = contMulheresMestreAux (cont + 1) (n+1)
            | otherwise = contMulheresMestreAux cont (n+1)
            where 
                (_, _, titulacao, sexo) = banco n

-- D)
listarDoutores :: Int -> [String]
listarDoutores intervalo = listarDoutorAux [] 1
    where
        listarDoutorAux lista intervalo = lista
        listarDoutorAux lista n
            | titulacao == "Doutor" = listarDoutorAux (lista ++ [nome]) (n+1)
            | otherwise = listarDoutorAux lista (n+1)
            where 
                (_, nome, titulacao, _) = banco n

--------------------------------------------------
-- Q.06
-- Objetivo: Reolve equacoes do segundo grau
-----------------------------------------------------
equacao2G :: (Float, Float, Float) -> (Float,Float)
equacao2G (a, b, c) 
    | delta (a, b, c) > 0 || delta (a, b, c) == 0 = (x1 (a, b, c), x2 (a, b, c))
    | otherwise = (0, 0)

delta :: (Float, Float, Float) -> Float
delta a b c = b ^ 2 - 4 * a * c

x1 :: (Float, Float, Float) -> Float
x1 (a, b, c) = ((-1) * b + sqrt(delta a b c)) / (2 * a)

x2 :: (Float, Float, Float) -> Float
x2 (a, b, c) = ((-1) * b - sqrt(delta a b c)) / (2 * a)

--------------------------------------------------
-- Q.07
-- Objetivo: Manipula e trata datas
--------------------------------------------------
type Data = (Int, Int, Int)

-- A) 
ehBissexto :: Int -> Bool
ehBissexto ano
  | ano `mod` 400 == 0 = True
  | ano `mod` 100 == 0 = False
  | ano `mod` 4   == 0 = True
  | otherwise          = False

-- B)
diasNoMes :: Int -> Int -> Int
diasNoMes mes ano
  | mes == 2 = if ehBissexto ano then 29 else 28
  | mes `elem` [4, 6, 9, 11] = 30
  | mes `elem` [1,3,5,7,8,10,12] = 31
  | otherwise = 0

-- C) 
dataValida :: Data -> Bool
dataValida (d, m, a) =
  m >= 1 && m <= 12 &&
  d >= 1 && d <= diasNoMes m a

-- D) 
proximoDia :: Data -> Data
proximoDia (d, m, a)
  | d < diasNoMes m a = (d+1, m, a)
  | m < 12            = (1, m+1, a)
  | otherwise         = (1, 1, a+1)