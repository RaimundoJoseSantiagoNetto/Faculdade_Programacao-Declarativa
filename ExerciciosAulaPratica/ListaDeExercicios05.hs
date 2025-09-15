module ListaDeExercicios05 where

type Nome = String
type Preco = Int
type CodigoBarra = Int
type BancoDeDados = [(CodigoBarra, Nome, Preco)]

bd:: BancoDeDados
bd = [(1001,"Refrigerante",450),
  (1002,"Leite",320),
  (1003,"Biscoito",200),
  (1004,"Suco",989),
  (1005,"Arroz",345),
  (1006,"Feijao",780)]

-----------------------------------------------------
-- Q.01
-- Objetivo: 
-----------------------------------------------------
buscarBDaux :: CodigoBarra -> BancoDeDados -> (Nome, Preco)
buscarBDaux _ [] = ("", 0)
buscarBDaux cod ((c, n, p):cauda) 
    | c == cod = (n, p)
    | otherwise = buscarBDaux cod cauda

-----------------------------------------------------
-- Q.02
-- Objetivo: 
-----------------------------------------------------
buscarBD :: CodigoBarra -> (Nome, Preco)
buscarBD cod = buscarBDaux cod bd

-----------------------------------------------------
-- Q.03
-- Objetivo: 
-----------------------------------------------------

fazerConta :: [CodigoBarra] -> [(Nome, Preco)]
fazerConta [] = []
fazerConta (cabeca:cauda) = buscarBD cabeca : fazerConta cauda

-----------------------------------------------------
-- Q.04
-- Objetivo: 
-----------------------------------------------------

dividir :: Int -> String
dividir num = show (div num 100) ++ "." ++ show (mod num 100)

-----------------------------------------------------
-- Q.05
-- Objetivo: 
-----------------------------------------------------
repetir :: Int -> String -> String
repetir 0 str = ""
repetir num str = str ++ repetir (num - 1) str

-----------------------------------------------------
-- Q.06
-- Objetivo: 
-----------------------------------------------------

tamanhoLinha :: Int
tamanhoLinha = 30

formatarLinha :: (Nome, Preco) -> String
formatarLinha (n, p) = n ++ (repetir qtdPontos ".") ++ dividir p ++ "\n"
    where
        qtdPontos = tamanhoLinha - (length n) - (length (dividir p))

-----------------------------------------------------
-- Q.07
-- Objetivo: 
-----------------------------------------------------

formatarLinhas :: [(Nome, Preco)] -> String
formatarLinhas [] = []
formatarLinhas (cabeca:cauda) = formatarLinha cabeca ++ formatarLinhas cauda

-----------------------------------------------------
-- Q.08
-- Objetivo: 
-----------------------------------------------------

calcularTotal :: [(Nome, Preco)] -> Int
calcularTotal [] = 0
calcularTotal ((n, p): cauda) = p + calcularTotal cauda

-----------------------------------------------------
-- Q.09
-- Objetivo: 
-----------------------------------------------------

formatarTotal :: Int -> String
formatarTotal num = "Total:" ++ (repetir qtdPontos ".") ++ dividir num ++ "\n"
    where
        qtdPontos = tamanhoLinha - 6 - (length (dividir num))
    


-----------------------------------------------------
-- Q.10
-- Objetivo: 
-----------------------------------------------------

formatarConta :: [(Nome, Preco)] -> String
formatarConta produtos = formatarLinhas produtos ++ formatarTotal total
    where 
        total = calcularTotal produtos


imprimirConta :: [CodigoBarra] -> IO()
imprimirConta lista = putStr (formatarConta(fazerConta lista))
