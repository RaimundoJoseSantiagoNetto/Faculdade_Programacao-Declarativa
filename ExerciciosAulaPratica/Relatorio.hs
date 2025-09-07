module Relatorio(relatorio) where

import Data.Char

tamanhoLinha :: Int
tamanhoLinha = 30

relatorio :: Int -> IO ()
relatorio n = putStr(cabecalho ++ imprimirLinhas n ++ rodape n)

titulo :: String
titulo = "Relatorio de Vendas"

espacamentoDoTitulo :: String
espacamentoDoTitulo = espacamentoAux(truncate(fromIntegral(tamanhoLinha - length titulo) / 2))

espacamentoAux :: Int -> String
espacamentoAux 0 = ""
espacamentoAux n = " " ++ espacamentoAux (n - 1)

cabecalho :: String
cabecalho = tracos ++  espacamentoDoTitulo ++ titulo ++ "\n" ++ tracos 
    where
        tracos = impSimbolo tamanhoLinha "-" ++ "\n"  

imprimirLinhas :: Int -> String
imprimirLinhas 0 = imprimirLinha 0 
imprimirLinhas n = imprimirLinhas (n-1) ++ "\n" ++ imprimirLinha n 

imprimirLinha :: Int -> String
imprimirLinha n = meses !! n ++ espacamentoLinha (meses !! n) n ++ show (vendas n)
    where 
        meses = ["Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"]

espacamentoLinha :: String -> Int -> String
espacamentoLinha mes num = espacamentoAux(tamanhoLinha - length mes - length (show (vendas num)))

impSimbolo :: Int -> String -> String
impSimbolo 0 s = ""
impSimbolo n s = s ++ impSimbolo (n-1) s

rodape :: Int -> String
rodape n = "\n" ++ impSimbolo tamanhoLinha "-" ++ "\n" 
    ++ imprimirTotal n ++ "\n" 
    ++ imprimirMaior n ++ "\n" 
    ++ imprimirMenor n ++ "\n" 
    ++ imprimirMedia n ++ "\n" 
    ++ impSimbolo tamanhoLinha "-" ++ "\n"

imprimirMaior :: Int -> String
imprimirMaior n = "Maior = " ++ show (maiorVenda n)

imprimirMenor :: Int -> String
imprimirMenor n = "Menor = " ++ show (menorVenda n)

imprimirTotal :: Int -> String
imprimirTotal n = "Total = " ++ show (somavendas n)

imprimirMedia :: Int -> String
imprimirMedia n = "Media = " ++ show (mediaVendas n)

maiorVenda :: Int -> Int
maiorVenda 0 = vendas 0
maiorVenda n = max (vendas n) (maiorVenda (n-1))

menorVenda :: Int -> Int
menorVenda 0 = vendas 0
menorVenda n = min (vendas n) (menorVenda (n-1))

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


