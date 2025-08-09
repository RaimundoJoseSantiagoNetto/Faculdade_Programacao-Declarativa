-----------------------------------------------------
-- Exercicios do capitulo 5 do livro
-----------------------------------------------------

\begin{code}

import Data.Char(ord)
import Data.Char(chr)

-----------------------------------------------------
-- Q.03
-- Objetivo: Troca o primeiro com o segundo elemento de um par de inteiros
-----------------------------------------------------
troca :: (Int, Int) -> (Int, Int)
troca (x1, y1) = (y1, x1)

-----------------------------------------------------
-- Q.04a
-- Objetivo: Soma dois pontos
-----------------------------------------------------
type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

somarPontos :: Ponto -> Ponto -> Ponto
somarPontos (x1, y1) (x2, y2) = ((x1 + x2), (y1 + y2))

-----------------------------------------------------
-- Q.04b
-- Objetivo: Verifica se uma reta esta na vertical
-----------------------------------------------------
verificarVertical :: Reta -> Bool
verificarVertical ((x1,_), (x2,_)) = x1 == x2

-----------------------------------------------------
-- Q.04c
-- Objetivo: Verifica se uma reta esta na horizontal
-----------------------------------------------------
verificarHorizontal :: Reta -> Bool
verificarHorizontal ((_,y1), (_,y2)) = y1 == y2

-----------------------------------------------------
-- Q.05
-- Objetivo: Define e realiza operacoes com numeros complexos
-----------------------------------------------------
type NumComplexo = (Float, Float)

adicaoComplexos :: NumComplexo -> NumComplexo -> String
adicaoComplexos (numR1, numI1) (numR2, numI2) = show(numR1 + numR2) ++ " + (" ++ show(numI1 + numI2) ++ ")i"

subtracaoComplexos :: NumComplexo -> NumComplexo -> String
subtracaoComplexos (numR1, numI1) (numR2, numI2) = show(numR1 - numR2) ++ " + (" ++ show(numI1 - numI2) ++ ")i"

multiplicacaoComplexos :: NumComplexo -> NumComplexo -> String
multiplicacaoComplexos num1 num2 = show(parteReal num1 num2) ++ " + (" ++ show(parteImaginaria num1 num2) ++ ")i"
  where 
    parteReal (numR1, numI1) (numR2, numI2) = (numR1 * numR2) - (numI1 * numI2)
    parteImaginaria (numR1, numI1) (numR2, numI2) = (numR1 * numI2) + (numI1 * numR2)

-----------------------------------------------------
-- Q.06
-- Objetivo: Retorna o maior e o menor valor de uma lista de inteiros
-----------------------------------------------------
minEmax :: [Int] -> (Int, Int)
minEmax [] = (0, 0)
minEmax (cabeca:cauda) = minEmax2 (cabeca, cabeca) (cabeca:cauda)
minEmax2 :: (Int, Int) -> [Int] -> (Int, Int)
minEmax2 (min, max) [] = (min, max)
minEmax2 (min, max) (cabeca:cauda)
  | cabeca >= max = minEmax2 (min, cabeca) cauda
  | cabeca >= min = minEmax2 (cabeca, max) cauda

-----------------------------------------------------
-- Q.07
-- Objetivo: Retorna o ultimo elemento de uma lista de inteiros e o resto da lista
-----------------------------------------------------
ultimo :: [Int] -> (Int, [Int])
ultimo [] = (0, [])
ultimo (cabeca:cauda) = ultimo2 (cabeca, []) (cabeca:cauda)
ultimo2 :: (Int, [Int]) -> [Int] -> (Int, [Int])
ultimo2 (ultimo, resto) [] = (ultimo, resto)
ultimo2 (ultimo, resto) (cabeca:cauda)
  | cauda == [] = ultimo2 (cabeca, resto) cauda
  | otherwise = ultimo2 (cabeca, resto ++ [cabeca]) cauda

-----------------------------------------------------
-- Q.08a
-- Objetivo: Adiciona um aluno na lista de alunos
-----------------------------------------------------
type Matricula = Int
type Nome = String
type Bairro = String
type Aluno = (Matricula, Nome, Bairro)
type Alunos = [Aluno]

adicionarAluno :: Alunos -> Aluno -> Alunos
adicionarAluno turma aluno = turma ++ [aluno]

-----------------------------------------------------
-- Q.08b
-- Objetivo: Exclui um aluno da lista de alunos
-----------------------------------------------------
excluirAluno :: Alunos -> Aluno -> Alunos
excluirAluno turma aluno = excluirAluno2 [] turma aluno
excluirAluno2 :: Alunos -> Alunos -> Aluno -> Alunos
excluirAluno2 listaAuxiliar [] aluno = listaAuxiliar
excluirAluno2 listaAuxiliar (cabeca:cauda) aluno 
  | cabeca == aluno = excluirAluno2 (listaAuxiliar) cauda aluno
  | otherwise = excluirAluno2 (listaAuxiliar ++ [cabeca]) cauda aluno

-----------------------------------------------------
-- Q.08c
-- Objetivo: Retorna o nome de todos os alunos de um bairro pesquisado
-----------------------------------------------------
pesquisarBairro :: Alunos -> Bairro -> [Nome]
pesquisarBairro turma bairro = pesquisarBairro2 [] turma bairro
pesquisarBairro2 :: [Nome] -> Alunos -> Bairro -> [Nome]
pesquisarBairro2 listaAuxiliar [] bairro = listaAuxiliar 
pesquisarBairro2 listaAuxiliar ((_,n,b):cauda) bairro 
  | bairro == b = pesquisarBairro2 (listaAuxiliar ++ [n]) cauda bairro
  | otherwise = pesquisarBairro2 listaAuxiliar cauda bairro

-----------------------------------------------------
-- Q.08d
-- Objetivo: Retorna os dados de um aluno, pelo numero de matricula
-----------------------------------------------------
pesquisarDados :: Alunos -> Matricula -> Aluno
pesquisarDados turma numMatricula = pesquisarDados2 (0, "", "") turma numMatricula
pesquisarDados2 :: Aluno -> Alunos -> Matricula -> Aluno
pesquisarDados2 listaAuxiliar [] numMatricula = listaAuxiliar 
pesquisarDados2 listaAuxiliar ((m,n,b):cauda) numMatricula 
  | numMatricula == m = pesquisarDados2 (m,n,b) cauda numMatricula
  | otherwise = pesquisarDados2 listaAuxiliar cauda numMatricula

-----------------------------------------------------
-- Q.09a
-- Objetivo: Retorna uma lista de codigos dos filmes de um genero pesquisado de uma lista (Filmes)
-----------------------------------------------------
type Codigo = Int
type NomeFilme = String
type Genero = String
type ValorAluguel = Float
type Filme = (Codigo, NomeFilme, Genero, ValorAluguel)
type Filmes = [Filme]

pesquisarGenero :: Filmes -> Genero -> [Codigo]
pesquisarGenero filmes genero = pesquisarGenero2 [] filmes genero
pesquisarGenero2 :: [Codigo] -> Filmes -> Genero -> [Codigo] 
pesquisarGenero2 listaAuxiliar [] genero = listaAuxiliar 
pesquisarGenero2 listaAuxiliar ((c, _, g, _):cauda) genero 
  | genero == g = pesquisarGenero2 (listaAuxiliar ++ [c]) cauda genero
  | otherwise = pesquisarGenero2 listaAuxiliar cauda genero

-----------------------------------------------------
-- Q.09b
-- Objetivo: Aumenta o valor do aluguel de todos os filmes em um valor, em uma lista (Filmes)
-----------------------------------------------------
aumentarAluguel :: Filmes -> Float -> Filmes
aumentarAluguel filmes aumento = aumentarAluguel2 [] filmes aumento
aumentarAluguel2 :: Filmes -> Filmes -> Float -> Filmes
aumentarAluguel2 listaAuxiliar [] aumento = listaAuxiliar 
aumentarAluguel2 listaAuxiliar ((c, n, g, v):cauda) aumento = aumentarAluguel2 (listaAuxiliar ++ [(c, n, g, v + aumento)]) cauda aumento

-----------------------------------------------------
-- Q.09c
-- Objetivo: Verifica quantos filmes de um genero estao presentes em uma lista (Filmes)
-----------------------------------------------------
verificarQuantidade :: Filmes -> Genero -> Int
verificarQuantidade filmes genero = verificarQuantidade2 0 filmes genero
verificarQuantidade2 :: Int -> Filmes -> Genero -> Int
verificarQuantidade2 contador [] genero = contador 
verificarQuantidade2 contador ((_, _, g, _):cauda) genero 
  | genero == g = verificarQuantidade2 (contador + 1) cauda genero
  | otherwise = verificarQuantidade2 contador cauda genero

-----------------------------------------------------
-- Q.10a
-- Objetivo: Retorna o primeiro nome de uma pessoa, pela String do nome completo da pessoa
-----------------------------------------------------
getString :: String -> String
getString nome = getString2 "" nome
getString2 :: String -> String -> String
getString2 stringAuxiliar [] = ""
getString2 stringAuxiliar (cabeca:cauda)
  | cabeca == ' ' = stringAuxiliar
  | otherwise = getString2 (stringAuxiliar ++ [cabeca]) cauda

-----------------------------------------------------
-- Q.10b
-- Objetivo: Retorna a String do nome completo de uma pessoa, sem o primeiro nome
-----------------------------------------------------
dropString :: String -> String
dropString nome = dropString2 "" (getString nome) nome
dropString2 :: String -> String -> String -> String
dropString2 stringAuxiliar primeiroNome [] = ""
dropString2 stringAuxiliar primeiroNome (cabeca:cauda)
  | stringAuxiliar == primeiroNome = cauda
  | otherwise = dropString2 (stringAuxiliar ++ [cabeca]) primeiroNome cauda

-----------------------------------------------------
-- Q.10c
-- Objetivo: Retorna uma lista de nomes, com base na String de um nome completo de uma pessoa
-----------------------------------------------------
splitString :: String -> [String]
splitString nome = splitString2 "" [] nome
splitString2 :: String -> [String] -> String -> [String]
splitString2 stringAuxiliar lista [] = lista ++ [stringAuxiliar]
splitString2 stringAuxiliar lista (cabeca:cauda)
  | cabeca == ' ' = splitString2 "" (lista ++ [stringAuxiliar]) cauda 
  | otherwise = splitString2 (stringAuxiliar ++ [cabeca]) lista cauda 

-----------------------------------------------------
-- Q.10d
-- Objetivo: Retorna o ultimo nome de uma lista de nomes
-----------------------------------------------------
lastString :: [String] -> String
lastString [] = ""
lastString (cabeca:cauda) 
  | cauda == [] = cabeca
  | otherwise = lastString cauda

-----------------------------------------------------
-- Q.10e
-- Objetivo: Retorna uma lista de nomes sem o ultimo nome
-----------------------------------------------------
initString :: [String] -> [String]
initString listaNomes = initString2 [] listaNomes
initString2 :: [String] -> [String] -> [String]
initString2 lista [] = []
initString2 lista (cabeca:cauda) 
  | cauda == [] = lista
  | otherwise = initString2 (lista ++ [cabeca]) cauda

-----------------------------------------------------
-- Q.10f
-- Objetivo: Retorna as iniciais dos nomes de uma lista de nomes
-----------------------------------------------------
initChars :: [String] -> String
initChars listaNomes = initChars2 "" listaNomes
initChars2 :: String -> [String] -> String
initChars2 stringAuxiliar [] = stringAuxiliar
initChars2 stringAuxiliar (cabeca:cauda) = initChars2 (stringAuxiliar ++ [head (cabeca)] ++ ['.']) cauda

-----------------------------------------------------
-- Q.10g
-- Objetivo: Converte um nome para a versao autoral resumida
-----------------------------------------------------
converte :: String -> String
converte nomeCompleto = lastString(splitString(nomeCompleto)) ++ " " ++ initChars(initString(splitString(nomeCompleto)))

-----------------------------------------------------
-- Q.11
-- Objetivo: Converte uma lista de nomes em uma lista de e-mails
-----------------------------------------------------
type NomeFuncionario = String
type Nomes = [NomeFuncionario]
type Email = String
type Emails = [Email]

-----------------------------------------------------
-- "import" da funcao da questao 15 dos exercicios do capitulo passado
-- Objetivo: Converte todos os caracteres de uma String para minuscula

paraMinuscula :: String -> String
paraMinuscula string = paraMinuscula2 "" string
paraMinuscula2 :: String -> String -> String
paraMinuscula2 transformada [] = transformada
paraMinuscula2 transformada (cabeca:cauda) = paraMinuscula2 (transformada ++ [paraMinusculaChar cabeca]) cauda

-- codigos das letras maiusculas 65 - 90
-- codigos das letras minusculas 97 - 122

paraMinusculaChar :: Char -> Char
paraMinusculaChar char = do
  let incremento = ord(char) - 65
  chr(97 + incremento)
-----------------------------------------------------

converteNomes :: Nomes -> Emails
converteNomes nomesDosFuncionarios = converteNomes2 [] nomesDosFuncionarios
converteNomes2 :: Emails -> Nomes -> Emails
converteNomes2 listaAuxiliar [] = listaAuxiliar 
converteNomes2 listaAuxiliar (cabeca:cauda) = converteNomes2 (listaAuxiliar ++ [iniciaisMinusculas "" (splitString(cabeca)) ++ "@empresa.com.br"]) cauda

iniciaisMinusculas :: String -> [String] -> String
iniciaisMinusculas listaAuxiliar [] = paraMinuscula(listaAuxiliar)
iniciaisMinusculas listaAuxiliar (cabeca:cauda) = iniciaisMinusculas (listaAuxiliar ++ [head(cabeca)]) cauda

-----------------------------------------------------
-- Q.12
-- Objetivo: Converte uma String de um valor (0 - 99) para uma String do valor em extenso
-- (feito pelo gpt)
-----------------------------------------------------
valorPorExtenso :: String -> String
valorPorExtenso valor =
  let (parteReais, parteCentavos) = quebrar valor
      reais = read parteReais :: Int
      centavos = read (ajustaCentavos parteCentavos) :: Int
      reaisTxt = if reais == 0 then "" else porExtenso reais ++ if reais == 1 then " real" else " reais"
      centavosTxt = if centavos == 0 then "" else porExtenso centavos ++ if centavos == 1 then " centavo" else " centavos"
  in case (reaisTxt, centavosTxt) of
       ("", "") -> "zero real"
       ("", c) -> c
       (r, "") -> r
       (r, c) -> r ++ " e " ++ c

quebrar :: String -> (String, String)
quebrar s = case span (/= '.') s of
  (r, []) -> (r, "00")
  (r, '.':c) -> (r, c)

ajustaCentavos :: String -> String
ajustaCentavos c
  | length c == 1 = c ++ "0"
  | length c >= 2 = take 2 c
  | otherwise = "00"

porExtenso :: Int -> String
porExtenso n
  | n <= 19 = unidades !! n
  | otherwise = dezenas !! (n `div` 10) ++ if n `mod` 10 /= 0 then " e " ++ unidades !! (n `mod` 10) else ""
  where
    unidades = ["zero", "um", "dois", "três", "quatro", "cinco", "seis", "sete", "oito", "nove",
      "dez", "onze", "doze", "treze", "quatorze", "quinze", "dezesseis", "dezessete", "dezoito", "dezenove"]
    dezenas = ["", "", "vinte", "trinta", "quarenta", "cinquenta", "sessenta", "setenta", "oitenta", "noventa"]

-----------------------------------------------------
-- Q.13
-- Objetivo: Reolve equacoes do segundo grau
-----------------------------------------------------
equacao2G :: Float -> Float -> Float -> (Float,Float)
equacao2G a b c = (x1 a b c, x2 a b c)

delta :: Float -> Float -> Float -> Float
delta a b c = b ^ 2 - 4 * a * c

x1 :: Float -> Float -> Float -> Float
x1 a b c = ((-1) * b + sqrt(delta a b c)) / (2 * a)

x2 :: Float -> Float -> Float -> Float
x2 a b c = ((-1) * b - sqrt(delta a b c)) / (2 * a)

-----------------------------------------------------
-- Q.14
-- Objetivo: Retorna o maior valor entre tres valores inteiros
-----------------------------------------------------
maior3 :: Int -> Int -> Int -> Int
maior3 num1 num2 num3 =
  if (num1 >= num2 && num1 >= num3) then num1
  else if (num2 >= num3) then num2
  else num3

-----------------------------------------------------
-- Q.15
-- Objetivo: Ordena uma tupla com tres valores inteiros
-----------------------------------------------------
ordena :: (Int, Int, Int) -> (Int, Int, Int)
ordena (num1, num2, num3) =
  if(num1 > num2) then ordena(num2, num1, num3)
  else if(num2 > num3) then ordena(num1, num3, num2)
  else (num1, num2, num3)

-----------------------------------------------------
-- Q.16
-- Objetivo: Criptografar e descriptografar uma mensagem
-----------------------------------------------------
criptografarMensagem :: String -> String
criptografarMensagem string = criptografarMensagem2 "" string
criptografarMensagem2 :: String -> String -> String
criptografarMensagem2 mensagem [] = mensagem
criptografarMensagem2 mensagem (cabeca:cauda) = criptografarMensagem2 (mensagem ++ [(cifra !! (elementIndex cabeca alfabeto))]) cauda
  where
    alfabeto = ['A', 'B' ..'Z'] ++ ['a', 'b' ..'z'] ++ [' ']
    cifra = ['D', 'E' .. 'Z'] ++ ['A', 'B', 'C'] ++ ['d', 'e' .. 'z'] ++ ['a', 'b', 'c'] ++ [' ']

descriptografarMensagem :: String -> String
descriptografarMensagem string = descriptografarMensagem2 "" string
descriptografarMensagem2 :: String -> String -> String
descriptografarMensagem2 mensagem [] = mensagem
descriptografarMensagem2 mensagem (cabeca:cauda) = descriptografarMensagem2 (mensagem ++ [(alfabeto !! (elementIndex cabeca cifra))]) cauda
  where
    alfabeto = ['A', 'B' ..'Z'] ++ ['a', 'b' ..'z'] ++ [' ']
    cifra = ['D', 'E' ..'Z'] ++ ['A', 'B', 'C'] ++ ['d', 'e' ..'z'] ++ ['a', 'b', 'c'] ++ [' ']

elementIndex :: Char -> [Char] -> Int
elementIndex char lista = elementIndex2 0 char lista
elementIndex2 :: Int -> Char -> [Char] -> Int
elementIndex2 cont char [] = (-1)
elementIndex2 cont char (cabeca:cauda)
  | cabeca == char = cont
  | otherwise = elementIndex2 (cont + 1) char cauda

\end{code}
