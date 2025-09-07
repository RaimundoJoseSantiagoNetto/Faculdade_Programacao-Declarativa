import Data.Char (chr)
import Data.Char (ord)

--------------------------------------------------
-- Q.01
-- Objetivo: Calcula a area do triangulo
--------------------------------------------------
areaTriangulo :: Float -> Float -> Float
areaTriangulo base altura = base * altura / 2

---------------------------------------------------
-- Q.02
-- Objetivo: Verifica o menor valor de tres numeros
----------------------------------------------------
menorTres :: Int -> Int -> Int -> Int
menorTres num1 num2 num3 
    | (num1 < num2) && (num1 < num3) = num1
    | (num2 < num1) && (num2 < num3) = num2
    | (num3 < num1) && (num3 < num2) = num3

---------------------------------------------------
-- Q.03
-- Objetivo: Verifica se quatro numeros sao iguais
----------------------------------------------------
quatroIguais :: Int -> Int -> Int -> Int -> Bool
quatroIguais num1 num2 num3 num4 = (num1 == num2) && (num2 == num3) && (num3 == num4)

---------------------------------------------------
-- Q.04
-- Objetivo: Verifica se tres valores sao diferentes
----------------------------------------------------
tresDiferentes :: Int -> Int -> Int ->  Bool
tresDiferentes num1 num2 num3 = (num1 /= num2) && (num1 /= num3) && (num2 /= num3)

---------------------------------------------------
-- Q.05
-- Objetivo: Calcula a media ponderada de quatro valores
----------------------------------------------------
mediaPonderadaQuatro :: Float -> Float -> Float -> Float -> Float
mediaPonderadaQuatro num1 num2 num3 num4 = (num1 + num2 * 2 + num3 * 3 + num4 * 4) / 10

---------------------------------------------------
-- Q.06
-- Objetivo: Aplica descondo de 20% ao preco de um produto
----------------------------------------------------
calcularDesconto :: Float -> Float
calcularDesconto preco = preco * 0.8

---------------------------------------------------
-- Q.07
-- Objetivo: Calcula o XOR de dois valores logicos
----------------------------------------------------
xor :: Bool -> Bool -> Bool
xor valor1 valor2 = (valor1 || valor2) && not(valor1 && valor2)

---------------------------------------------------
-- Q.08
-- Objetivo: Devolve o decimo termo de uma PA
----------------------------------------------------
decimoTermo :: Float -> Float -> Float
decimoTermo primeiroTermo razao = primeiroTermo + (10 - 1) * razao

---------------------------------------------------
-- Q.09
-- Objetivo: Verifica se um numero eh positivo, negativo ou nulo
----------------------------------------------------
verificarNumero :: Float -> String
verificarNumero numero =
    if(numero > 0) then "Positivo"
    else if(numero < 0) then "Negativo"
    else "Nulo"

---------------------------------------------------
-- Q.10
-- Objetivo: Calcula o NAND de dois valores logicos
----------------------------------------------------
nand :: Bool -> Bool -> Bool
nand valor1 valor2 = not(valor1 && valor2)

---------------------------------------------------
-- Q.11
-- Objetivo: Verifica se o caracter eh um digito
----------------------------------------------------

-- de 48 a 57 sao os valores de 0 a 9 na tabela ASCII

ehDigito :: Char -> Bool
ehDigito char = (ord char) >= 40 && (ord char) <= 57

---------------------------------------------------
-- Q.12
-- Objetivo: Converte um caracter para maiuscula
----------------------------------------------------

-- codigo das letras maiusculas 65 - 90
-- cogigo das letras minusculas 97 - 122

paraMaiuscula :: Char -> Char
paraMaiuscula char =
    if(ord char >= 97 && ord char <= 122) then chr(65 + incremento)
    else char
    where 
        incremento = ord(char) - 97

---------------------------------------------------
-- Q.13
-- Objetivo: Converte um caracter para minuscula
----------------------------------------------------

paraMinuscula :: Char -> Char
paraMinuscula char =
    if(ord char >= 65 && ord char <= 90) then chr(97 + incremento)
    else char
    where 
        incremento = ord(char) - 65

---------------------------------------------------
-- Q.14
-- Objetivo: Soma os algarismos de um numero inteiro de 0 a 999
----------------------------------------------------
centenas :: Int -> Int
centenas num = div (mod num 1000 - dezenas num - unidades num) 100

dezenas :: Int -> Int
dezenas num = div (mod num 100 - unidades num) 10

unidades :: Int -> Int
unidades num = mod num 10

somaAlgarismos :: Int -> Int
somaAlgarismos num = centenas num + dezenas num + unidades num

---------------------------------------------------
-- Q.15
-- Objetivo: Calcula a media aritmetica de tres valores
----------------------------------------------------
mediaTres :: Float -> Float -> Float -> Float
mediaTres num1 num2 num3 = (num1 + num2 + num3)/3

---------------------------------------------------
-- Q.16
-- Objetivo: Verificar triangulo
----------------------------------------------------
verificarTriangulo :: Float -> Float -> Float -> Bool
verificarTriangulo num1 num2 num3 = (num1 + num2 > num3) && (num1 + num3 > num2) && (num2 + num3 > num1) 

---------------------------------------------------
-- Q.17
-- Objetivo: Retorna o tipo do triangulo
----------------------------------------------------
descobrirTriangulo :: Float -> Float -> Float -> String
descobrirTriangulo num1 num2 num3
    | (num1 == num2 && num1 == num3) || (num2 == num1 && num2 == num3) || (num3 == num1 && num3 == num2) = "Isoiceles"
    | num1 == num2 && num2 == num3 = "Equilatero"
    | otherwise = "Escaleno"

---------------------------------------------------
-- Q.18
-- Objetivo: Calcula a soma de um progressao aritmetica finita
----------------------------------------------------
somaFinita :: Float -> Float -> Float -> Float
somaFinita a1 an n = (a1 + an) * n / 2

---------------------------------------------------
-- Q.19
-- Objetivo: Localiza o ponto no plano cartesiano
----------------------------------------------------
localizacaoNoPlano :: Float -> Float -> String
localizacaoNoPlano x y 
    | x == 0 && y == 0 = "Origem"
    | x == 0 && y > 0 = "Eixo y positivo"
    | x == 0 && y < 0 = "Eixo y negativo"
    | x > 0 && y == 0 = "Eixo x positivo"
    | x < 0 && y == 0 = "Eixo x negativo"
    | x > 0 && y > 0 = "Primeiro quadrante"
    | x > 0 && y < 0 = "Quarto quadrante"
    | x < 0 && y > 0 = "Segundo quadrante"
    | x < 0 && y < 0 = "Terceiro quadrante"

---------------------------------------------------
-- Q.20
-- Objetivo: Le um numero inteiro e retorna o nome do mes
----------------------------------------------------
qualMes :: Int -> String
qualMes num 
    | num >= 1 && num <= 12 = meses !! (num - 1)
    | otherwise = error "Valor invalido" 
    where meses = ["Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"]

-----------------------------------------------------
-- Q.21
-- Objetivo: Verifica se um numero eh par
-----------------------------------------------------
ehPar :: Int -> Bool
ehPar num | mod num 2 == 0 = True
  | otherwise = False

-----------------------------------------------------
-- Q.22
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
-- Q.23
-- Objetivo: Converte a temperatura de Fareiheit para Celsius
-----------------------------------------------------
converterTemperatura :: Float -> Float 
converterTemperatura f | f == 32 = 0 | otherwise = 5 / 9 * (f - 32)

---------------------------------------------------
-- Q.24
-- Objetivo: Calcula o IMC de uma pessoa
----------------------------------------------------
calcularIMC :: Float -> Float -> String
calcularIMC peso altura 
    | imc < 18.5 = "Abaixo do peso"
    | imc < 24.9 = "Peso normal"
    | imc < 29.9 = "Sobrepeso"
    | imc < 34.9 = "Obesidade grau I"
    | imc < 39.9 = "Obesidade grau II"
    | otherwise = "Obesidade grau III"
    where imc = peso / altura ^ 2

---------------------------------------------------
-- Q.25
-- Objetivo: Clasifica a pessoa, conforme a idade
----------------------------------------------------
classificarIdade :: Int -> String
classificarIdade idade 
    | idade < 13 = "Crianca"
    | idade < 21 = "Adolecente"
    | idade < 60 = "Adulto"
    | otherwise = "Idoso"

---------------------------------------------------
-- Q.26
-- Objetivo: Classifica uma nota
----------------------------------------------------
classificarNota :: Int -> String
classificarNota nota
    | nota < 49 = "F"
    | nota < 59 = "D"
    | nota < 69 = "C"
    | nota < 79 = "B"
    | nota <= 100 = "A"

---------------------------------------------------
-- Q.27
-- Objetivo: Transforma o caracter em seu sucessor - dos caracteres alfabeticos
----------------------------------------------------
proximoCaracter :: Char -> Char
proximoCaracter char =
    if((ord char >= 97 && ord char <= 122) || (ord char >= 65 && ord char <= 90)) then 
        if(ord char == 122)  then chr 97
        else if(ord char == 90) then chr 65
        else chr(ord char + 1)
    else error "Nao eh um caracter alfabetico"

---------------------------------------------------
-- Q.28
----------------------------------------------------
-- A) Eh um estilo ou modelo de programacao que define como os problemas devem ser resolvidos 
-- e como o codigo deve ser estruturado. Exemplos: imperativo, funcional, orientado a objetos, logico

-- B) Baseia-se no uso de funções matematicas, evitando estados mutaveis e efeitos colaterais. 
-- Programas funcionais sao compostos pela aplicacao e composicao de funcoes puras

-- C) Modelo em que o programador descreve passo a passo as instrucoes que o computador deve 
-- executar, modificando estados e variaveis. Ex.: C, Java, Python (quando usados de forma imperativa)

-- D) Propriedade de um dado que, uma vez criado, não pode ser alterado. Em vez de modificar valores, 
-- gera-se uma nova versão

-- E) Qualquer mudanca de estado que ocorre fora do escopo da funcao. Exemplos: alterar uma variavel global, 
-- modificar um arquivo, imprimir na tela

-- F) Funcoes que, para os mesmos parametros de entrada, sempre retornam o mesmo resultado e nao produzem efeitos colaterais

-- G) Propriedade de uma expressao que pode ser substituida pelo seu valor sem alterar o comportamento do programa. 
-- Isso so eh garantido quando se usam funcoes puras

-- H) Funcao que pode receber outras funcoes como argumento e/ou retornar funcoes como resultado. Exemplo: map, filter, fold

-- I) Capacidade de algumas linguagens de programacao de deduzirem automaticamente o tipo de uma expressao ou variavel, 
-- sem que o programador precise especificar explicitamente (como em Haskell)

---------------------------------------------------
-- Q.29
----------------------------------------------------
-- A) Retorna o valor absoluto (modulo) de um numero

-- B) Retorna o sinal do numero:
-- -1 se for negativo
-- 0 se for zero
-- 1 se for positivo

-- C) Retorna a raiz quadrada (resultado em numero de ponto flutuante)

-- D) Calcula e^x (exponencial de base e)

-- E) Logaritmo natural (base e)

-- F) Inverte o sinal de um numero

-- G) Logaritmo em qualquer base

-- H) Arredonda um numero para baixo (sempre para o menor inteiro mais proximo)

-- I) Arredonda um numero para cima (retorna Integer)

-- J) Arredonda para o inteiro mais proximo

-- K) Corta a parte decimal, aproximando para zero

---------------------------------------------------
-- Q.30
----------------------------------------------------
-- A) 
-- truncate 5.4 -> 5
-- floor 6.7 -> 6
-- ceiling 8.9 -> 9
-- abs (-7) -> 7
-- signum 8 -> 1
-- Somando: 5 + 6 + 9 + 7 + 1 = 28

-- B) 
-- 0xFF -> numero hexadecimal
-- FF = 15*16 + 15 = 255
-- 0b1111 -> numero binario
-- 1111 = 15
-- 0o12 -> numero octal
-- 12 = 1*8 + 2 = 10
-- Somando: 255 + 15 + 10 = 280

---------------------------------------------------
-- Q.31
----------------------------------------------------
-- i)
-- dobro (dobro 2)
-- = dobro (2 + 2)
-- = dobro 4
-- = 4 + 4
-- = 8

-- ii)
-- dobro (dobro 2)
-- = (dobro 2) + (dobro 2) 
-- = (2 + 2) + (2 + 2)
-- = 4 + 4
-- = 8

---------------------------------------------------
-- Q.32
----------------------------------------------------
-- A avaliacao estrita (eager evaluation) eh a estrategia em que os argumentos de
-- uma funcao sao avaliados imediatamente, antes da execucao da funcao, o que torna
-- o comportamento mais previsivel, porem pode gastar tempo com calculos desnecessarios;
-- ja a avaliacao preguicosa (lazy evaluation), usada em Haskell, adia a avaliacao dos 
-- argumentos ate que seu valor seja realmente necessario, o que permite trabalhar com 
-- estruturas infinitas e evitar calculos inuteis, mas pode dificultar o controle do uso
-- de memoria e gerar atrasos inesperados na execucao

---------------------------------------------------
-- Q.33 
----------------------------------------------------
-- Haskell utiliza avaliacao preguicosa por padrao, o que significa que as expressoes so sao
-- avaliadas quando seus valores sao realmente necessarios; por exemplo, a lista infinita 
-- naturais = [1..] nao causa erro, pois nada eh calculado ate que um valor seja requisitado, 
-- e ao pedir take 5 naturais, o Haskell gera apenas os cinco primeiros numeros [1,2,3,4,5], 
-- sem precisar avaliar a lista inteira.

---------------------------------------------------
-- Q.34
----------------------------------------------------
-- Em java, o codigo da erro, porque ele avalia todas as expressoes dos parametros
-- antes de executar as linhas das funcao - eager avaluation. Por outro lado, em Haskell,
-- o problema nao ocorre porque o interpretador nao efetua todos os calculos dos parametros.
-- Pelo fato de as linhas de codigo da funcao nao utilizarem o resultado da divisao do parametro
-- nenhuma excessao eh lancada - por conta da lazy avaluation

---------------------------------------------------
-- Q.35
----------------------------------------------------
quantidadeDigitos :: Integer -> Int
quantidadeDigitos n = floor (logBase 10 (fromIntegral n)) + 1

---------------------------------------------------
-- Q.36
----------------------------------------------------
-- Em Haskell, -> eh associativo a direita.
-- Portanto f :: Int -> (Int -> Int) equivale a f :: Int -> Int -> Int (letra C está correta)

---------------------------------------------------
-- Q.37
----------------------------------------------------
-- A) Paradigma Declarativo

-- B) Paradigma Imperativo

-- C) Paradigma Imperativo

-- D) Paradigma Imperativo

-- E) Paradigma Estruturado

-- F) Paradigma Orientado a Objetos

-- G) Paradigma Declarativo

-- H) Paradigma Lógico

-- I) Paradigma Funcional

---------------------------------------------------
-- Q.38
----------------------------------------------------
-- A funcao soma eh uma funcao pura e a funcao incremento eh uma funcao impura. Na funcao soma
-- as mesmas entradas sempre gerarao os mesmos retornos, enquanto no incremento, por haver
--  modificacao de uma variavel global, o resultado sempre sera diferente para cada vez que a
-- funcao eh chamada, mesmo que seja utilizado o mesmo parametro varias vezes

---------------------------------------------------
-- Q.39
----------------------------------------------------
-- O codigo C utiliza eager evaluation (avaliacao ansiosa), e por isso calculo_pesado() eh executado mesmo sem ser usado

---------------------------------------------------
-- Q.40
----------------------------------------------------
-- A) Falso
-- Haskell eh essencialmente uma linguagem funcional pura (nao eh multi-paradigma como Python ou Scala, por exemplo)

-- B) Falso
-- Haskell usa lazy evaluation (avaliacao preguicosa) por padrao, ou seja, so avalia expressoes quando o valor eh necessario

-- C) Falso
-- Haskell tem tipagem estatica e forte: os tipos sao verificados em tempo de compilacao

-- D) Falso
-- Eh justamente o contrario: Haskell eh uma linguagem de funcoes puras, onde funcoes nao tem efeitos colaterais. 
-- Para tratar efeitos colaterais (I/O, estado, etc.), usa-se o conceito de monads

-- E) Falso
-- Os dados em Haskell sao imutaveis por padrao. Uma vez criado um valor, ele nao pode ser alterado

-- F) Verdadeiro
-- Haskell tem um poderoso sistema de inferencia de tipos (baseado no algoritmo Hindley–Milner). 
-- Muitas vezes nao eh necessario anotar os tipos, pois o compilador consegue deduzi-los automaticamente
