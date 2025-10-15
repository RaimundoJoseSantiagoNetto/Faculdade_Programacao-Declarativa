-----------------------------------------------------
-- Q.01
-----------------------------------------------------

type Nome = String
type Idade = Int

data Pessoa = Pessoa Nome Idade deriving (Show, Eq)

p1 = Pessoa "Ana" 20
p2 = Pessoa "Carlos" 25
p3 = Pessoa "Marcus" 12

-- A) Verifica a maioridade de uma pessoa

maiorIdade :: Pessoa -> Bool
maiorIdade (Pessoa _ idade) = idade > 18

-- B) Adiciona um a idade atual de uma pessoa

aniversario :: Pessoa -> Pessoa
aniversario (Pessoa nome idade) = Pessoa nome (idade + 1)

-----------------------------------------------------
-- Q.02
-- Objetivo: Calcula o perimetro de formas geometricas
-----------------------------------------------------
  
data Forma = Circulo Float | Retangulo Float Float -- medidas em centimetros

cir = Circulo 5
ret = Retangulo 3 4


perimetro :: Forma -> Float
perimetro (Circulo raio) = 2 * pi * raio
perimetro (Retangulo altura largura) = 2 * altura + 2 * largura

-----------------------------------------------------
-- Q.03
-----------------------------------------------------

data ListaInt = Vazia | Cons Int ListaInt deriving (Show, Eq)

l1 = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Vazia))))

-- A) Remove o ultimo elemento de uma lista

removeUltimo :: ListaInt -> ListaInt
removeUltimo Vazia = Vazia
removeUltimo (Cons _ Vazia) = Vazia
removeUltimo (Cons cabeca cauda) = Cons cabeca (removeUltimo cauda)


-- B) Dobra os elementos da lista

dobrarLista :: ListaInt -> ListaInt
dobrarLista Vazia = Vazia
dobrarLista (Cons cabeca cauda) = Cons (cabeca * 2) (dobrarLista cauda)

-----------------------------------------------------
-- Q.04
-----------------------------------------------------

data Arvore a = Nula | No a (Arvore a) (Arvore a) deriving (Show, Eq)

arvbin = No 1 (No 2 Nula Nula) (No 3 Nula Nula)

-- A) Calcula o numero de nos de uma arvore binaria

contarNos :: Arvore a -> Int
contarNos Nula = 0
contarNos (No _ arv1 arv2) = 1 + contarNos arv1 + contarNos arv2

-- B) Calcula o numero de folhas de uma arvore binaria

contarFolhas :: Arvore a -> Int
contarFolhas Nula = 1
contarFolhas (No _ arv1 arv2) = contarFolhas arv1 + contarFolhas arv2

-- C) Verifica qual o maior elemento de uma arvore binaria

maiorElemento :: Ord a => Arvore a -> a
maiorElemento Nula = error "Arvore vazia nao possui maior elemento"
maiorElemento (No x Nula Nula) = x
maiorElemento (No x esq Nula)  = max x (maiorElemento esq)
maiorElemento (No x Nula dir)  = max x (maiorElemento dir)
maiorElemento (No x esq dir)   = maximum [x, maiorElemento esq, maiorElemento dir]

-----------------------------------------------------
-- Q.05 e Q.06
-- Objetivo: Cria o tipo expressao e avalia expressoes
-----------------------------------------------------

data Expressao = 
  Valor Double
  | Soma Expressao Expressao
  | Subtracao Expressao Expressao
  | Mult Expressao Expressao
  | Div Expressao Expressao
  deriving (Show)

expr = Soma (Valor 2) (Mult (Valor 3) (Valor 4))

avaliar :: Expressao -> Double
avaliar (Valor num) = num
avaliar (Soma e1 e2) = avaliar e1 + avaliar e2
avaliar (Subtracao e1 e2) = avaliar e1 - avaliar e2
avaliar (Mult e1 e2) = avaliar e1 * avaliar e2
avaliar (Div e1 e2)
  | avaliar e2 == 0 = error "Nao pode ser feita divisao por zero"
  | otherwise = avaliar e1 / avaliar e2

-----------------------------------------------------
-- Q.07
-----------------------------------------------------

-- lambster.dev

true  = \x y -> x
false = \x y -> y
--and'  = \p q -> p q p
--or'   = \p q -> p p q
--not'  = \p -> p false true
--xor'  = \p q -> p (not' q) q

-- parece que nao da para fazer

-- A)

-- B)

-- C)

-- D)

-- E)

-- F)

-- G)

-- H)

-- I)

-- J)

-- K)

-- L)

-- M)

-- N)

-- O)

-- P)

-----------------------------------------------------
-- Q.08
-----------------------------------------------------

-- A) [a] -> a

-- B) (a, b) -> (b, a)

-- C) a -> b -> (a, b)

-- D) Num a => a -> a

-- E) Eq a => [a] -> Bool

-- F) (a -> a) -> a -> a

-----------------------------------------------------
-- Q.09
-- Objetivo: Substitui um valor por outro em uma lista
-----------------------------------------------------

substitui :: Eq a => a -> a -> [a] -> [a]
substitui _ _ [] = []
substitui valorQueSeraTrocado novoValor (cabeca:cauda)
  | cabeca == valorQueSeraTrocado = novoValor : substitui valorQueSeraTrocado novoValor cauda
  | otherwise = cabeca : substitui valorQueSeraTrocado novoValor cauda

-----------------------------------------------------
-- Q.10
-----------------------------------------------------

-- A)
-- Tipo: Num a => [a]
-- Valor: [2,3,4]

-- B)
-- Tipo: (Ord a, Num a) => [Bool]
-- Valor: [True, False, False, False]

-- C)
-- Tipo: (Ord a, Num a) => [a]
-- Valor: [6]

-- D)
-- Tipo: Integral a => [a]
-- Valor: [2,4,6,8,10]

-- E)
-- Tipo: (Ord a, Num a) => [a]
-- Valor: [9,4,1,1,4,9]

-- F)
-- Tipo: (Ord a, Num a) => [a]
-- Valor: [1,4,9]

-- G)
-- Tipo: [String]
-- Valor: ["As","artes","dos","alunos"]

-- H)
-- Tipo: [String]
-- Valor: ["sO","saluno","sbem-comportado"]

-- I)
-- Tipo: Num a => [[a]]
-- Valor: [[1,4],[9,16,25]]

-----------------------------------------------------
-- Q.11
-- Objetivo: Aplica uma lista de funcoes a uma lista de valores
-----------------------------------------------------

aplica :: [a -> a] -> [a] -> [a]
aplica [] lst = lst
aplica (cabeca1:cauda1) lst = aplica cauda1 (map cabeca1 lst)

-----------------------------------------------------
-- Q.12
-----------------------------------------------------

-- A)
-- Tipo: Num a => a -> a
-- Valor: eh uma funcao que soma 1 ao argumento

-- B)
-- Tipo: Num a => a
-- Valor: 6 + 1 = 7

-- C)
-- Tipo: Ord a, Num a => a -> Bool (recebe um numero e retorna se ele eh maior que zero)
-- Valor: eh uma funcao booleana

-- D)
-- Tipo: Num a => a -> a -> a (funcao que recebe dois numeros e retorna sua soma)
-- Valor: funcao soma de dois argumentos | λx. λy. x + y

-- E)
-- Tipo: Num a => a -> a
-- Valor: λy. 7 + y (funcao que soma 7 ao argumento)

-- F)
-- Tipo: Num a => a
-- Valor: 7 + 3 = 10

-- G)
-- Tipo: Num a => a -> (a -> a)
-- Valor: funcao que, dado x, retorna outra funcao que soma x ao argumento y | λx. (λy. x + y)

-- H)
-- Tipo: (a -> a) -> a -> a (recebe uma funcao f e um valor x, aplica f duas vezes sobre x)
-- Valor: λf. λx. f (f x)

-- I)
-- Tipo: Num a => a -> a
-- Valor: λx. x + 2

-----------------------------------------------------
-- Q.13
-----------------------------------------------------

-- 1) [True,False,True,False,True]

-- 2) [1,3,5]

-- 3) [1,2,7,1]

-- 4) [6,3]

-- 5) [9,11,13,15]

-----------------------------------------------------
-- Q.14
-- Objetivo: Representa uma expressao boleana
-----------------------------------------------------

data BoolExpr = Const Bool  -- constante booleana (True ou False)
  | Var String              -- variavel booleana (ex: "x")
  | Not BoolExpr            -- negacao de uma expressao
  | And BoolExpr BoolExpr   -- conjuncao (E)
  | Or  BoolExpr BoolExpr   -- disjuncao (OU)
  deriving (Show, Eq)

-----------------------------------------------------
-- Q.15
-- Objetivo: 
-----------------------------------------------------

type Memoria = [(String, Bool)]

-----------------------------------------------------
-- Q.16
-- Objetivo: Avalisa uma expressao com os valores da memoria
-----------------------------------------------------

eval :: Memoria -> BoolExpr -> Bool
eval mem (Const b) = b
eval mem (Var x) = 
  case lookup x mem of
    Just v  -> v
    Nothing -> False
eval mem (Not e) = not (eval mem e)
eval mem (And e1 e2) = (eval mem e1) && (eval mem e2)
eval mem (Or  e1 e2) = (eval mem e1) || (eval mem e2)

-----------------------------------------------------
-- Q.17
-----------------------------------------------------

data OP = SOMA | SUB | PROD | DIV deriving (Show, Eq)

data Expr = Folha Int | Nodo OP Expr Expr deriving (Show, Eq)

-- A)

aplica :: OP -> Int -> Int -> Int
aplica SOMA x y = x + y
aplica SUB  x y = x - y
aplica PROD x y = x * y
aplica DIV  x y = x `div` y  -- divisao inteira

-- B)

avalia :: Expr -> Int
avalia (Folha n) = n
avalia (Nodo op e1 e2) = aplica op (avalia e1) (avalia e2)

-- C)

imprime :: Expr -> String
imprime (Folha n) = show n
imprime (Nodo op e1 e2) = "(" ++ imprime e1 ++ " " ++ mostraOp op ++ " " ++ imprime e2 ++ ")"

mostraOp :: OP -> String
mostraOp SOMA = "+"
mostraOp SUB  = "-"
mostraOp PROD = "*"
mostraOp DIV  = "/"

-----------------------------------------------------
-- Q.18
-----------------------------------------------------

data Part = AM | PM deriving (Eq, Show)
data TIME = Local Int Int Part -- Formato em 12 horas
| Total Int Int -- Formato em 24 horas

time1 :: TIME
time1 = Local 3 45 PM -- 3:45 PM
time2 :: TIME
time2 = Local 11 30 AM -- 11:30 AM
time3 :: TIME
time3 = Total 15 20 -- 15:20 (3:20 PM)
time4 :: TIME
time4 = Total 9 5 -- 09:05 AM

-- A)

totalMinutos :: TIME -> Int
totalMinutos (Total h m) = h * 60 + m
totalMinutos (Local h m AM) = ((if h == 12 then 0 else h) * 60) + m
totalMinutos (Local h m PM) = ((if h == 12 then 12 else h + 12) * 60) + m

-- B)

instance Eq TIME 
  where
    t1 == t2 = totalMinutos t1 == totalMinutos t2

-- C)

  instance Ord TIME 
    where
      compare t1 t2 = compare (totalMinutos t1) (totalMinutos t2)
