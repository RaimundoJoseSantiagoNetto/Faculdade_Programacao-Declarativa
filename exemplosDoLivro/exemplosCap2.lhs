-----------------------------------------------------
-- Exemplos do capitulo 2 do livro
-----------------------------------------------------

\begin{code}

-----------------------------------------------------
-- Q.2-9
-- Objetivo: Calcula a funcao nand
-----------------------------------------------------
nAnd :: Bool -> Bool -> Bool
nAnd p q = not(p && q)

-----------------------------------------------------
-- Q.2-14
-- Objetivo: Verifica se um caracter eh maiusculo e
-- Verifica se um caracter eh minusculo
-----------------------------------------------------
ehMaiuscula :: Char -> Bool
ehMaiuscula char = ('A' <= char) && (char <= 'Z')

ehMinuscula :: Char -> Bool
ehMinuscula char = ('a' <= char) && (char <= 'z')

-----------------------------------------------------
-- Q.2-16
-- Objetivo: Concatena tres Strings
-----------------------------------------------------
linhas :: String -> String -> String -> String
linhas s1 s2 s3 = s1 ++ s2 ++ s3

\end{code}