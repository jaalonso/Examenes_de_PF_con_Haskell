-- Informática (1º del Grado en Matemáticas)
-- 3º examen de evaluación continua (23 de enero de 2014)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. El factorial de 7 es
--    7! = 1 * 2 * 3 * 4 * 5 * 6 * 7 = 5040
-- por tanto, el último dígito no nulo del factorial de 7 es 4.
-- 
-- Definir la función
--    ultimoNoNuloFactorial :: Integer -> Integer
-- tal que (ultimoNoNuloFactorial n) es el último dígito no nulo del
-- factorial de n. Por ejemplo,
--    ultimoNoNuloFactorial 7  ==  4
-- ---------------------------------------------------------------------

ultimoNoNuloFactorial :: Integer -> Integer
ultimoNoNuloFactorial n = ultimoNoNulo (factorial n)

-- (ultimoNoNulo n) es el último dígito no nulo de n. Por ejemplo,
--    ultimoNoNulo 5040  ==  4
ultimoNoNulo :: Integer -> Integer
ultimoNoNulo n | m /= 0    = m
               | otherwise = ultimoNoNulo (n `div` 10)
               where m = n `rem` 10

-- 2ª definición (por comprensión)
ultimoNoNulo2 :: Integer -> Integer
ultimoNoNulo2 n = read [head (dropWhile (=='0') (reverse (show n)))]

-- (factorial n) es el factorial de n. Por ejemplo,
--    factorial 7  ==  5040
factorial :: Integer -> Integer
factorial n = product [1..n]

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Una lista se puede comprimir indicando el número de
-- veces consecutivas que aparece cada elemento. Por ejemplo, la lista 
-- comprimida de [1,1,7,7,7,5,5,7,7,7,7] es [(2,1),(3,7),(2,5),(4,7)],
-- indicando que comienza con dos 1, seguido de tres 7, dos 5 y cuatro
-- 7. 
--
-- Definir, por comprensión, la función 
--    expandidaC :: [(Int,a)] -> [a]
-- tal que (expandidaC ps) es la lista expandida correspondiente a ps
-- (es decir, es la lista xs tal que la comprimida de xs es ps). Por
-- ejemplo, 
--    expandidaC [(2,1),(3,7),(2,5),(4,7)]  ==  [1,1,7,7,7,5,5,7,7,7,7]
-- ---------------------------------------------------------------------

expandidaC :: [(Int,a)] -> [a]
expandidaC ps = concat [replicate k x | (k,x) <- ps]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, por recursión, la función 
--    expandidaR :: [(Int,a)] -> [a]
-- tal que (expandidaR ps) es la lista expandida correspondiente a ps
-- (es decir, es la lista xs tal que la comprimida de xs es ps). Por
-- ejemplo, 
--    expandidaR [(2,1),(3,7),(2,5),(4,7)]  ==  [1,1,7,7,7,5,5,7,7,7,7]
-- ---------------------------------------------------------------------

expandidaR :: [(Int,a)] -> [a]
expandidaR []         = []
expandidaR ((n,x):ps) = replicate n x ++ expandidaR ps

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Un número n es de Angelini si n y 2n tienen algún
-- dígito común. Por ejemplo, 2014 es un número de Angelini ya que 2014
-- y su doble (4028) comparten los dígitos 4 y 0.
--
-- Definir la función
--    angelini :: Integer -> Bool
-- tal que (angelini n) se verifica si n es un número de Angelini. Por
-- ejemplo, 
--    angelini 2014  ==  True
--    angelini 2067  ==  False
-- ---------------------------------------------------------------------

-- 1ª definición (con any)
angelini :: Integer -> Bool
angelini n = any (`elem` (show (2*n))) (show n)  

-- 2ª definición (por comprensión)
angelini2 :: Integer -> Bool
angelini2 n = not (null [x | x <- show n, x `elem` show (2*n)])

-- 3ª definición (por recursión)
angelini3 :: Integer -> Bool
angelini3 n = aux (show n) (show (2*n))
    where aux [] _      = False
          aux (x:xs) ys = x `elem` ys || aux xs ys

-- 4ª definición (por plegado)
angelini4 :: Integer -> Bool
angelini4 n = aux (show n) 
    where aux   = foldr f False
          f x y = x `elem` show (2*n) || y

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. ¿Cuál es el primer año que no será de Angelini?
-- ---------------------------------------------------------------------

-- El cálculo es
--    ghci> head [n | n <- [2014..], not (angelini n)]
--    2057

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. El número 37 es primo y se puede escribir como suma de
-- primos menores distintos (en efecto, los números 3, 11 y 23 son
-- primos y su suma es 37.
--
-- Definir la función
--    primoSumaDePrimos :: Integer -> Bool
-- tal que (primoSumaDePrimos n) se verifica si n es primo y se puede
-- escribir como suma de primos menores que n. Por ejemplo,
--    primoSumaDePrimos 37  ==  True
--    primoSumaDePrimos 39  ==  False
--    primoSumaDePrimos 11  ==  False
-- ---------------------------------------------------------------------

primoSumaDePrimos :: Integer -> Bool
primoSumaDePrimos n = primo n && esSumaDePrimos n

-- (esSumaDePrimos n) se verifica si n es una suma de primos menores que
-- n. Por ejemplo,
--    esSumaDePrimos 37  ==  True
--    esSumaDePrimos 11  ==  False
esSumaDePrimos :: Integer -> Bool
esSumaDePrimos n = esSuma n [x | x <- 2:[3,5..n-1], primo x]

-- (primo n) se verifica si n es primo. Por ejemplo,
--    primo 37  ==  True
--    primo 38  ==  False
primo :: Integer -> Bool
primo n = [x | x <- [1..n], n `rem` x == 0] == [1,n]

-- (esSuma n xs) s verifica si n es suma de elementos de xs. Por ejemplo,
--    esSuma 20 [4,2,7,9]  ==  True
--    esSuma 21 [4,2,7,9]  ==  False
esSuma :: Integer -> [Integer] -> Bool
esSuma 0 _                  = True                    
esSuma n []                 = False 
esSuma n (x:xs) | n == x    = True
                | n > x     = esSuma n xs || esSuma (n-x) xs
                | otherwise = esSuma n xs

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. ¿Cuál será el próximo año primo suma de primos? ¿y el
-- anterior? 
-- ---------------------------------------------------------------------

-- El cálculo es 
--    ghci> head [p | p <- [2014..], primoSumaDePrimos p]
--    2017
--    ghci> head [p | p <- [2014,2013..], primoSumaDePrimos p]
--    2011
