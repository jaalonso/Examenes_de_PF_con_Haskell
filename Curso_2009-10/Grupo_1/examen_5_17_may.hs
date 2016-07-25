-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 5º examen de evaluación continua (17 de mayo de 2010)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir Haskell la función 
--    primo :: Int -> Integer
-- tal que (primo n) es el n-ésimo número primo. Por ejemplo, 
--    primo 5 = 11 
-- ---------------------------------------------------------------------

primo :: Int -> Integer
primo n = primos !! (n-1)

-- primos es la lista de los números primos. Por ejemplo,
--    take 10 primos  ==  [2,3,5,7,11,13,17,19,23,29]
primos :: [Integer]
primos = 2 : [n | n <- [3,5..], esPrimo n]

-- (esPrimo n) se verifica si n es primo. 
esPrimo :: Integer-> Bool
esPrimo n = [x | x <- [1..n], rem n x == 0] == [1,n]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función
--    sumaCifras :: Integer -> Integer
-- tal que (sumaCifras n) es la suma de las cifras del número n. Por
-- ejemplo, 
--    sumaCifras 325 = 10
-- ---------------------------------------------------------------------

sumaCifras :: Integer -> Integer
sumaCifras n 
    | n < 10    = n
    | otherwise = sumaCifras(div n 10) + n `rem` 10

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la función
--    primosSumaPar :: Int -> [Integer]
-- tal que (primosSumaPar n) es el conjunto de elementos del conjunto de
-- los n primeros primos tales que la suma de sus cifras es par. Por
-- ejemplo, 
--    primosSumaPar 10 = [2,11,13,17,19]
-- ---------------------------------------------------------------------

primosSumaPar :: Int -> [Integer]
primosSumaPar n = 
   [x | x <- take n primos, even (sumaCifras x)]

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Definir la función
--    numeroPrimosSumaPar :: Int -> Int
-- tal que (numeroPrimosSumaPar n) es la cantidad de elementos del
-- conjunto de los n primeros primos tales que la suma de sus cifras es
-- par. Por ejemplo, 
--    numeroPrimosSumaPar 10 = 5
-- ---------------------------------------------------------------------

numeroPrimosSumaPar :: Int -> Int
numeroPrimosSumaPar = length . primosSumaPar

-- ---------------------------------------------------------------------
-- Ejercicio 1.5. Definir la función
--    puntos :: Int -> [(Int,Int)]
-- tal que (puntos n) es la lista de los puntos de la forma (x,y) donde x
-- toma los valores 0,10,20,...,10*n e y es la cantidad de elementos del
-- conjunto de los x primeros primos tales que la suma de sus cifras es
-- par. Por ejemplo, 
--    puntos 5 = [(0,0),(10,5),(20,10),(30,17),(40,21),(50,23)]
-- ---------------------------------------------------------------------

puntos :: Int -> [(Int,Int)]
puntos n = [(i,numeroPrimosSumaPar i) | i <- [0,10..10*n]]
