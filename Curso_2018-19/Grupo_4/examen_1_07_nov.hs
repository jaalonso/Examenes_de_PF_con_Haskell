-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 1º examen de evaluación continua (7 de noviembre de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    divisoresPrimos :: Integer -> [Integer]
-- tal que (divisoresPrimos x) es la lista de los divisores primos de x. 
-- Por ejemplo, 
--    divisoresPrimos 40  ==  [2,5]
--    divisoresPrimos 70  ==  [2,5,7]
-- ------------------------------------------------------------------------

divisoresPrimos :: Integer -> [Integer]
divisoresPrimos x = [n | n <- divisores x, primo n]

-- (divisores n) es la lista de los divisores del número n. Por ejemplo,
--    divisores 30  ==  [1,2,3,5,6,10,15,30]  
divisores :: Integer -> [Integer]
divisores n = [x | x <- [1..n], n `mod` x == 0]

-- (primo n) se verifica si n es primo. Por ejemplo,
--    primo 30  == False
--    primo 31  == True  
primo :: Integer -> Bool
primo n = divisores n == [1, n]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    esPotencia :: Integer -> Integer -> Bool
-- tal que (esPotencia x a) se verifica si x es una potencia de a. Por
-- ejemplo, 
--    esPotencia 32 2  ==  True
--    esPotencia 42 2  ==  False
-- ---------------------------------------------------------------------

-- 1ª definición
esPotencia :: Integer -> Integer -> Bool
esPotencia x a = x `elem` [a^n | n <- [0..x]]

-- 2ª definición
esPotencia2 :: Integer -> Integer -> Bool
esPotencia2 x a = aux x a 0
    where aux x a b | b > x     = False
                    | otherwise = x == a ^ b || aux x a (b+1)

-- 3ª definición 
esPotencia3 :: Integer -> Integer -> Bool
esPotencia3 0 _ = False
esPotencia3 1 a = True
esPotencia3 _ 1 = False
esPotencia3 x a = rem x a == 0 && esPotencia3 (div x a) a

-- ----------------------------------------------------------------------
-- Ejercicio 3. Todo número entero positivo n se puede escribir como
-- 2^k*m, con m impar. Se dice que m es la parte impar de n. Por
-- ejemplo, la parte impar de 40 es 5 porque 40 = 5*2^3.
-- 
-- Definir la función 
--    parteImpar :: Integer -> Integer
-- tal que (parteImpar n) es la parte impar de n. Por ejemplo,
--    parteImpar 40  ==  5
-- ----------------------------------------------------------------------

parteImpar :: Integer -> Integer
parteImpar n | even n    = parteImpar (n `div` 2)
             | otherwise = n

-- ---------------------------------------------------------------------
-- Ejercicio 4. Una forma de aproximar el valor del número e es usando
-- la siguiente igualdad: 
--
--                  1        2        3        4        5
--            e = ------ + ------ + ------ + ------ + ------ + ...
--                 2*0!     2*1!     2*2!     2*3!     2*4!
--
--
-- Definir la función
--    aproximaE :: Double -> Double
-- tal que (aproximaE n) es la aproximación del número e calculada con
-- la serie anterior hasta el término n-ésimo. Por ejemplo,
--    aproximaE 10  ==  2.718281663359788
--    aproximaE 15  ==  2.718281828458612
--    aproximaE 20  ==  2.718281828459045
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

aproximaE :: Double -> Double
aproximaE n =
  sum [(i+1) / (2 * factorial i) | i <- [0..n]]
  where factorial i = product [1..i]

-- 2ª definición
-- =============

aproximaE2 :: Double -> Double
aproximaE2 0 = 1/2
aproximaE2 n = (n+1)/(2 * factorial n) + aproximaE2 (n-1)
  where factorial i = product [1..i]
  
