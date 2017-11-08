-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 1º examen de evaluación continua (2 de noviembre de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías                                                        --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. La suma de la serie 
--    1/1 - 1/2 + 1/3 - 1/4 + ...+ (-1)^n/(n+1) + ...
-- es log 2.
-- 
-- Definir la función 
--    sumaSL :: Double -> Double
-- tal que (sumaSL n) es la aproximación de (log 2) obtenida mediante n
-- términos de la serie. Por ejemplo,
--    sumaSL 2   == 0.8333333333333333
--    sumaSL 10  == 0.7365440115440116
--    sumaSL 100 == 0.6931471805599453
--
-- Indicaciones:
-- + en Haskell (log x) es el logaritmo neperiano de x; por ejemplo,
--      log (exp 1) == 1.0
-- + usar la función (**) para la potencia.
-- ---------------------------------------------------------------------

sumaSL :: Double -> Double
sumaSL n = sum [(-1)**k/(k+1) | k <- [0..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función 
--    errorSL :: Double -> Double
-- tal que (errorSL x) es el menor número de términos de la serie
-- anterior necesarios para obtener su límite con un error menor que
-- x. Por ejemplo, 
--    errorSL 0.1    ==  4.0
--    errorSL 0.01   ==  49.0
--    errorSL 0.001  ==  499.0
-- ---------------------------------------------------------------------

errorSL :: Double -> Double
errorSL x = head [m | m <- [1..]
                    , abs (sumaSL m - log 2) < x]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Se define la raizS de un número natural como sigue. Dado
-- un número natural N, sumamos todos sus dígitos, y repetimos este
-- procedimiento hasta que quede un solo dígito al cual llamamos raizS
-- de N. Por ejemplo para 9327: 3+2+7 = 21 y 2+1 = 3. Por lo tanto, la
-- raizS de 9327 es 3.

-- Definir la función
--    raizS :: Integer -> Integer
-- tal que (raizS n) es la raizS de n. Por ejemplo.
--   raizS 9327                 == 3
--   raizS 932778214521         == 6
--   raizS 93277821452189123561 == 5
-- ---------------------------------------------------------------------

raizS :: Integer -> Integer
raizS n | n < 10    = n
        | otherwise = raizS (sum (digitos n))

digitos :: Integer -> [Integer]
digitos n = [read [x] | x <- show n]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función 
--    alterna :: [a] -> [a] -> [a]
-- tal que (alterna xs ys) es la lista obtenida intercalando los
-- elementos de xs e ys. Por ejemplo,
--   alterna [5,1] [2,7,4,9]     == [5,2,1,7,4,9]
--   alterna [5,1,7] [2..10]     == [5,2,1,3,7,4,5,6,7,8,9,10]
--   alterna [2..10] [5,1,7]     == [2,5,3,1,4,7,5,6,7,8,9,10]
--   alterna [2,4..12] [1,5..30] == [2,1,4,5,6,9,8,13,10,17,12,21,25,29]
-- ---------------------------------------------------------------------

alterna :: [a] -> [a] -> [a]
alterna [] ys         = ys
alterna xs []         = xs
alterna (x:xs) (y:ys) = x:y:alterna xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con QuickCheck que el número de elementos de 
-- (alterna xs ys) es la suma de los números de elementos de xs e ys.
-- ---------------------------------------------------------------------

propAlterna :: [a] -> [a] -> Bool
propAlterna xs ys = length (alterna xs ys) == length xs + length ys

-- La comprobación es
--    ghci> quickCheck propAlterna
--    +++ OK, passed 100 tests.  

-- ---------------------------------------------------------------------
-- Ejercicio 4. La sucesión de los números triangulares se obtiene
-- sumando los números naturales. Así, el 7º número triangular es
--    1 + 2 + 3 + 4 + 5 + 6 + 7 = 28.
-- 
-- Los primeros 10 números triangulares son
--    1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
-- 
-- Los divisores de los primeros 7 números triangulares son:
--     1: 1
--     3: 1,3
--     6: 1,2,3,6
--    10: 1,2,5,10
--    15: 1,3,5,15
--    21: 1,3,7,21
--    28: 1,2,4,7,14,28
--
-- Como se puede observar, 28 es el menor número triangular con más de 5
-- divisores.
-- 
-- Definir la función
--    menorTND :: Int -> Integer
-- tal que (menorTND n) es el menor número triangular que tiene al menos
-- n divisores. Por ejemplo,
--    menorTND 5  == 28
--    menorTND 10 == 120
--    menorTND 50 == 25200
-- ---------------------------------------------------------------------

menorTND :: Int -> Integer
menorTND x = head $ filter ((> x) . numeroDiv) triangulares

triangulares :: [Integer]
triangulares = [(n*(n+1)) `div` 2 | n <- [1..]]

numeroDiv :: Integer -> Int
numeroDiv n = 2 + length [x | x <- [2..n `div` 2], rem n x == 0]
