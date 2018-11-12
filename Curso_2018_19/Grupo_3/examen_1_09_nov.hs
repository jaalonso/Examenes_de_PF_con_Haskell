-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 1º examen de evaluación continua (2 de noviembre de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías                                                        --
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. La suma de la serie geométrica de razón r cuyo primer
-- elemento es y es
--     1 + r + r² + r³ + ...

-- Definir la función 
--    sumaSG :: Double -> Double -> Double
-- tal que (sumaSG r n) es la suma de los n+1 primeros términos de dicha
-- serie. Por ejemplo,
--    sumaSG 2 0        ==  1.0
--    sumaSG 2 1        ==  3.0
--    sumaSG 2 2        ==  7.0
--    sumaSG (1/2) 100  ==  2.0
--    sumaSG (1/3) 100  ==  1.5
--    sumaSG (1/4) 100  ==  1.3333333333333333
--
-- Indicación: usar la función (**) para la potencia.
-- ---------------------------------------------------------------------

sumaSG :: Double -> Double -> Double
sumaSG r n = sum [r**k | k <- [0..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. La serie converge cuando |r| < 1 y su límite es
-- 1/(1-r).
-- 
-- Definir la función 
--    errorSG :: Double -> Double -> Double
-- tal que (errorSG r x) es el menor número de términos de la serie
-- anterior necesarios para obtener su límite con un error menor que
-- x. Por ejemplo, 
--    errorSG (1/2) 0.001 == 10.0
--    errorSG (1/4) 0.001 == 5.0
--    errorSG (1/8) 0.001 == 3.0
--    errorSG (1/8) 1e-6  == 6.0
-- ---------------------------------------------------------------------

errorSG :: Double -> Double -> Double
errorSG r x = head [m | m <- [0..]
                      , abs (sumaSG r m - y) < x]
  where y = 1/(1-r)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función 
--    todosDiferentes :: Eq a => [a] -> Bool
-- tal que (todosDiferentes xs) se verifica si todos los elementos de la
-- lista xs son diferentes. Por ejemplo,
--    todosDiferentes [1..20]         == True
--    todosDiferentes (7:[1..20])     == False
--    todosDiferentes "Buenas"        == True
--    todosDiferentes "Buenas tardes" == False
-- ---------------------------------------------------------------------

-- 1ª definición:
todosDiferentes :: Eq a => [a] -> Bool
todosDiferentes (x:xs) = x `notElem` xs && todosDiferentes xs
todosDiferentes _      = True

-- 2ª definición:
todosDiferentes2 :: Eq a => [a] -> Bool
todosDiferentes2 xs = nub xs == xs

-- Equivalencia:
prop_todosDiferentes :: [Int] -> Bool
prop_todosDiferentes xs =
  todosDiferentes xs == todosDiferentes2 xs

-- Comprobación:
--    λ> quickCheck prop_todosDiferentes
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3. Una lista se denomina "cuadrada" si se puede obtener
-- concatenando dos copias de una misma lista. Por ejemplo, "abab" y
-- "aa" son listas cuadradas pero "aaa" y "abba" no lo son.
-- 
-- Definir la función
--    esCuadrada :: (Eq a, Ord a) => [a] -> Bool
-- tal que (esCuadrada xs) se verifica si xs es una lista cuadrada.
-- Por ejemplo,
--    esCuadrada "aa"   == True
--    esCuadrada "aaa"  == False
--    esCuadrada "abab" == True
--    esCuadrada "abba" == False
-- ---------------------------------------------------------------------

esCuadrada :: (Eq a, Ord a) => [a] -> Bool
esCuadrada xs | odd m      = False
              | otherwise  = as == bs
  where m = length xs
        n = m `div` 2
        (as, bs) = splitAt n xs

-- ---------------------------------------------------------------------
-- Ejercicio 4. Un niño quiere subir saltando una escalera. Con cada
-- salto que da puede subir 1, 2 o 3 peldaños. Por ejemplo, si la
-- escalera tiene 3 peldaños la puede subir de 4 formas distintas:
--    1, 1, 1
--    1, 2
--    2, 1
--    3
-- 
-- Definir la función
--    numeroFormas :: Int -> Int
-- tal que (numeroFormas n) es el número de formas en que puede subir
-- una escalera de n peldaños. Por ejemplo,
--    numeroFormas 3  == 4
--    numeroFormas 4  == 7
--    numeroFormas 10 == 274
-- ---------------------------------------------------------------------

numeroFormas :: Int -> Int
numeroFormas 1 = 1
numeroFormas 2 = 2
numeroFormas 3 = 4
numeroFormas n =
  numeroFormas (n-1) + numeroFormas (n-2) + numeroFormas (n-3)
