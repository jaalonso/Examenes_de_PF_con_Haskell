-- Informática (1º del Grado en Matemáticas, Grupo 2)
-- 2º examen de evaluación continua (27 de noviembre de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías                                                        --
-- ---------------------------------------------------------------------

import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Un número es un escalón en una lista si todos los que le
-- siguen en la lista son mayores que él. Por ejemplo, 3 es un escalón
-- en la lista [4,3,6,5,8] porque 6, 5 y 8 son mayores, pero 4 no es un
-- escalón ya que el 3 le sigue y no es mayor que 4.
-- 
-- Definir la función
--    escalones :: [Int] -> [Int]
-- tal que (escalones xs) es la lista de los escalones de la lista
-- xs. Por ejemplo,
--    escalones [5,3,6,5,8] == [3,5,8]
--    escalones [4,6,0]     == [0]
-- ---------------------------------------------------------------------

-- 1ª solución (por comprensión)
-- =============================

escalones1 :: [Int] -> [Int]
escalones1 xs =
  [y | (y:ys) <- init (tails xs)
     , all (>y) ys]

-- 2ª solución (por recursión)
-- ===========================

escalones2 :: [Int] -> [Int]
escalones2 [] = []
escalones2 (x:xs) | all (>x) xs = x : escalones2 xs
                  | otherwise   = escalones2 xs

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    sumaNum :: [Int] -> [Int] -> Int
-- tal (sumaNum xs ns) es la suma de los elementos de la lista xs que
-- ocupan las posiciones que se indican en la lista de números no
-- negativos ns. Por ejemplo, 
-- + sumaNum [1,2,4,3,6] [0,1,4] es 1+2+6, ya que 1, 2 y 6 son los
--   números de la lista que están en las posiciones 0,1 y 4.
-- + sumaNum [1,2,5,3,9] [4,6,2] es 9+0+5 ya que el 9 y el 5 están en
--   las posiciones 4 y 2 pero en la posicion 6 no aparece ningún
--   elemento. 
-- Ejemplos:
--    sumaNum [1,2,4,3,6] [2,4,6]  ==  10
--    sumaNum [1,2,4,3,6] [0,1,4]  ==  9
--    sumaNum [1,2,5,3,9] [4,6,2]  ==  14
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión)
sumaNum1 :: [Int] -> [Int] -> Int
sumaNum1 xs ns = sum [xs!!n | n <- ns, n < length xs]

-- 2ª definición (por recursión)
sumaNum2 :: [Int] -> [Int] -> Int
sumaNum2 _ [] = 0
sumaNum2 xs (n:ns) | n < length xs = xs!!n + sumaNum2 xs ns
                   | otherwise     = sumaNum2 xs ns

-- 3ª definición (por recursión final)
sumaNum3  :: [Int] -> [Int] -> Int
sumaNum3 xs ns = aux ns 0 
    where aux [] ac = ac
          aux (y:ys) ac | y < length xs = aux ys (ac + (xs!!y))
                        | otherwise     = aux ys ac

-- ---------------------------------------------------------------------
-- Ejercicio 3. Consideremos la sucesión construida a partir de un
-- número n, sumando los factoriales de los dígitos de n, y repitiendo
-- sobre el resultado dicha operación. Por ejemplo, si comenzamos en 69,
-- obtendremos:  
--    69
-- 363600  (porque 6! + 9! = 363600)  
--   1454  (porque 3! + 6! + 3! + 6! + 0! + 0! = 1454)
--    169  (porque 1! + 4! + 5! + 4! = 169)
-- 363601  (porque 1! + 6! + 9! = 363601)
--   1454  (porque 3! + 6! + 3! + 6! + 0! + 1! = 1454)
-- ......
-- 
-- La cadena correspondiente a un número n son los términos de la
-- sucesión que empieza en n hasta la primera repetición de un elemento
-- en la sucesión. Por ejemplo, la cadena de 69 es 
--   [69,363600,1454,169,363601]
-- ya que el siguiente número sería 1454, que ya está en la lista.
-- 
-- Definir la función cadena
--   cadena  :: Int -> [Int]
-- tal que (cadena n) es la cadena correspondiente al número n. Por
-- ejemplo, 
--
-- cadena 69    ==  [69,363600,1454,169,363601]
-- cadena 145   ==  [145]
-- cadena 78    ==  [78,45360,871,45361]
-- cadena 569   ==  [569,363720,5775,10320,11,2]
-- cadena 3888  ==  [3888,120966,364324,782,45362,872]
-- length (cadena 1479) ==  60
-- ---------------------------------------------------------------------

cadena :: Int -> [Int]
cadena x = aux x [x]
  where aux y ac | elem (f y) ac = ac
                 | otherwise     = aux (f y) (ac ++[f y])
        f x       = sum [fac x | x <- digitos x]
        digitos x = [read [y] | y <- show x]
        fac n     = product [1..n] 


