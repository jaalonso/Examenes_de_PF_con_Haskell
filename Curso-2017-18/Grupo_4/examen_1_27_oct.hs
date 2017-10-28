-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 1º examen de evaluación continua (27 de octubre de 2017)
-- ---------------------------------------------------------------------

-- Nota: La puntuación de cada ejercicio es 2.5 puntos.

-- ---------------------------------------------------------------------
-- § Librerías                                                        --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. La suma de la serie
--    1/3 + 1/15 + 1/35 + 1/63 + ...+ 1/(4*x^2-1) + ...
-- es 1/2. 
-- 
-- Definir la función 
--    sumaSerie:: Double -> Double
-- tal que (sumaSerie n) es la aproximación de 1/2 obtenida mediante n
-- términos de la serie. Por ejemplo,
--    sumaSerie 2   == 0.39999999999999997
--    sumaSerie 10  == 0.4761904761904761
--    sumaSerie 100 == 0.49751243781094495
-- ---------------------------------------------------------------------

sumaSerie :: Double -> Double
sumaSerie n = sum [1/(4*x^2-1) | x <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que la sumas finitas de la
-- serie siempre son menores que 1/2.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_SumaSerie :: Double -> Bool
prop_SumaSerie n = sumaSerie n < 0.5

-- La comprobación es
--    λ> quickCheck prop_SumaSerie
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2. La sombra de un número x es el que se obtiene borrando
-- las cifras de x que ocupan lugares impares (empezando a contar en
-- 0). Por ejemplo, la sombra de 123 es 13 ya que borrando el 2, que
-- ocupa la posición 1, se obtiene  el 13.
-- 
-- Definir la función
--    sombra :: Int -> Int
-- tal que (sombra x) es la sombra de x. Por ejemplo,
--    sombra 93245368790412345  ==  925670135
--    sombra 4736               ==  43
--    sombra 473                ==  43
--    sombra 47                 ==  4
--    sombra 4                  ==  4
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión)
-- ===============================

sombra :: Int -> Int
sombra n = read [x | (x,n) <- zip (show n) [0..], even n]

-- 2ª definición (por recursión)
-- =============================

sombra2 :: Int -> Int
sombra2 n = read (elementosEnPares (show n))

-- (elementosEnPares xs) es la lita de los elementos de xs en posiciones
-- pares. Por ejemplo,
--    elementosEnPares [4,7,3,6]  ==  [4,3]
--    elementosEnPares [4,7,3]    ==  [4,3]
--    elementosEnPares [4,7]      ==  [4]
--    elementosEnPares [4]        ==  [4]
--    elementosEnPares []         ==  []
elementosEnPares :: [a] -> [a]
elementosEnPares (x:y:zs) = x : elementosEnPares zs
elementosEnPares xs       = xs

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    cerosDelFactorial :: Integer -> Integer
-- tal que (cerosDelFactorial n) es el número de ceros en que termina el 
-- factorial de n. Por ejemplo,
--    cerosDelFactorial 24 ==  4
--    cerosDelFactorial 25 ==  6
-- ---------------------------------------------------------------------
 
-- 1ª definición
-- =============
 
cerosDelFactorial1 :: Integer -> Integer
cerosDelFactorial1 n = ceros (factorial n)
 
-- (factorial n) es el factorial n. Por ejemplo,
--    factorial 3  ==  6
factorial :: Integer -> Integer
factorial n = product [1..n]
 
-- (ceros n) es el número de ceros en los que termina el número n. Por
-- ejemplo, 
--    ceros 320000  ==  4
ceros :: Integer -> Integer
ceros n | rem n 10 /= 0 = 0
        | otherwise     = 1 + ceros (div n 10)
 
-- 2ª definición
-- =============
 
cerosDelFactorial2 :: Integer -> Integer
cerosDelFactorial2 n | n < 5     = 0
                     | otherwise = m + cerosDelFactorial2 m
  where m = n `div` 5
 
-- Comparación de la eficiencia
--    ghci> cerosDelFactorial1 (3*10^4)
--    7498
--    (3.96 secs, 1,252,876,376 bytes)
--    ghci> cerosDelFactorial2 (3*10^4)
--    7498
--    (0.03 secs, 9,198,896 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    todosDistintos :: Eq a => [a] -> Bool
-- tal que (todosDistintos xs) se verifica si todos los elementos de xs
-- son distintos. Por ejemplo,
--    todosDistintos [2,3,5,7,9]    ==  True
--    todosDistintos [2,3,5,7,9,3]  ==  False
--    todosDistintos "Betis"        ==  True
--    todosDistintos "Sevilla"      ==  False
--    ---------------------------------------------------------------------

todosDistintos :: Eq a => [a] -> Bool
todosDistintos []     = True
todosDistintos (x:xs) = x `notElem` xs && todosDistintos xs

