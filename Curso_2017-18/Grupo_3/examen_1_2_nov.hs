-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 1º examen de evaluación continua (2 de noviembre de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Consideremos las regiones del plano delimitadas por
-- las rectas x = 1, x = -1, y = 1 e y = -1. Diremos que dos regiones
-- son vecinas si comparten una frontera distinta de un punto; es decir.
-- no son vecinas dos regiones con un único punto de contacto.
--
-- En este ejercicio se pretende contar el número de regiones vecinas de
-- aquella en la que está un punto dado. Por ejemplo, el punto (0,0)
-- está en la region central, que tiene 4 regiones vecinas; el punto
-- (2,2) está en la región superior derecha, que tiene 2 regiones
-- vecinas; y el punto (2,0) está en la región media derecha, que tiene
-- 3 regiones vecinas. Para cualquier punto que se encuentre en las
-- rectas x = 1, x = -1, y = 1 e y = -1, el resultado debe ser 0.
--
-- Definir la función
--    numeroRegionesVecinas :: (Float,Float) -> Int
-- tal que (numeroRegionesVecinas (x,y)) es el número de regiones
-- vecinas de aquella en la que está contenido el punto (x,y). Por
-- ejemplo, 
--    numeroRegionesVecinas (0,0)  ==  4
--    numeroRegionesVecinas (2,2)  ==  2
--    numeroRegionesVecinas (2,0)  ==  3
--    numeroRegionesVecinas (1,0)  ==  0
-- ---------------------------------------------------------------------

numeroRegionesVecinas :: (Float,Float) -> Int
numeroRegionesVecinas (x,y)
  | x' <  1 && y' <  1 = 4
  | x' == 1 || y' == 1 = 0
  | x' >  1 && y' >  1 = 2
  | otherwise          = 3
  where x' = abs x
        y' = abs y

-- ---------------------------------------------------------------------
-- Ejercicio 2. Una secuencia de números es de signo alternado si en
-- ella se alternan los números positivos y negativos. Se pueden dar dos
-- casos de secuencias de signo alternado: 
-- + El primer término es positivo, el segundo es negativo, el tercero
--   es positivo, el cuarto es negativo, y así sucesivamente. Por
--   ejemplo, la secuencia
--      1, -1, 2, -2, 3, -3
-- + El primer término es negativo, el segundo es positivo, el tercero
--   es negativo, el cuarto es positivo, y así sucesivamente. Por
--   ejemplo, la secuencia
--      -1, 1, -2, 2, -3, 3
-- Las secuencias que tengan un 0 entre sus elementos nunca son de signo
-- alternado.
--
-- Definir la función
--    signosAlternados :: [Int] -> Bool
-- tal que (signosAlternados xs) se verifica si la secuencia de números
-- enteros xs es de signo alternado. Por ejemplo, 
--    signosAlternados [1,-1,2,-2,3,-3]  ==  True
--    signosAlternados [-1,1,-2,2,-3,3]  ==  True
--    signosAlternados [0,-1,1,-1,0]     ==  False
--    signosAlternados [1,2,-1,1,-5]     ==  False
--    signosAlternados [1,-2,1,-1,-5]    ==  False
-- ---------------------------------------------------------------------

signosAlternados :: [Int] -> Bool
signosAlternados xs =
  and [x*y < 0 | (x,y) <- zip xs (tail xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Una forma de aproximar el valor del número e es usando
-- la siguiente igualdad: 
--
--                  1        2        3        4        5
--            e = ------ + ------ + ------ + ------ + ------ + ...
--                 2*0!     2*1!     2*2!     2*3!     2*4!
--
-- Es decir, la serie cuyo término general n-ésimo es el cociente entre
-- (n+1) yel doble del factorial de n:
--
--                    n+1
--           s(n) =  ------
--                    2*n!
--
-- Definir por comprensión la función:
--    aproximaEC :: Double -> Double
-- tal que (aproximaEC n) es la aproximación del número e calculada con
-- la serie anterior hasta el término n-ésimo. Por ejemplo,
--    aproximaEC 10  ==  2.718281663359788
--    aproximaEC 15  ==  2.718281828458612
--    aproximaEC 20  ==  2.718281828459045
-- ---------------------------------------------------------------------

aproximaEC :: Double -> Double
aproximaEC n =
  sum [(i+1) / (2*product [1..i]) | i <- [0..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir por recursión la función:
--    aproximaER :: Double -> Double
-- tal que (aproximaER n) es la aproximación del número e calculada con
-- la serie anterior hasta el término n-ésimo. Por ejemplo,
--    aproximaER 10  ==  2.718281663359788
--    aproximaER 15  ==  2.718281828458612
--    aproximaER 20  ==  2.718281828459045
-- ---------------------------------------------------------------------

aproximaER :: Double -> Double
aproximaER 0 = 1/2
aproximaER n =
  (n+1)/(2*product [1..n]) + aproximaER (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definición por recursión la función:
--    restaCifrasDe2en2 :: Integer -> Integer
-- tal que (restaCifrasDe2en2 n) es el número obtenido a partir del
-- número n, considerando sus cifras de 2 en 2 y tomando el valor
-- absoluto de sus diferencias. Por ejemplo
--    restaCifrasDe2en2 3       ==  3
--    restaCifrasDe2en2 83      ==  5
--    restaCifrasDe2en2 283     ==  25
--    restaCifrasDe2en2 5283    ==  35
--    restaCifrasDe2en2 2538    ==  35
--    restaCifrasDe2en2 102583  ==  135
-- ---------------------------------------------------------------------

restaCifrasDe2en2 :: Integer -> Integer
restaCifrasDe2en2 n
  | n < 10    = n
  | otherwise = 10 * restaCifrasDe2en2 (n `div` 100) + abs (x-y)
  where x = n `mod` 10
        y = (n `mod` 100) `div` 10

