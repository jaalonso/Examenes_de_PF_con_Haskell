-- Informática (1º del Grado en Matemáticas, Grupo 2)
-- 1º examen de evaluación continua (9 de noviembre de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías                                                        --
-- ---------------------------------------------------------------------

import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Dados tres números reales estrictamente positivos y
-- distintos, r, s y t, construimos una diana con tres círculos
-- concéntricos de centro el origen de coordenadas. Al disparar una
-- flecha a la diana, obtenemos 5 puntos si acertamos en el interior
-- (estricto, sin contar el borde) del círculo más pequeño; 3 puntos si
-- acertamos en el interior (estricto, sin contar el borde) del círculo
-- de tamaño medio, pero fuera  del círculo más pequeño; y 1 punto si
-- acertamos en el interior (estricto, sin contar el borde) del círculo
-- más grande, pero fuera del círculo de tamaño medio. En cualquier otro
-- caso obtenemos 0 puntos. 
-- 
-- Definir la función
--   puntosDisparoDiana :: Double -> Double -> Double -> (Double,Double)
--                         -> Int
-- tal que (puntosDisparoDiana r s t (a,b)) es la puntuación que
-- obtendríamos al acertar con una flecha en el punto de coordenadas
-- (a,b) en la diana construida con los números reales estrictamente
-- positivos y distintos, r, s y t. Por ejemplo:
--   puntosDisparoDiana 1 3 7 (0,0)    ==  5
--   puntosDisparoDiana 3 1 7 (-2,-2)  ==  3
--   puntosDisparoDiana 7 1 3 (-4,4)   ==  1
--   puntosDisparoDiana 1 7 3 (5,-5)   ==  0
--   puntosDisparoDiana 3 7 1 (1,0)    ==  3
--   puntosDisparoDiana 7 3 1 (0,-3)   ==  1
-- ----------------------------------------------------------------------------

-- 1ª solución
puntosDisparoDiana :: Double -> Double -> Double -> (Double,Double) -> Int
puntosDisparoDiana r s t (a,b)
  | d < x^2   = 5
  | d < y^2   = 3
  | d < z^2   = 1
  | otherwise = 0
  where x = minimum [r,s,t]
        z = maximum [r,s,t]
        y = (r+s+t)-(x+z)
        d = a^2+b^2

-- 2ª solución:
puntosDisparoDiana2 :: Double -> Double -> Double -> (Double,Double) -> Int
puntosDisparoDiana2 r s t (a,b)
  | d < x^2   = 5
  | d < y^2   = 3
  | d < z^2   = 1
  | otherwise = 0
  where [x,y,z] = sort [r,s,t]
        d       = a^2+b^2

-- ---------------------------------------------------------------------
-- Ejercicio 2. Decimos que una lista es posicionalmente divisible si
-- todos sus elementos son divisibles por el número de la posición que 
-- ocupan (contando desde 1). Por ejemplo, la lista [2,10,9] es
-- posicionalmente divisible pues 2 es divisible por 1, 10 es divisible
-- por 2 y 9 es divisible  por 3. Por otro lado, la lista [1,3,5] no es
-- posicionalmente divisible pues aunque 1 es divisible por 1, 3 no es
-- divisible por 2. 
-- 
-- Definir la función 
--   listaPosDivisible :: [Integer] -> Bool
-- tal que (listaPosDivisible xs) se verifica si xs es una lista
-- posicionalmente divisible. Por ejemplo:
--   listaPosDivisible [1,2,3,4]    ==  True
--   listaPosDivisible [5,8,15,20]  ==  True
--   listaPosDivisible [2,10,9,8]   ==  True
--   listaPosDivisible [1,3,5]      ==  False
--   listaPosDivisible [1,6,6,6]    ==  False
--   listaPosDivisible []           ==  True
-- ----------------------------------------------------------------------------

listaPosDivisible :: [Integer] -> Bool
listaPosDivisible xs =
  and [mod x i == 0 | (x,i) <- zip xs [1..]]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Una forma de aproximar el valor del número e es usando
-- la siguiente igualdad: 
--
--           1         1      1      1      1      1
--          --- = 1 - ---- + ---- - ---- + ---- - ---- ...
--           e         1!     2!     3!     4!     5!
--
-- Es decir, la serie cuyo término general n-ésimo es:
--
--                    (-1)^n
--           s(n) =  --------
--                       n!
--
-- Definir la función
--    aproximaE :: Double -> Double
-- tal que (aproximaE n) es la aproximación del número e calculada con la
-- serie anterior hasta el término n-ésimo. Por ejemplo,
--    aproximaE  0  ==  1.0
--    aproximaE  1  ==  Infinity
--    aproximaE  2  ==  2.0
--    aproximaE  3  ==  2.9999999999999996
--    aproximaE  4  ==  2.666666666666666
--    aproximaE  5  ==  2.727272727272727
--    aproximaE 10  ==  2.718281657666403
--    aproximaE 15  ==  2.718281828459377
--    aproximaE 20  ==  2.718281828459044
-- ----------------------------------------------------------------------------

-- 1ª definición:
aproximaE :: Double -> Double
aproximaE n =
  1 / sum [(-1)**i / product [1..i] | i <- [0..n] ]

-- 2ª definición
aproximaE2 :: Double -> Double
aproximaE2 n = 1 / aux n
  where aux :: Double -> Double
        aux 0 = 1
        aux n = (-1)**n / product [1..n] + aux (n-1)
   
-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    numeroCambiosParidad :: Integer -> Integer
-- tal que (numeroCambiosParidad n) es el número de veces que aparecen
-- en el número positivo n dos cifras consecutivas de paridad diferente
-- (una par y otra impar). Por ejemplo,
--    numeroCambiosParidad 1357  ==  0
--    numeroCambiosParidad 2468  ==  0
--    numeroCambiosParidad 1346  ==  1
--    numeroCambiosParidad 1234  ==  3
--    numeroCambiosParidad 12    ==  1
--    numeroCambiosParidad 5     ==  0
-- ----------------------------------------------------------------------------

-- 1ª solución
-- ===========

numeroCambiosParidad :: Integer -> Integer
numeroCambiosParidad n
  | n < 10      = 0
  | odd (n1+n2) = 1 + numeroCambiosParidad (sinUltimoDigito n)
  | otherwise   = numeroCambiosParidad (sinUltimoDigito n)
  where n1 = ultimoDigito n
        n2 = penultimoDigito n

-- (ultimoDigito n) es el último dígito del número n. Por ejemplo.
--    ultimoDigito 2018  ==  8
ultimoDigito :: Integer -> Integer
ultimoDigito n = n `mod` 10

-- (penultimoDigito n) es el penúltimo dígito del número n. Por ejemplo.
--    penultimoDigito 2018  ==  1
penultimoDigito :: Integer -> Integer
penultimoDigito n = (n `div` 10) `mod` 10

-- (sinUltimoDigito n) es el número n sin el último dígito. Por ejemplo,
--    sinUltimoDigito 2018  ==  201
sinUltimoDigito :: Integer -> Integer
sinUltimoDigito n = (n `div` 10)

-- 2ª solución
-- ===========

numeroCambiosParidad2 :: Integer -> Integer
numeroCambiosParidad2 n =
  numeroCambiosParidadLista (digitos n)

-- (digitos n) es la lista de los dígitos de n. Por ejemplo,
digitos ::Integer -> [Integer]
digitos n = [read [c] | c <- show n]

-- (numeroCambiosParidadLista ns) es el número de veces que aparecen
-- en la lista ns n dos elementos consecutivos de paridad diferente
-- (una par y otra impar). Por ejemplo,
--    numeroCambiosParidadLista [1,3,4,6]  ==  1
--    numeroCambiosParidadLista [1,2,3,4]  ==  3
numeroCambiosParidadLista :: [Integer] -> Integer
numeroCambiosParidadLista (x:y:zs)
  | odd (x+y) = 1 + numeroCambiosParidadLista (y:zs)
  | otherwise = numeroCambiosParidadLista (y:zs)
numeroCambiosParidadLista _ = 0
  
-- 3ª solución
-- ===========

numeroCambiosParidad3 :: Integer -> Integer
numeroCambiosParidad3 n =
  numeroCambiosParidadLista2 (digitos n)

-- (numeroCambiosParidadLista2 ns) es el número de veces que aparecen
-- en la lista ns n dos elementos consecutivos de paridad diferente
-- (una par y otra impar). Por ejemplo,
--    numeroCambiosParidadLista2 [1,3,4,6]  ==  1
--    numeroCambiosParidadLista2 [1,2,3,4]  ==  3
numeroCambiosParidadLista2 :: [Integer] -> Integer
numeroCambiosParidadLista2 ns =
  sum [1 | (x,y) <- zip ns (tail ns)
         , odd (x+y)]
