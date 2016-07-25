-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 3º examen de evaluación continua (15 de marzo de 2010)
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1.1. Definir, por recursión, la función
--    pares :: [Int] -> [Int]
-- tal que (pares xs) es la lista de los elementos pares de xs. Por
-- ejemplo,  
--    pares [2,5,7,4,6,8,9]    ==  [2,4,6,8]
-- ---------------------------------------------------------------------

pares   :: [Int] -> [Int]
pares [] = []
pares (x:xs) | even x    = x : pares xs
             | otherwise = pares xs

-- ---------------------------------------------------------------------
-- Ejercicio 1.1.2. Definir, por recursión, la función
--    impares :: [Int] -> [Int]
-- tal que (impares xs) es la lista de los elementos impares de xs. Por
-- ejemplo,  
--    impares [2,5,7,4,6,8,9]  ==  [5,7,9]
-- ---------------------------------------------------------------------

impares   :: [Int] -> [Int]
impares [] = []
impares (x:xs) | odd x     = x : impares xs
               | otherwise = impares xs

-- ---------------------------------------------------------------------
-- Ejercicio 1.1.3. Definir, por recursión, la función
--    suma :: [Int] -> Int
-- tal que (suma xs) es la suma de los elementos de xs. Por ejemplo, 
--    suma [2,5,7,4,6,8,9]  ==  41
-- ---------------------------------------------------------------------

suma :: [Int] -> Int
suma []     = 0
suma (x:xs) = x + suma xs

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que la suma de la suma de
-- (pares xs) y la suma de (impares xs) es igual que la suma de xs. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_pares :: [Int] -> Bool
prop_pares xs =
    suma (pares xs) + suma (impares xs) == suma xs

-- La comprobación es
--    ghci> quickCheck prop_pares
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 1.3 Demostrar por inducción que que la suma de la suma de
-- (pares xs) y la suma de (impares xs) es igual que la suma de xs. 
-- ---------------------------------------------------------------------

{-
 Demostración:
 La propiedad que hay que demostrar es
    suma (pares xs) + suma (impares xs) = suma xs
 
 Caso base: Hay que demostrar que
    suma (pares []) + suma (impares []) = suma []
 En efecto,
    suma (pares []) + suma (impares [])
    = suma [] + suma []                    [por pares.1 e impares.1]
    = 0 + 0                                [por suma.1]
    = 0                                    [por aritmética]
    = suma []                              [por suma.1]

 Paso de inducción: Se supone que la hipótesis de inducción
    suma (pares xs) + suma (impares xs) = suma xs 
 Hay que demostrar que  
    suma (pares (x:xs)) + suma (impares (x:xs)) = suma (x:xs)
 Lo demostraremos distinguiendo dos casos

 Caso 1: Supongamos que x es par. Entonces,
     suma (pares (x:xs)) + suma (impares (x:xs))   
     = suma (x:pares xs) + suma (impares xs)       [por pares.2, impares.3]
     = x + suma (pares xs) + suma (impares xs)     [por suma.2]
     = x + suma xs                                 [por hip. de inducción]
     = suma (x:xs)                                 [por suma.2]

 Caso 1: Supongamos que x es impar. Entonces,
     suma (pares (x:xs)) + suma (impares (x:xs))   
     = suma (pares xs) + suma (x:impares xs)       [por pares.3, impares.2]
     = suma (pares xs) + x + suma (impares xs)     [por suma.2]
     = x + suma xs                                 [por hip. de inducción]
     = suma (x:xs)                                 [por suma.2]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 2.1.1. Definir, por recursión, la función
--    duplica :: [a] -> [a]
-- tal que (duplica xs) es la lista obtenida duplicando los elementos de
-- xs. Por ejemplo,
--    duplica  [7,2,5]  ==  [7,7,2,2,5,5]
-- ---------------------------------------------------------------------

duplica :: [a] -> [a]
duplica [] = []
duplica (x:xs) = x:x:duplica xs

-- ---------------------------------------------------------------------
-- Ejercicio 2.1.2. Definir, por recursión, la función
--    longitud :: [a] -> Int
-- tal que (longitud xs) es el número de elementos de xs. Por ejemplo,
--    longitud [7,2,5]  ==  3
-- ---------------------------------------------------------------------

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Comprobar con QuickCheck que (longitud (duplica xs))
-- es el doble de (longitud xs), donde xs es una lista de números
-- enteros.  
-- ---------------------------------------------------------------------

-- La propiedad es
prop_duplica :: [Int] -> Bool
prop_duplica xs =
    longitud (duplica xs) == 2 * longitud xs

-- La comprobación es
--    ghci> quickCheck prop_duplica
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Demostrar por inducción que la longitud de 
-- (duplica xs) es el doble de la longitud de xs. 
-- ---------------------------------------------------------------------

{-
 Demostración: Hay que demostrar que
    longitud (duplica xs) = 2 * longitud xs
 Lo haremos por inducción en xs.

 Caso base: Hay que demostrar que
    longitud (duplica []) = 2 * longitud []
 En efecto
    longitud (duplica xs) 
    = longitud []            [por duplica.1]
    = 0                      [por longitud.1]
    = 2 * 0                  [por aritmética]
    = longitud []            [por longitud.1]

 Paso de inducción: Se supone la hipótesis de inducción
    longitud (duplica xs) = 2 * longitud xs
 Hay que demostrar que
    longitud (duplica (x:xs)) = 2 * longitud (x:xs)
 En efecto,
    longitud (duplica (x:xs))
    = longitud (x:x:duplica xs)       [por duplica.2]
    = 1 + longitud (x:duplica xs)     [por longitud.2]
    = 1 + 1 + longitud (duplica xs)   [por longitud.2]
    = 1 + 1 + 2*(longitud xs)         [por hip. de inducción]
    = 2 * (1 + longitud xs)           [por aritmética]
    = 2 * longitud (x:xs)             [por longitud.2]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función
--    listasMayores :: [[Int]] -> [[Int]]
-- tal que (listasMayores xss) es la lista de las listas de xss de mayor
-- suma. Por ejemplo,
--    ghci> listasMayores [[1,3,5],[2,7],[1,1,2],[3],[5]]
--    [[1,3,5],[2,7]]
-- ---------------------------------------------------------------------

listasMayores :: [[Int]] -> [[Int]]
listasMayores xss = [xs | xs <- xss, sum xs == m]
    where m = maximum [sum xs | xs <- xss]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con QuickCheck que todas las listas de 
-- (listasMayores xss) tienen la misma suma.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_listasMayores :: [[Int]] -> Bool
prop_listasMayores xss =
    iguales [sum xs | xs <- listasMayores xss]

-- (iguales xs) se verifica si todos los elementos de xs son
-- iguales. Por ejemplo,
--    iguales [2,2,2]  ==  True
--    iguales [2,3,2]  ==  False
iguales :: Eq a => [a] -> Bool
iguales (x1:x2:xs) = x1 == x2 && iguales (x2:xs)
iguales _          = True

-- La comprobación es
--    ghci> quickCheck prop_listasMayores
--    OK, passed 100 tests.

