-- Informática (1º del Grado en Matemáticas)
-- 2º examen de evaluación continua (30 de noviembre de 2016)
-- =====================================================================

-- ---------------------------------------------------------------------
-- Librerías auxiliares
-- ---------------------------------------------------------------------

import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Dada una lista de números ns, su lista de degradación es
-- la lista que se obtiene contando para cada elemento de la lista
-- original n, el número de elementos consecutivos en la lista que son 
-- estrictamente menores que n. Por ejemplo, la lista de degradación de
-- [5,3,1,7,6,2,8] es [2,1,0,2,1,0,0] pues:
-- + Al 5 le siguen 2 elementos consecutivos estrictamente menores (3 y 1)
-- + Al 3 le sigue 1 elemento consecutivo estrictamente menor (1)
-- + Al 1 no le sigue ningún elemento estrictamente menor
-- + Al 7 le siguen 2 elementos consecutivos estrictamente menores (6 y 2)
-- + Al 6 le sigue 1 elemento consecutivo estrictamente menor (2)
-- + Al 2 no le sigue ningún elemento estrictamente menor
-- + Al 8 no le sigue ningún elemento.
--
-- Definir la función 
--   listaDegradacion :: [Int] -> [Int]
-- tal que (listaDegradacion ns) es la lista de degradación de la lista
-- ns. Por ejemplo,
--   listaDegradacion [5,3,1,7,6,2,8]  ==  [2,1,0,2,1,0,0]
--   listaDegradacion [1,2,3,4,5]      ==  [0,0,0,0,0]
--   listaDegradacion [5,4,3,2,1]      ==  [4,3,2,1,0]
--   listaDegradacion [9,7,1,4,8,4,0]  ==  [6,2,0,0,2,1,0]
-- ---------------------------------------------------------------------

listaDegradacion :: [Int] -> [Int]
listaDegradacion xs =
  [length (takeWhile (<y) ys) | (y:ys) <- init (tails xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Una lista de números se puede describir indicando
-- cuantas veces se repite cada elemento. Por ejemplo la lista
-- [1,1,1,3,3,2,2] se puede describir indicando que hay 3 unos, 2 treses
-- y 2 doses. De esta forma, la descripción de una lista es otra lista
-- en la que se indica qué elementos hay en la primera y cuántas veces
-- se repiten. Por ejemplo, la descripción de la lista [1,1,1,3,3,2,2]
-- es [3,1,2,3,2,2]. Ocasionalmente, la descripción de una lista es más
-- corta que la propia lista. 
--
-- Se considera la función 
--   originalDescripcion :: [Int] -> [Int]
-- tal que (originalDescripcion xs) es la lista ys tal que la descripción
-- de ys es la lista xs. Es decir, la lista xs indica qué elementos hay
-- en ys y cuántas veces se repiten. Por ejemplo,
--   originalDescripcion [3,1,2,3,2,2]  ==  [1,1,1,3,3,2,2]
--   originalDescripcion [1,1,3,2,2,3]  ==  [1,2,2,2,3,3]
--   originalDescripcion [2,1,3,3,3,1]  ==  [1,1,3,3,3,1,1,1]
-- ---------------------------------------------------------------------

originalDescripcion :: [Int] -> [Int]
originalDescripcion (n:x:xs) =
  replicate n x ++ originalDescripcion xs
originalDescripcion _ = []

-- ---------------------------------------------------------------------
-- Ejercicio 3. Se dice que un elemento x de una lista xs respeta la
-- ordenación si x es mayor o igual que todos lo que tiene delante en xs
-- y es menor o igual que todos lo que tiene detrás en xs. Por ejemplo,
-- en la lista lista [3,2,1,4,6,5,7,9,8] el número 4 respeta la
-- ordenación pero el número 5 no la respeta (porque es mayor que el 6
-- que está delante).
--
-- Definir la función
--    respetuosos :: Ord a => [a] -> [a]
-- tal que (respetuosos xs) es la lista de los elementos de xs que
-- respetan la ordenación. Por ejemplo,
--    respetuosos [3,2,1,4,6,4,7,9,8]  ==  [4,7]
--    respetuosos [2,1,3,4,6,4,7,8,9]  ==  [3,4,7,8,9]
--    respetuosos "abaco"              ==  "aco"
--    respetuosos "amor"               ==  "amor"
--    respetuosos "romanos"            ==  "s"
--    respetuosos [1..9]               ==  [1,2,3,4,5,6,7,8,9]
--    respetuosos [9,8..1]             ==  []
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
respetuosos :: Ord a => [a] -> [a]
respetuosos xs =
  [z | k <- [0..n-1]
     , let (ys,z:zs) = splitAt k xs
     , all (<=z) ys
     , all (>=z) zs]
  where n = length xs

-- 2ª definición (por recursión):
respetuosos2 :: Ord a => [a] -> [a]
respetuosos2 = aux [] [] 
  where aux zs _  []      = reverse zs
        aux zs ys (x:xs)
          | all (<=x) ys && all (>=x) xs = aux (x:zs) (x:ys) xs
          | otherwise                    = aux zs     (x:ys) xs

-- 2ª definición
respetuosos3 :: Ord a => [a] -> [a]
respetuosos3 xs = [ x | (ys,x,zs) <- zip3 (inits xs) xs (tails xs)
                      , all (<=x) ys
                      , all (x<=) zs ]

-- 4ª solución
respetuosos4 :: Ord a =>[a] ->[a]
respetuosos4 xs =
  [x | (a, x, b) <- zip3 (scanl1 max xs) xs (scanr1 min xs)
     , a <= x && x <= b]

-- Comparación de eficiencia
--    ghci> length (respetuosos [1..3000])
--    3000
--    (3.31 secs, 1,140,407,224 bytes)
--    ghci> length (respetuosos2 [1..3000])
--    3000
--    (2.85 secs, 587,082,160 bytes)
--    ghci> length (respetuosos3 [1..3000])
--    3000
--    (2.12 secs, 785,446,880 bytes)
--    ghci> length (respetuosos4 [1..3000])
--    3000
--    (0.02 secs, 0 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Un número equilibrado es aquel en el que la suma de
-- los dígitos que ocupan una posición par es igual a la suma de los
-- dígitos que ocupan una posición impar.
-- 
-- Definir la constante
--    equilibrados :: [Int]
-- cuyo valor es la lista infinita de todos los números equilibrados. Por
-- ejemplo,
--    take 13 equilibrados  ==  [0,11,22,33,44,55,66,77,88,99,110,121,132]
--    equilibrados!!1000    ==  15345
--    equilibrados!!2000    ==  31141
--    equilibrados!!3000    ==  48686
-- ---------------------------------------------------------------------

equilibrados :: [Int]
equilibrados =
  filter equilibrado [0..]

equilibrado :: Int -> Bool
equilibrado n =
  sum (zipWith (*) (digitos n) (cycle [1,-1])) == 0

digitos :: Int -> [Int]
digitos n =
  [read [c] | c <- show n]

