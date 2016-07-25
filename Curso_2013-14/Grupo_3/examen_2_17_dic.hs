-- Informática (1º del Grado en Matemáticas)
-- 2º examen de evaluación continua (17 de diciembre de 2013)
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List (sort)

-- ---------------------------------------------------------------------
-- Ejercicio 1. [2.5 puntos] Definir la función
--    expandida :: [Int] -> [Int]
-- tal que (expandida xs) es la lista obtenida duplicando cada uno de
-- los elementos pares de xs. Por ejemplo,
--    expandida [3,5,4,6,6,1,0]  ==  [3,5,4,4,6,6,6,6,1,0,0]
--    expandida [3,5,4,6,8,1,0]  ==  [3,5,4,4,6,6,8,8,1,0,0]
-- ---------------------------------------------------------------------

expandida :: [Int] -> [Int]
expandida [] = []
expandida (x:xs) | even x    = x : x : expandida xs
                 | otherwise = x : expandida xs

-- ---------------------------------------------------------------------
-- Ejercicio 2. [2.5 puntos] Comprobar con QuickCheck que el número de
-- elementos de (expandida xs) es el del número de elementos de xs más
-- el número de elementos pares de xs.
-- ---------------------------------------------------------------------

prop_expandida :: [Int] -> Bool
prop_expandida xs =
    length (expandida xs) == length xs + length (filter even xs) 

-- La comprobación es
--    ghci> quickCheck prop_expandida
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3. [2.5 puntos] Definir la función
--    digitosOrdenados :: Integer -> Integer
-- tal que (digitosOrdenados n) es el número obtenido ordenando los
-- dígitos de n de mayor a menor. Por ejemplo,
--    digitosOrdenados 325724237  ==  775433222
-- ---------------------------------------------------------------------

digitosOrdenados :: Integer -> Integer
digitosOrdenados n = read (ordenados (show n))

ordenados :: Ord a => [a] -> [a]
ordenados [] = []
ordenados (x:xs) = 
    ordenados mayores ++ [x] ++ ordenados menores
    where mayores = [y | y <- xs, y > x]
          menores = [y | y <- xs, y <= x]

-- Nota: La función digitosOrdenados puede definirse por composición
digitosOrdenados2 :: Integer -> Integer
digitosOrdenados2 = read . ordenados . show

-- Nota: La función digitosOrdenados puede definirse por composición y
-- también usando sort en lugar de ordenados
digitosOrdenados3 :: Integer -> Integer
digitosOrdenados3 = read . reverse . sort . show

-- ---------------------------------------------------------------------
-- Ejercicio 4. [2.5 puntos] Sea f la siguiente función, aplicable a
-- cualquier número entero positivo: 
-- * Si el número es par, se divide entre 2.
-- * Si el número es impar, se multiplica por 3 y se suma 1.
-- 
-- La carrera de Collatz consiste en, dada una lista de números ns,
-- sustituir cada número n de ns por f(n) hasta que alguno sea igual a
-- 1. Por ejemplo, la siguiente sucesión es una carrera de Collatz
--    [ 3, 6,20, 49, 73]
--    [10, 3,10,148,220]
--    [ 5,10, 5, 74,110]
--    [16, 5,16, 37, 55]
--    [ 8,16, 8,112,166]
--    [ 4, 8, 4, 56, 83]
--    [ 2, 4, 2, 28,250]
--    [ 1, 2, 1, 14,125]
-- En esta carrera, los ganadores son 3 y 20.
-- 
-- Definir la función 
--    ganadores :: [Int] -> [Int]
--    ganadores [3,6,20,49,73]  ==  [3,20]
-- ---------------------------------------------------------------------

ganadores :: [Int] -> [Int]
ganadores xs = selecciona xs (final xs)

-- (final xs) es el estado final de la carrera de Collatz a partir de
-- xs. Por ejemplo,
--    final [3,6,20,49,73]  ==  [1,2,1,14,125]
final :: [Int] -> [Int]
final xs | elem 1 xs = xs
         | otherwise = final [siguiente x | x <- xs]

-- (siguiente x) es el siguiente de x en la carrera de Collatz. Por
-- ejemplo, 
--    siguiente 3  ==  10
--    siguiente 6  ==  3
siguiente :: Int -> Int
siguiente x | even x    = x `div` 2
            | otherwise = 3*x+1

-- (selecciona xs ys) es la lista de los elementos de xs cuyos tales que
-- los elementos de ys en la misma posición son iguales a 1. Por ejemplo,
--    selecciona [3,6,20,49,73] [1,2,1,14,125]  ==  [3,20]
selecciona :: [Int] -> [Int] -> [Int]
selecciona xs ys =
    [x | (x,y) <- zip xs ys, y == 1]
