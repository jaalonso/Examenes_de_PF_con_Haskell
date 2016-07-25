-- Informática: 3º examen de evaluación continua (23 de enero de 2014)
-- ---------------------------------------------------------------------

-- Puntuación: Cada uno de los 5 ejercicios vale 2 puntos.

import Data.List 

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    sumaSinMuliplos :: Int -> [Int] -> Int
-- tal que (sumaSinMuliplos n xs) es la suma de los números menores o
-- iguales a n que no son múltiplos de ninguno de xs. Por ejemplo,
--    sumaSinMuliplos 10 [2,5]    ==  20
--    sumaSinMuliplos 10 [2,5,6]  ==  20
--    sumaSinMuliplos 10 [2,5,3]  ==  8
-- ---------------------------------------------------------------------

sumaSinMuliplos :: Int -> [Int] -> Int
sumaSinMuliplos n xs = sum [x | x <- [1..n], sinMultiplo x xs]

-- 1ª definición
sinMultiplo :: Int -> [Int] -> Bool
sinMultiplo n xs = all (\x -> n `mod` x /= 0) xs

-- 2ª definición (por comprensión):
sinMultiplo2 :: Int -> [Int] -> Bool
sinMultiplo2 n xs = and [n `mod` x /= 0 | x <- xs]

-- 3ª definición (por recursión):
sinMultiplo3 :: Int -> [Int] -> Bool
sinMultiplo3 n []     = True
sinMultiplo3 n (x:xs) = n `mod` x /= 0 && sinMultiplo3 n xs

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    menorFactorial :: (Int -> Bool) -> Int
-- tal que (menorFactorial p) es el menor n tal que n! cumple la
-- propiedad p. Por ejemplo,
--    menorFactorialP (>5)                     ==  3
--    menorFactorialP (\x -> x `mod` 21 == 0)  ==  7
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

menorFactorial :: (Int -> Bool) -> Int
menorFactorial p = head [n | (n,m) <- zip [0..] factoriales, p m]
    where factoriales = scanl (*) 1 [1..]

-- 2ª solución
-- ===========

menorFactorial2 :: (Int -> Bool) -> Int
menorFactorial2 p = 1 + length (takeWhile (not . p) factoriales)

factoriales :: [Int]
factoriales = [factorial n | n <- [1..]]

factorial :: Int -> Int
factorial n = product [1..n]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Las expresiones aritméticas se pueden representar como
-- árboles con números en las hojas y operaciones en los nodos. Por
-- ejemplo, la expresión "9-2*4" se puede representar por el árbol
--      - 
--     / \
--    9   *
--       / \
--      2   4
-- 
-- Definiendo el tipo de dato Arbol por 
--    data Arbol = H Int | N (Int -> Int -> Int) Arbol Arbol
-- la representación del árbol anterior es
--    N (-) (H 9) (N (*) (H 2) (H 4))
--
-- Definir la función
--    valor :: Arbol -> Int
-- tal que (valor a) es el valor de la expresión aritmética
-- correspondiente al árbol a. Por ejemplo,
--    valor (N (-) (H 9) (N (*) (H 2) (H 4)))    ==  1
--    valor (N (+) (H 9) (N (*) (H 2) (H 4)))    ==  17
--    valor (N (+) (H 9) (N (div) (H 4) (H 2)))  ==  11
--    valor (N (+) (H 9) (N (max) (H 4) (H 2)))  ==  13
-- ---------------------------------------------------------------------

data Arbol = H Int | N (Int -> Int -> Int) Arbol Arbol

valor :: Arbol -> Int
valor (H x)     = x
valor (N f i d) = f (valor i) (valor d)

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Se dice que el elemento y es un superior de x en una
-- lista xs si y > x y la posición de y es mayor que la de x en xs. Por
-- ejemplo, los superiores de 5 en [7,3,5,2,8,5,6,9,1] son el 8, el 6 y
-- el 9. El número de superiores de cada uno de sus elementos se
-- representa en la siguiente tabla
--    elementos:             [ 7, 3, 5, 2, 8, 5, 6, 9, 1]
--    número de superiores:    2  5  3  4  1  2  1  0  0
-- El elemento con máximo número de superiores es el 3 que tiene 5
-- superiores. 
--
-- Definir la función
--    maximoNumeroSup :: Ord a => [a] -> Int
-- tal que (maximoNumeroSup xs) es el máximo de los números de
-- superiores de los elementos de xs. Por ejemplo,
--   maximoNumeroSup [7,3,5,2,8,4,6,9,1]  ==  5
--   maximoNumeroSup "manifestacion"      ==  10
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

maximoNumeroSup :: Ord a => [a] -> Int
maximoNumeroSup [] = 0
maximoNumeroSup xs = maximum [length (filter (z<) zs) | z:zs <- tails xs]

-- 2ª solución
-- ===========

maximoNumeroSup2 :: Ord a => [a] -> Int
maximoNumeroSup2 = maximum . numeroSup

-- (numeroSup xs) es la lista de los números de superiores de cada
-- elemento de xs. Por ejemplo,
--    numeroSup [7,3,5,2,8,5,6,9,1] ==  [2,5,3,4,1,2,1,0,0]
numeroSup :: Ord a => [a] -> [Int]
numeroSup []     = []
numeroSup [_]    = [0]
numeroSup (x:xs) = length (filter (>x) xs) : numeroSup xs

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Comprobar con QuickCheck que (maximoNumeroSup xs) es
-- igual a cero si, y sólo si, xs está ordenada de forma no decreciente.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_maximoNumeroSup :: [Int] ->  Bool
prop_maximoNumeroSup xs = (maximoNumeroSup xs == 0) == ordenada xs
    where ordenada xs = and (zipWith (>=) xs (tail xs))

-- La comprobación es 
--    ghci> quickCheck prop_maximoNumeroSup
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 5. En la siguiente figura, al rotar girando 90º en el
-- sentido del reloj la matriz de la izquierda se obtiene la de la
-- derecha 
--    1 2 3        7 4 1
--    4 5 6        8 5 2
--    7 8 9        9 6 3
-- 
-- Definir la función
--    rota :: [[a]] -> [[a]]
-- tal que (rota xss) es la matriz obtenida girando 90º en el sentido
-- del reloj la matriz xss, Por ejemplo, 
--    rota [[1,2,3],[4,5,6],[7,8,9]]  ==  [[7,4,1],[8,5,2],[9,6,3]]
--    rota ["abcd","efgh","ijkl"]     ==  ["iea","jfb","kgc","lhd"]
-- ---------------------------------------------------------------------

rota :: [[a]] -> [[a]]
rota []     = []
rota ([]:_) = []
rota xss    = reverse (map head xss) : rota (map tail xss)
