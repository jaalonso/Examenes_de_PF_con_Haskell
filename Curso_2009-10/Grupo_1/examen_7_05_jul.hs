-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- Examen de la 1ª convocatoria (5 de julio de 2010)
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    numeroCerosFactorial :: Integer -> Integer
-- tal que (numeroCerosFactorial n) es el número de ceros con los que
-- termina el factorial de n. Por ejemplo,
--    numeroCerosFactorial 17 == 3
-- ---------------------------------------------------------------------

numeroCerosFactorial :: Integer -> Integer
numeroCerosFactorial n = numeroCeros (product [1..n])

-- (numeroCeros x) es el número de ceros con los que termina x. Por
-- ejemplo, 
--    numeroCeros 35400  ==  2 
numeroCeros :: Integer -> Integer
numeroCeros x | mod x 10 /= 0 = 0
              | otherwise     = 1 + numeroCeros (div x 10)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Las matrices pueden representarse mediante una lista de
-- listas donde cada una de las lista representa una fila  de la
-- matriz. Por ejemplo, la matriz 
--    |1 0 -2|
--    |0 3 -1|
-- puede representarse por [[1,0,-2],[0,3,-1]]. Definir la función
--    producto :: Num t => [[t]] -> [[t]] -> [[t]]
-- tal que (producto a b) es el producto de las matrices a y b. Por
-- ejemplo, 
--    ghci> producto [[1,0,-2],[0,3,-1]] [[0,3],[-2,-1],[0,4]]
--    [[0,-5],[-6,-7]]
-- ---------------------------------------------------------------------

producto :: Num t => [[t]] -> [[t]] -> [[t]]
producto a b = 
    [[sum [x*y | (x,y) <- zip fil col] | col <- transpose b] | fil <- a]

-- ---------------------------------------------------------------------
-- Ejercicio 3. El ejercicio 4 de la Olimpiada Matemáticas de 1993 es el
-- siguiente: 
--    Demostrar que para todo número primo p distinto de 2 y de 5,
--    existen infinitos múltiplos de p de la forma 1111......1 (escrito
--    sólo con unos).  
-- Definir la función
--    multiplosEspeciales :: Integer -> Int -> [Integer]
-- tal que (multiplosEspeciales p n) es una lista de n múltiplos p de la
-- forma 1111...1 (escrito sólo con unos), donde p es un número primo
-- distinto de 2 y 5. Por ejemplo,
--    multiplosEspeciales 7 2  ==  [111111,111111111111]
-- ---------------------------------------------------------------------

-- 1ª definición:
multiplosEspeciales :: Integer -> Int -> [Integer]
multiplosEspeciales p n = take n [x | x <- unos, mod x p == 0]

-- unos es la lista de los números de la forma 111...1 (escrito sólo con
-- unos). Por ejemplo,
--    take 5 unos  ==  [1,11,111,1111,11111]
unos :: [Integer]
unos = 1 : [10*x+1 | x <- unos]

-- Otra definición no recursiva de unos es
unos' :: [Integer]
unos' = [div (10^n-1) 9 | n <- [1..]]

-- 2ª definición:
multiplosEspeciales2 :: Integer -> Int -> [Integer]
multiplosEspeciales2 p n = 
    [div (10^((p-1)*x)-1) 9 | x <- [1..fromIntegral n]]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    recorridos :: [a] -> [[a]]
-- tal que (recorridos xs) es la lista de todos los posibles recorridos
-- por el grafo cuyo conjunto de vértices es xs y cada vértice se
-- encuentra conectado con todos los otros y los recorridos pasan por
-- todos los vértices una vez y terminan en el vértice inicial. Por
-- ejemplo, 
--    ghci> recorridos [2,5,3]
--    [[2,5,3,2],[5,2,3,5],[3,5,2,3],[5,3,2,5],[3,2,5,3],[2,3,5,2]]
-- Indicación: No importa el orden de los recorridos en la lista.
-- ---------------------------------------------------------------------

recorridos :: [a] -> [[a]]
recorridos xs = [(y:ys)++[y] | (y:ys) <- permutations xs]
