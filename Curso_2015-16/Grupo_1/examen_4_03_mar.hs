-- Informática (1º del Grado en Matemáticas)
-- 4º examen de evaluación continua (3 de marzo de 2016)
-- ---------------------------------------------------------------------

import Data.List
import Data.Numbers.Primes
import Data.Array
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. [2.5 puntos] Un número es libre de cuadrados si no es
-- divisible por el cuadrado de ningún entero mayor que 1. Por ejemplo,
-- 70 es libre de cuadrado porque sólo es divisible por 1, 2, 5, 7 y 70;
-- en cambio, 40 no es libre de cuadrados porque es divisible por 2². 
--
-- Definir la función
--    libreDeCuadrados :: Integer -> Bool
-- tal que (libreDeCuadrados x) se verifica si x es libre de
-- cuadrados. Por ejemplo,
--    libreDeCuadrados 70                 ==  True
--    libreDeCuadrados 40                 ==  False
--    libreDeCuadrados 510510             ==  True
--    libreDeCuadrados (((10^10)^10)^10)  ==  False
-- ---------------------------------------------------------------------

-- 1ª definición
libreDeCuadrados :: Integer -> Bool
libreDeCuadrados n = all (==1) (map length $ group $ primeFactors n)

-- 2ª definición                     
libreDeCuadrados2 :: Integer -> Bool
libreDeCuadrados2 n = nub ps == ps
    where ps = primeFactors n

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Un número de Lucas-Carmichael es un entero positivo n
-- compuesto, impar, libre de cuadrados y  tal que si p es un factor
-- primo de n, entonces p + 1 es un factor de n + 1.  
-- 
-- Definir la función
--    lucasCarmichael :: [Integer]
-- que calcula la sucesión de los números de Lucas-Carmichael. Por
-- ejemplo,
--    take 8 lucasCarmichael == [399,935,2015,2915,4991,5719,7055,8855]
-- ---------------------------------------------------------------------

lucasCarmichael:: [Integer]
lucasCarmichael = 
    [x | x <- [2..],
         let ps = primeFactors x,
         and [mod (x+1) (p+1) == 0 | p <- ps],
         nub ps == ps,
         not (isPrime x),
         odd x]

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Calcular el primer número de Lucas-Carmichael con 5
-- factores primos.
-- ---------------------------------------------------------------------

-- El cálculo es
--    ghci> head [x | x <- lucasCarmichael, length (primeFactors x) == 5]
--    588455

-- ---------------------------------------------------------------------
-- Ejercicio 2. [2 puntos] Representamos los árboles binarios con
-- elementos en las hojas y en los nodos mediante el tipo de dato 
--    data Arbol a = H a | N a (Arbol a) (Arbol a) deriving Show
-- Por ejemplo,
--    ej1 :: Arbol Int
--    ej1 = N 5 (N 2 (H 1) (H 2)) (N 3 (H 4) (H 2))
-- 
-- Definir la función
--    ramasCon :: Eq a => Arbol a -> a -> [[a]]
-- tal que (ramasCon a x) es la lista de las ramas del árbol a en las
-- que aparece el elemento x. Por ejemplo,
--   ramasCon ej1 2 ==  [[5,2,1],[5,2,2],[5,3,2]]
-- ---------------------------------------------------------------------

data Arbol a = H a | N a (Arbol a) (Arbol a) 
               deriving Show
 
ej1 :: Arbol Int
ej1 = N 5 (N 2 (H 1) (H 2)) (N 3 (H 4) (H 2))
 
ramasCon :: Eq a => Arbol a -> a -> [[a]]
ramasCon a x = [ys | ys <- ramas a, x `elem` ys]
 
ramas :: Arbol a -> [[a]]
ramas (H x)     = [[x]]
ramas (N x i d) = [x:ys | ys <- ramas i ++ ramas d]

-- ---------------------------------------------------------------------
-- Ejercicio 3. [2 puntos] Representamos las matrices mediante el tipo
-- de dato 
--    type Matriz a = Array (Int,Int) a
-- Por ejemplo,
--    ejM :: Matriz Int
--    ejM = listArray ((1,1),(2,4)) [1,2,3,0,4,5,6,7]
-- representa la matriz
--    |1 2 3 0|
--    |4 5 6 7|
-- 
-- Definir la función
--    ampliada :: Num a => Matriz a -> Matriz a
-- tal que (ampliada p) es la matriz obtenida al añadir una nueva fila a
-- p cuyo elemento i-ésimo es la suma de la columna i-ésima de p. Por
-- ejemplo,

--    |1 2 3 0|        |1 2 3 0|
--    |4 5 6 7| ==>    |4 5 6 7| 
--                     |5 7 9 7|
-- 
-- En Haskell,
--    ghci> ampliada ejM
--    array ((1,1),(3,4)) [((1,1),1),((1,2),2),((1,3),3),((1,4),0),
--                         ((2,1),4),((2,2),5),((2,3),6),((2,4),7),
--                         ((3,1),5),((3,2),7),((3,3),9),((3,4),7)]
-- ---------------------------------------------------------------------

type Matriz a = Array (Int,Int) a
 
ejM :: Matriz Int
ejM = listArray ((1,1),(2,4)) [1,2,3,0,4,5,6,7]
 
ampliada :: Num a => Matriz a -> Matriz a
ampliada p = 
    array ((1,1),(m+1,n)) [((i,j),f i j) | i <- [1..m+1], j <- [1..n]]
    where (_,(m,n)) = bounds p
          f i j | i <= m    = p!(i,j)
                | otherwise = sum [p!(i,j) | i <- [1..m]]

-- ---------------------------------------------------------------------
-- Ejercicio 4. [2 puntos] Definir la función 
--    acumulaSumas :: [[Int]] -> [Int]
-- tal que (acumulaSumas xss) calcula la suma de las listas de xss de
-- forma acumulada. Por ejemplo,
--   acumulaSumas [[1,2,5],[3,9], [1,7,6,2]] == [8,20,36]
-- ---------------------------------------------------------------------

-- 1ª definición
acumulaSumas :: [[Int]] -> [Int]
acumulaSumas xss = acumula $ map sum xss

acumula :: [Int] -> [Int] 
acumula [] = []
acumula (x:xs) = x:(map (+x) $ acumula xs)

-- 2ª definición
acumulaSumas2 :: [[Int]] -> [Int]
acumulaSumas2 xss = scanl1 (+) $ map sum xss

-- 3ª definición
acumulaSumas3 :: [[Int]] -> [Int]
acumulaSumas3  = scanl1 (+) . map sum 
                    
-- ---------------------------------------------------------------------
-- Ejercicio 5 (en Maxima). [1.5 puntos]  Construir un programa que
-- calcule la suma de todos los números primos menores que n. Por
-- ejemplo, 
--    sumaPrimosMenores (10)     = 17
--    sumaPrimosMenores (200000) = 1709600813
-- ---------------------------------------------------------------------
