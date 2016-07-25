-- Informática (1º del Grado en Matemáticas)
-- Examen de la 2ª convocatoria (16 de septiembre de 2011)
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    subsucesiones :: [Integer] -> [[Integer]]
-- tal que (subsucesiones xs) es la lista de las subsucesiones
-- crecientes de elementos consecutivos de xs. Por ejemplo, 
--    subsucesiones [1,0,1,2,3,0,4,5]  == [[1],[0,1,2,3],[0,4,5]]
--    subsucesiones [5,6,1,3,2,7]      == [[5,6],[1,3],[2,7]]
--    subsucesiones [2,3,3,4,5]        == [[2,3],[3,4,5]]
--    subsucesiones [7,6,5,4]          == [[7],[6],[5],[4]]
-- ---------------------------------------------------------------------
 
subsucesiones :: [Integer] -> [[Integer]]
subsucesiones []  = []
subsucesiones [x] = [[x]]
subsucesiones (x:y:zs)
    | x < y     = (x:us):vss
    | otherwise = [x]:p
    where p@(us:vss) = subsucesiones (y:zs)
 
-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    menor :: Ord a => [[a]] -> a
-- tal que (menor xss) es el menor elemento común a todas las listas de
-- xss, donde las listas de xss están ordenadas (de menor a mayor) y
-- pueden ser infinitas. Por ejemplo,
--    menor [[3,4,5]]                           ==  3
--    menor [[1,2,3,4,5,6,7],[0.5,3/2,4,19]]    ==  4.0
--    menor [[0..],[4,6..],[2,3,5,7,11,13,28]]  ==  28
-- ---------------------------------------------------------------------
 
menor :: Ord a => [[a]] -> a
menor (xs:xss) = 
    head [x | x <- xs, all (x `pertenece`) xss] 

-- (pertenece x ys) se verifica si x pertenece a la lista ys, donde ys
-- es una lista ordenada de menor a mayor y, posiblemente, infinita. Por
-- ejemplo, 
--    pertenece 6 [0,2..]  ==  True
--    pertenece 7 [0,2..]  ==  False
pertenece :: Ord a => a -> [a] -> Bool
pertenece x [] = False
pertenece x (y:ys) | x < y  = False
                   | x == y = True
                   | x > y  = pertenece x ys

-- ---------------------------------------------------------------------
-- Ejercicio 3. Un conjunto A está cerrado respecto de una función  f si
-- para todo elemento x de A se tiene que f(x) pertenece a A. La
-- clausura de un conjunto B respecto de una función f es el menor
-- conjunto A que contiene a B y es cerrado respecto de f. Por ejemplo, 
-- la clausura de {0,1,2] respecto del opuesto es {0,1,2,-1,-2}.
-- 
-- Definir la función  
--    clausura :: Eq a => (a -> a) -> [a] -> [a]
-- tal que (clausura f xs) es la clausura de xs respecto de f. Por
-- ejemplo, 
--    clausura (\x -> -x) [0,1,2]         ==  [0,1,2,-1,-2]
--    clausura (\x -> (x+1) `mod` 5) [0]  ==  [0,1,2,3,4]
-- ---------------------------------------------------------------------

clausura :: Eq a => (a -> a) -> [a] -> [a]
clausura f xs = clausura' f xs xs
    where clausura' f xs ys | null zs = ys
                            | otherwise = clausura' f zs (ys++zs)
              where zs = nuevosSucesores f xs ys

nuevosSucesores :: Eq a => (a -> a) -> [a] -> [a] -> [a]
nuevosSucesores f xs ys = nub [f x | x <- xs] \\ ys

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. El problema del laberinto numérico consiste en, dados
-- un par de números, encontrar la longitud del camino más corto entre
-- ellos usando sólo las siguientes operaciones: 
--    * multiplicar por 2,
--    * dividir por 2 (sólo para los pares) y
--    * sumar 2.
-- Por ejemplo,
--    longitudCaminoMinimo 3 12  ==  2
--    longitudCaminoMinimo 12 3  ==  2
--    longitudCaminoMinimo 9 2   ==  8
--    longitudCaminoMinimo 2 9   ==  5
-- Unos caminos mínimos correspondientes a los ejemplos anteriores son
-- [3,6,12], [12,6,3], [9,18,20,10,12,6,8,4,2] y [2,4,8,16,18,9].
-- 
-- Definir la función
--    orbita :: Int -> [Int] -> [Int]
-- tal que (orbita n xs) es el conjunto de números que se pueden obtener
-- aplicando como máximo n veces las operaciones a los elementos de
-- xs. Por ejemplo, 
--    orbita 0 [12]  ==  [12]
--    orbita 1 [12]  ==  [6,12,14,24]
--    orbita 2 [12]  ==  [3,6,7,8,12,14,16,24,26,28,48]
-- ---------------------------------------------------------------------

orbita :: Int -> [Int] -> [Int]
orbita 0 xs = sort xs
orbita n xs = sort (nub (ys ++ concat [sucesores x | x <- ys]))
    where ys = orbita (n-1) xs
          sucesores x | odd x     = [2*x, x+2]
                      | otherwise = [2*x, x `div` 2, x+2]

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función
--    longitudCaminoMinimo :: Int -> Int -> Int
-- tal que (longitudCaminoMinimo x y) es la longitud del camino mínimo
-- desde x hasta y en el laberinto numérico. 
--    longitudCaminoMinimo 3 12  ==  2
--    longitudCaminoMinimo 12 3  ==  2
--    longitudCaminoMinimo 9 2   ==  8
--    longitudCaminoMinimo 2 9   ==  5
-- ---------------------------------------------------------------------

longitudCaminoMinimo :: Int -> Int -> Int
longitudCaminoMinimo x y = 
    head [n | n <- [1..], y `elem` orbita n [x]] 

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. En este ejercicio se estudia las relaciones entre los
-- valores de polinomios y los de sus correspondientes expresiones
-- aritméticas. 
-- 
-- Las expresiones aritméticas construidas con una variables, los
-- números enteros y las operaciones de sumar y multiplicar se pueden
-- representar mediante el tipo de datos Exp definido por   
--    data Exp = Var | Const Int | Sum Exp Exp | Mul Exp  Exp
--               deriving Show
-- Por ejemplo, la expresión 3+5x^2 se puede representar por
--    exp1 :: Exp
--    exp1 = Sum (Const 3) (Mul Var (Mul Var (Const 5)))
-- 
-- Definir la función 
--    valorE :: Exp -> Int -> Int
-- tal que (valorE e n) es el valor de la expresión e cuando se
-- sustituye su variable por n. Por ejemplo,
--    valorE exp1 2  ==  23
-- ---------------------------------------------------------------------
 
data Exp = Var | Const Int | Sum Exp Exp | Mul Exp  Exp
           deriving Show
 
exp1 :: Exp
exp1 = Sum (Const 3) (Mul Var (Mul Var (Const 5)))
 
valorE :: Exp -> Int -> Int
valorE Var         n = n
valorE (Const a)   n = a
valorE (Sum e1 e2) n = valorE e1 n + valorE e2 n
valorE (Mul e1 e2) n = valorE e1 n * valorE e2 n
 
-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Los polinomios se pueden representar por la lista de
-- sus coeficientes. Por ejemplo, el polinomio 3+5x^2 se puede
-- representar por [3,0,5].  
-- 
-- Definir la función
--    expresion :: [Int] -> Exp
-- tal que (expresion p) es una expresión aritmética equivalente al
-- polinomio p. Por ejemplo,
--    ghci> expresion [3,0,5]
--    Sum (Const 3) (Mul Var (Sum (Const 0) (Mul Var (Const 5))))
-- ---------------------------------------------------------------------
 
expresion :: [Int] -> Exp
expresion [a]   = Const a
expresion (a:p) = Sum (Const a) (Mul Var (expresion p))
 
-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Definir la función
--    valorP :: [Int] -> Int -> Int
-- tal que (valorP p n) es el valor del polinomio p cuando se sustituye
-- su variable por n. Por ejemplo,
--    valorP [3,0,5] 2  ==  23
-- ---------------------------------------------------------------------
 
valorP :: [Int] -> Int -> Int
valorP [a] _ = a
valorP (a:p) n = a + n * valorP p n
 
-- ---------------------------------------------------------------------
-- Ejercicio 5.4. Comprobar con QuickCheck que, para todo polinomio p y
-- todo entero n,
--    valorP p n == valorE (expresion p) n
-- ---------------------------------------------------------------------
 
-- La propiedad es
prop_valor :: [Int] -> Int -> Property
prop_valor p n =
    not (null p) ==> 
    valorP p n == valorE (expresion p) n
 
-- La comprobación es
--    ghci> quickCheck prop_valor
--    +++ OK, passed 100 tests.
