-- Informática (1º del Grado en Matemáticas)
-- Examen de la 3º convocatoria (20 de noviembre de 2014)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    mayorProducto :: Int -> [Int] -> Int
-- tal que (mayorProducto n xs) es el mayor producto de una sublista de
-- xs de longitud n. Por ejemplo,
--    mayorProducto 3 [3,2,0,5,4,9,1,3,7]  ==  180
-- ya que de todas las sublistas de longitud 3 de [3,2,0,5,4,9,1,3,7] la
-- que tiene mayor producto es la [5,4,9] cuyo producto es 180.
-- ---------------------------------------------------------------------
          
mayorProducto :: Int -> [Int] -> Int
mayorProducto n cs 
    | length cs < n = 1
    | otherwise     = maximum [product xs | xs <- segmentos n cs]
   where segmentos n cs = [take n xs | xs <- tails cs]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    sinDobleCero :: Int -> [[Int]]
-- tal que (sinDobleCero n) es la lista de las listas de longitud n
-- formadas por el 0 y el 1 tales que no contiene dos ceros
-- consecutivos. Por ejemplo,
--    ghci> sinDobleCero 2
--    [[1,0],[1,1],[0,1]]
--    ghci> sinDobleCero 3
--    [[1,1,0],[1,1,1],[1,0,1],[0,1,0],[0,1,1]]
--    ghci> sinDobleCero 4
--    [[1,1,1,0],[1,1,1,1],[1,1,0,1],[1,0,1,0],[1,0,1,1],
--     [0,1,1,0],[0,1,1,1],[0,1,0,1]]
-- ---------------------------------------------------------------------

sinDobleCero :: Int -> [[Int]]
sinDobleCero 0 = [[]]
sinDobleCero 1 = [[0],[1]]
sinDobleCero n = [1:xs | xs <- sinDobleCero (n-1)] ++
                 [0:1:ys | ys <- sinDobleCero (n-2)]

-- ---------------------------------------------------------------------
-- Ejercicio 3. La sucesión A046034 de la OEIS (The On-Line Encyclopedia
-- of Integer Sequences) está formada por los números tales que todos
-- sus dígitos son primos. Los primeros términos de A046034 son 
--    2,3,5,7,22,23,25,27,32,33,35,37,52,53,55,57,72,73,75,77,222,223
-- 
-- Definir la constante
--    numerosDigitosPrimos :: [Int]
-- cuyos elementos son los términos de la sucesión A046034. Por ejemplo,
--    ghci> take 22 numerosDigitosPrimos
--    [2,3,5,7,22,23,25,27,32,33,35,37,52,53,55,57,72,73,75,77,222,223]
-- ¿Cuántos elementos hay en la sucesión menores que 2013?
-- ---------------------------------------------------------------------

numerosDigitosPrimos :: [Int]
numerosDigitosPrimos = 
    [n | n <- [2..], digitosPrimos n]

-- (digitosPrimos n) se verifica si todos los dígitos de n son
-- primos. Por ejemplo,
--    digitosPrimos 352  ==  True
--    digitosPrimos 362  ==  False
digitosPrimos :: Int -> Bool
digitosPrimos n = all (`elem` "2357") (show n)

-- 2ª definición de digitosPrimos:
digitosPrimos2 :: Int -> Bool
digitosPrimos2 n = subconjunto (cifras n) [2,3,5,7]

-- (cifras n) es la lista de las cifras de n. Por ejemplo,
cifras :: Int -> [Int]
cifras n = [read [x] | x <-show n]

-- (subconjunto xs ys) se verifica si xs es un subconjunto de ys. Por
-- ejemplo, 
subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto xs ys = and [elem x ys | x <- xs]

-- El cálculo es
--    ghci> length (takeWhile (<2013) numerosDigitosPrimos)
--    84

-- ----------------------------------------------------------------------
-- Ejercicio 4. Entre dos matrices de la misma dimensión se puede
-- aplicar distintas operaciones binarias entre los elementos en la
-- misma posición. Por ejemplo, si a y b son las matrices 
--    |3 4 6|     |1 4 2|
--    |5 6 7|     |2 1 2|
-- entonces a+b y a-b son, respectivamente
--    |4 8 8|     |2 0 4|
--    |7 7 9|     |3 5 5|
-- 
-- Las matrices enteras se pueden representar mediante tablas con
-- índices enteros: 
--    type Matriz = Array (Int,Int) Int
-- y las matrices anteriores se definen por
--    a, b :: Matriz
--    a = listArray ((1,1),(2,3)) [3,4,6,5,6,7]
--    b = listArray ((1,1),(2,3)) [1,4,2,2,1,2]
-- 
-- Definir la función
--    opMatriz :: (Int -> Int -> Int) -> Matriz -> Matriz -> Matriz
-- tal que (opMatriz f p q) es la matriz obtenida aplicando la operación
-- f entre los elementos de p y q de la misma posición. Por ejemplo,
--    ghci> opMatriz (+) a b
--    array ((1,1),(2,3)) [((1,1),4),((1,2),8),((1,3),8),
--                         ((2,1),7),((2,2),7),((2,3),9)]
--    ghci> opMatriz (-) a b
--    array ((1,1),(2,3)) [((1,1),2),((1,2),0),((1,3),4),
--                         ((2,1),3),((2,2),5),((2,3),5)]
-- --------------------------------------------------------------------- 

type Matriz = Array (Int,Int) Int

a, b :: Matriz
a = listArray ((1,1),(2,3)) [3,4,6,5,6,7]
b = listArray ((1,1),(2,3)) [1,4,2,2,1,2]

-- 1ª definición
opMatriz :: (Int -> Int -> Int) -> Matriz -> Matriz -> Matriz
opMatriz f p q = 
    array ((1,1),(m,n)) [((i,j), f (p!(i,j)) (q!(i,j)))
                      | i <- [1..m], j <- [1..n]] 
    where (_,(m,n)) = bounds p

-- 2ª definición
opMatriz2 :: (Int -> Int -> Int) -> Matriz -> Matriz -> Matriz
opMatriz2 f p q = 
    listArray (bounds p) [f x y | (x,y) <- zip (elems p) (elems q)]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Las expresiones aritméticas se pueden definir usando el
-- siguiente tipo de datos 
--    data Expr = N Int 
--              | X 
--              | S Expr Expr 
--              | R Expr Expr 
--              | P Expr Expr 
--              | E Expr Int
--              deriving (Eq, Show)
-- Por ejemplo, la expresión 
--    3*x - (x+2)^7
-- se puede definir por
--    R (P (N 3) X) (E (S X (N 2)) 7)
-- 
-- Definir la función  
--    maximo :: Expr -> [Int] -> (Int,[Int])
-- tal que (maximo e xs) es el par formado por el máximo valor de la
-- expresión e para los puntos de xs y en qué puntos alcanza el
-- máximo. Por ejemplo, 
--    ghci> maximo (E (S (N 10) (P (R (N 1) X) X)) 2) [-3..3]
--    (100,[0,1])
-- ---------------------------------------------------------------------

data Expr = N Int 
          | X 
          | S Expr Expr 
          | R Expr Expr 
          | P Expr Expr 
          | E Expr Int
          deriving (Eq, Show)

maximo :: Expr -> [Int] -> (Int,[Int])
maximo e ns = (m,[n | n <- ns, valor e n == m])  
    where m = maximum [valor e n | n <- ns]

valor :: Expr -> Int -> Int
valor (N x) _ = x
valor X     n = n
valor (S e1 e2) n = (valor e1 n) + (valor e2 n)
valor (R e1 e2) n = (valor e1 n) - (valor e2 n)
valor (P e1 e2) n = (valor e1 n) * (valor e2 n)
valor (E e  m ) n = (valor e  n)^m
