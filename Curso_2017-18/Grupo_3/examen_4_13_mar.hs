-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 4º examen de evaluación continua (13 de marzo de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías                                                        --
-- ---------------------------------------------------------------------

import Data.Matrix 
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Una lista de números se puede describir indicando
-- cuantas veces se repite cada elemento. Por ejemplo la lista
-- [1,1,1,3,3,2,2] se puede describir indicando que hay 3 unos, 2 treses
-- y 2 doses. De esta forma, la descripción de una lista es otra lista
-- en la que se indica qué elementos hay y cuántas veces se repiten. Por
-- ejemplo, la descripción de la lista [1,1,1,3,3,2,2] es [3,1,2,3,2,2].
--
-- La secuencia de listas que comienza en [1] y en la que cada una de
-- los demás elementos es la descripción de la lista anterior se llama
-- 'Look And Say'. Sus primeros términos son
--   [1], [1,1], [2,1], [1,2,1,1], [1,1,1,2,2,1], [3,1,2,2,1,1], ...
--
-- Definir la constante
--   lookAndSay :: [[Int]]
-- cuyo valor es la secuencia infinita 'Look And Say'. Por ejemplo,
--   take 5 lookAndSay         == [[1],[1,1],[2,1],[1,2,1,1],[1,1,1,2,2,1]]
--   lookAndSay !! 8           == [3,1,1,3,1,2,1,1,1,3,1,2,2,1]
--   length (lookAndSay !! 20) == 408
--   sum (lookAndSay !! 20)    == 679
-- ---------------------------------------------------------------------

lookAndSay :: [[Int]]
lookAndSay =
  [1] : map describe lookAndSay

describe :: [Int] -> [Int]
describe xs =
  concatMap (\ g -> [length g,head g]) (group xs)

-- ---------------------------------------------------------------------
-- Ejercicio 2. La identidad
--
--                       x-1
--   (sqrt x) = 1 + --------------
--                   1 + (sqrt x)
--
-- se puede usar para aproximar la raíz cuadrada de un número de la
-- siguiente forma:
--   y(0)   = 1
--   y(n+1) = 1 + (x-1)/(1+y(n))
--
-- Definir la función
--   aproximaRaiz :: Double -> Double
-- tal que (aproximaRaiz x) es la aproximación con 12 cifras decimales
-- exactas de la raíz cuadrada del número positivo x, calculada usando
-- el método anterior. Por ejemplo,
--   aproximaRaiz 2   ==  1.4142135623728214
--   aproximaRaiz 3   ==  1.7320508075686347
--   aproximaRaiz 10  ==  3.1622776601685203
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

aproximaRaiz :: Double -> Double
aproximaRaiz x =
  aproximaRaizAux x 1

aproximaRaizAux :: Double -> Double -> Double
aproximaRaizAux x y
  | abs (x-y^2) < 10**(-12) = y
  | otherwise               = aproximaRaizAux x (1+(x-1)/(1+y))

-- 2ª solución
-- ===========

aproximaRaiz2 :: Double -> Double
aproximaRaiz2 x =
  until (\y -> abs (x-y^2) < 10**(-12))
        (\y -> 1+(x-1)/(1+y))
        1

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--   permutaDos :: [a] -> [[a]]
-- tal que (permutaDos xs) es la familia de todas las permutaciones de
-- la lista xs en las que sólo hay dos elementos intercambiados. Por
-- ejemplo, 
--   permutaDos [1..3]            ==  [[1,3,2],[2,1,3],[3,2,1]]
--   permutaDos [1..4]            ==  [[1,2,4,3],[1,3,2,4],[1,4,3,2],
--                                     [2,1,3,4],[3,2,1,4],[4,2,3,1]]
--   length (permutaDos [1..10])  ==  45
--   length (permutaDos [1..20])  ==  190
-- ----------------------------------------------------------------------------

permutaDos :: [a] -> [[a]]
permutaDos []      = []
permutaDos [x]     = []
permutaDos [x1,x2] = [[x2,x1]]
permutaDos (x:xs) =
  map (x:) (permutaDos xs) ++ intercambiaPrimero (x:xs)

-- (intercambiaPrimero xs) es la lista de las lista obtenidas
-- intercambiando el primer elemento de xs con cada uno de los
-- restantes. Por ejemplo,
--    λ> intercambiaPrimero [1..5]
--    [[2,1,3,4,5],[3,2,1,4,5],[4,2,3,1,5],[5,2,3,4,1]]
intercambiaPrimero :: [a] -> [[a]]
intercambiaPrimero [] = []
intercambiaPrimero (x:ys) =
  [(ys!!i : take i ys) ++ (x : drop (i+1) ys)
  | i <- [0..length ys -1]]

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Consideremos el siguiente tipo de matriz:
--     ( V1  V2  V3  V4  .. )
--     ( V2  V3  V4  V5  .. )
--     ( V3  V4  V5  V6  .. )
--     ( ..  ..  ..  ..  .. )
-- donde todos los elementos de cualquier diagonal paralela a la diagonal
-- secundaria son iguales. Diremos que ésta es una matriz inclinada.
--
-- Definir la función:
--   matrizInclinada :: Int -> Int -> Int -> Matrix Int
-- tal que (matrizInclinada v p q) es la matriz inclinada de p filas
-- por q columnas en la que cada valor Vi es igual a i*v. Por ejemplo,
--    λ> matrizInclinada 1 3 5
--    ( 1 2 3 4 5 )
--    ( 2 3 4 5 6 )
--    ( 3 4 5 6 7 )
--    
--    λ> matrizInclinada 2 4 2
--    (  2  4 )
--    (  4  6 )
--    (  6  8 )
--    (  8 10 )
--    
--    λ> matrizInclinada (-2) 3 3
--    (  -2  -4  -6 )
--    (  -4  -6  -8 )
--    (  -6  -8 -10 )
-- ---------------------------------------------------------------------

matrizInclinada :: Int -> Int -> Int -> Matrix Int
matrizInclinada v p q =
  matrix p q (\ (i,j) -> v*(i+j-1))

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función:
--   esMatrizInclinada :: Matrix Int -> Bool
-- tal que (esMatrizInclinada a) se verifica si a es una matriz
-- inclinada. 
--   esMatrizInclinada (matrizInclinada 1 3 5)  ==  True
--   esMatrizInclinada (identity 4)             ==  False
-- ---------------------------------------------------------------------

esMatrizInclinada :: Matrix Int -> Bool
esMatrizInclinada a =
  all todosIguales (diagonalesSecundarias a)
  
-- (diagonalesSecundarias a) es la lista de las diagonales secundarias
-- de la matriz a. Por ejemplo,
--    λ> diagonalesSecundarias ((matrizInclinada 1 3 5))
--    [[1],[2,2],[3,3,3],[4,4,4],[5,5,5],[6,6],[7]]
diagonalesSecundarias :: Matrix Int -> [[Int]]
diagonalesSecundarias a =
  [[a ! (i,k-i) | i <- [1..m], 1+i <= k, k <= n+i]
   | k <- [2..m+n]]
  where m = nrows a
        n = ncols a

-- (todosIguales xs) se verifica si todos los elementos de xs son
-- iguales. Por ejemplo,
--    todosIguales [3,3,3]  ==  True
--    todosIguales [3,4,3]  ==  False
todosIguales :: Eq a => [a] -> Bool
todosIguales []     = True
todosIguales (x:xs) = all (==x) xs
