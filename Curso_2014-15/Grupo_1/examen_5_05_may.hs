-- Informática (1º del Grado en Matemáticas)
-- 5º examen de evaluación continua (4 de mayo de 2015)                 
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Array
import Data.Char
import Data.List
import Data.Numbers.Primes
import I1M.Grafo
import Test.QuickCheck
import qualified Data.Matrix as M
import qualified Data.Set as S

-- ---------------------------------------------------------------------
-- Ejercicio 1. Dado dos números n y m, decimos que m es un múltiplo
-- especial de n si m es un múltiplo de n y m no tiene ningún factor
-- primo que sea congruente con 1 módulo 3.
-- 
-- Definir la función
--    multiplosEspecialesCota :: Int -> Int -> [Int]
-- tal que (multiplosEspecialesCota n l) es la lista ordenada de todos los
-- múltiplos especiales de n que son menores o iguales que l. Por ejemplo,
--    multiplosEspecialesCota 5 50  ==  [5,10,15,20,25,30,40,45,50]
--    multiplosEspecialesCota 7 50  ==  []
-- ----------------------------------------------------------------------------

multiplosEspecialesCota :: Int -> Int -> [Int]
multiplosEspecialesCota n l =
    [m | m <- [k*n | k <- [1..l `div` n]], 
         all (\p -> p `mod` 3 /= 1) (primeFactors m)]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Dado un grafo no dirigido G, un camino en G es una
-- secuencia de nodos [v(1),v(2),v(3),...,v(n)] tal que para todo i
-- entre 1 y n-1, (v(i),v(i+1)) es una arista de G. Por ejemplo, dados
-- los grafos  
--    g1 = creaGrafo ND (1,3) [(1,2,0),(1,3,0),(2,3,0)]
--    g2 = creaGrafo ND (1,4) [(1,2,0),(1,3,0),(1,4,0),(2,4,0),(3,4,0)]
-- la lista [1,2,3] es un camino en g1, pero no es un camino en g2
-- puesto que la arista (2,3) no existe en g2. 
--
-- Definir la función
--    camino :: (Ix a, Num t) => (Grafo a t) -> [a] -> Bool
-- tal que (camino g vs) se verifica si la lista de nodos vs es un camino
-- en el grafo g. Por ejemplo, 
--    camino g1 [1,2,3]  ==  True
--    camino g2 [1,2,3]  ==  False
-- .....................................................................

g1 = creaGrafo ND (1,3) [(1,2,0),(1,3,0),(2,3,0)]
g2 = creaGrafo ND (1,4) [(1,2,0),(1,3,0),(1,4,0),(2,4,0),(3,4,0)]

camino :: (Ix a, Num t) => (Grafo a t) -> [a] -> Bool
camino g vs = all (aristaEn g) (zip vs (tail vs))

-- ---------------------------------------------------------------------
-- Ejercicio 3. Una relación binaria homogénea R sobre un conjunto A se
-- puede representar mediante un par (xs,ps) donde xs es el conjunto de
-- los elementos de A (el universo de R) y ps es el conjunto de pares de
-- R (el grafo de R). El tipo de las relaciones binarias se define por
--    type Rel a = (S.Set a,S.Set (a,a))
-- Algunos ejemplos de relaciones binarias homogéneas son
--    r1 = (S.fromList [1..4], S.fromList [(1,2),(2,1),(1,3),(4,3)])
--    r2 = (S.fromList [1..4], S.fromList [(1,2),(2,1),(3,3),(4,3)])
--    r3 = (S.fromList [1..4], S.fromList [(1,2),(2,1),(1,4),(4,3)])
--    r4 = (S.fromList [1..4], S.fromList [(1,2),(2,1),(3,4),(4,3)])
--
-- Una relación binaria homogénea R = (U,G) es inyectiva si para todo x
-- en U hay un único y en U tal que (x,y) está en G. Por ejemplo, las
-- relaciones r2 y r4 son inyectivas, pero las relaciones r1 y r3 no.
--
-- Una relación binaria homogénea R = (U,G) es sobreyectiva si para todo
-- y en U hay algún x en U tal que (x,y) está en G. Por ejemplo, las
-- relaciones r3 y r4 son sobreyectivas, pero las relaciones r1 y r2
-- no. 
--
-- Una relación binaria homogénea R = (U,G) es biyectiva si es inyectiva y 
-- sobreyectiva. Por ejemplo, la relación r4 es biyectiva, pero las
-- relaciones r1, r2 y r3 no.
-- 
-- Definir la función
--    biyectiva :: (Ord a, Eq a) => Rel a -> Bool
-- tal que (biyectiva r) si verifica si la relación r es biyectiva. Por
-- ejemplo,
--    biyectiva r1  ==  False
--    biyectiva r2  ==  False
--    biyectiva r3  ==  False
--    biyectiva r4  ==  True
-- ---------------------------------------------------------------------

type Rel a = (S.Set a,S.Set (a,a))

r1 = (S.fromList [1..4], S.fromList [(1,2),(2,1),(1,3),(4,3)])
r2 = (S.fromList [1..4], S.fromList [(1,2),(2,1),(3,3),(4,3)])
r3 = (S.fromList [1..4], S.fromList [(1,2),(2,1),(1,4),(4,3)])
r4 = (S.fromList [1..4], S.fromList [(1,2),(2,1),(3,4),(4,3)])

biyectiva :: (Ord a, Eq a) => Rel a -> Bool
biyectiva (u,g) = S.map fst g == u && S.map snd g == u 

-- ---------------------------------------------------------------------
-- Ejercicio 4. La visibilidad de una lista es el número de elementos
-- que son estrictamente mayores que todos los anteriores. Por ejemplo,
-- la visibilidad de la lista [1,2,5,2,3,6] es 4.
--
-- La visibilidad de una matriz P es el par formado por las
-- visibilidades de las filas de P y las visibilidades de las
-- columnas de P. Por ejemplo, dada la matriz
--        ( 4 2 1 )
--    Q = ( 3 2 5 )
--        ( 6 1 8 )
-- la visibilidad de Q es ([1,2,2],[2,1,3]). 
-- 
-- Definir las funciones
--    visibilidadLista  :: [Int] -> Int
--    visibilidadMatriz :: M.Matrix Int -> ([Int],[Int])
-- tales que 
-- + (visibilidadLista xs) es la visibilidad de la lista xs. Por
--   ejemplo,
--      visibilidadLista [1,2,5,2,3,6]   ==  4
--      visibilidadLista [0,-2,5,1,6,6]  ==  3
-- + (visibilidadMatriz p) es la visibilidad de la matriz p. Por ejemplo,
--      ghci> visibilidadMatriz (M.fromLists [[4,2,1],[3,2,5],[6,1,8]])
--      ([1,2,2],[2,1,3])
--      ghci> visibilidadMatriz (M.fromLists [[0,2,1],[0,2,5],[6,1,8]])
--      ([2,3,2],[2,1,3])
-- ----------------------------------------------------------------------------

visibilidadLista :: [Int] -> Int
visibilidadLista xs =
    length [x | (ys,x) <- zip (inits xs) xs, all (<x) ys]

visibilidadMatriz :: M.Matrix Int -> ([Int],[Int])
visibilidadMatriz p =
    ([visibilidadLista [p M.! (i,j) | j <- [1..n]] | i <- [1..m]],
     [visibilidadLista [p M.! (i,j) | i <- [1..m]] | j <- [1..n]])
     where m = M.nrows p
           n = M.ncols p

-- ---------------------------------------------------------------------
-- Ejercicio 5. Decimos que un número es alternado si no tiene dos
-- cifras consecutivas iguales ni tres cifras consecutivas en orden
-- creciente no estricto o decreciente no estricto. Por ejemplo, los
-- números 132425 y 92745 son alternados, pero los números 12325 y 29778
-- no. Las tres primeras cifras de 12325 están en orden creciente y
-- 29778 tiene dos cifras iguales consecutivas.
-- 
-- Definir la constante
--    alternados :: [Integer]
-- cuyo valor es la lista infinita de los números alternados. Por ejemplo,
--    take 10 alternados                      ==  [0,1,2,3,4,5,6,7,8,9]
--    length (takeWhile (< 1000) alternados)  ==  616
--    alternados !! 1234567                   ==  19390804
-- ----------------------------------------------------------------------------

-- 1ª definición
-- =============

-- (cifras n) es la lista de las cifras de n. Por ejemplo.
--    cifras 325  ==  [3,2,5]
cifras :: Integer -> [Int]
cifras n = map digitToInt (show n)

-- (cifrasAlternadas xs) se verifica si las lista de cifras xs es
-- alternada. Por ejemplo,
--    cifrasAlternadas [1,3,2,4,2,5]  ==  True
--    cifrasAlternadas [9,2,7,4,5]    ==  True
--    cifrasAlternadas [1,2,3,2,5]    ==  False
--    cifrasAlternadas [2,9,7,7,8]    ==  False
cifrasAlternadas :: [Int] -> Bool
cifrasAlternadas [x1,x2] = x1 /= x2
cifrasAlternadas (x1:x2:x3:xs) =
    not (((x1 <= x2) && (x2 <= x3)) || ((x1 >= x2) && (x2 >= x3))) &&
    cifrasAlternadas (x2:x3:xs)
cifrasAlternadas _ = True

-- (alternado n) se verifica si n es un número alternado. Por ejemplo,
--    alternado 132425  ==  True
--    alternado 92745   ==  True
--    alternado 12325   ==  False
--    alternado 29778   ==  False
alternado :: Integer -> Bool
alternado n = cifrasAlternadas (cifras n)

alternados1 :: [Integer]
alternados1 = filter alternado [0..]

-- 2ª definición
-- =============

-- (extiendeAlternado n) es la lista de números alternados que se pueden
-- obtener añadiendo una cifra al final del número alternado n. Por
-- ejemplo,
--    extiendeAlternado 7   ==  [70,71,72,73,74,75,76,78,79]
--    extiendeAlternado 24  ==  [240,241,242,243]
--    extiendeAlternado 42  ==  [423,424,425,426,427,428,429]
extiendeAlternado :: Integer -> [Integer]
extiendeAlternado n 
    | n < 10    = [n*10+h | h <- [0..n-1]++[n+1..9]]
    | d < c     = [n*10+h | h <- [0..c-1]]
    | otherwise = [n*10+h | h <- [c+1..9]]
    where c = n `mod` 10
          d = (n `mod` 100) `div` 10

alternados2 :: [Integer]
alternados2 = concat (iterate (concatMap extiendeAlternado) [0])
