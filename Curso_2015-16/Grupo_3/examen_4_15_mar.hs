-- Informática (1º del Grado en Matemáticas)
-- 4º examen de evaluación continua (15 de marzo de 2016)               
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Array
import qualified Data.Matrix as M

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    sumaMaxima :: [Integer] -> Integer
-- tal que (sumaMaxima xs) es el valor máximo de la suma de elementos
-- consecutivos de la lista xs. Por ejemplo,
--    sumaMaxima []             ==  0
--    sumaMaxima [-1,-2,-3]     ==  -1
--    sumaMaxima [2,-2,3,-3,4]  ==  4
--    sumaMaxima [2,-1,3,-2,3]  ==  5
--    sumaMaxima [1,-1,3,-2,4]  ==  5
--    sumaMaxima [2,-1,3,-2,4]  ==  6
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

sumaMaxima1 :: [Integer] -> Integer
sumaMaxima1 [] = 0
sumaMaxima1 xs =
    maximum (map sum [sublista xs i j | i <- [0..length xs - 1],
                                        j <- [i..length xs - 1]])

sublista :: [Integer] -> Int -> Int -> [Integer]
sublista xs i j =
    [xs!!k | k <- [i..j]]

-- 2ª definición
-- =============

sumaMaxima2 :: [Integer] -> Integer
sumaMaxima2 [] = 0
sumaMaxima2 xs 
    | m <= 0    = m 
    | otherwise = sumaMaximaAux 0 0 xs
    where m = maximum xs

sumaMaximaAux :: Integer -> Integer -> [Integer] -> Integer
sumaMaximaAux m v [] = 
    max m v
sumaMaximaAux m v (x:xs)
    | x >= 0    = sumaMaximaAux m (v+x) xs
    | v+x > 0   = sumaMaximaAux (max m v) (v+x) xs
    | otherwise = sumaMaximaAux (max m v) 0 xs

-- 3ª definición
-- =============

sumaMaxima3 :: [Integer] -> Integer
sumaMaxima3 [] = 0
sumaMaxima3 xs = maximum (map sum (segmentos xs))

-- (segmentos xs) es la lista de los segmentos de xs. Por ejemplo 
--    segmentos "abc"  ==  ["a","ab","abc","b","bc","c"]
segmentos :: [a] -> [[a]]
segmentos xs =
    concat [tail (inits ys) | ys <- init (tails xs)]

-- 4ª definición
-- =============

sumaMaxima4 :: [Integer] -> Integer
sumaMaxima4 [] = 0
sumaMaxima4 xs = 
    maximum (concat [scanl1 (+) ys | ys <- tails xs])

-- Comprobación
-- ============

-- La propiedad es
prop_sumaMaxima :: [Integer] -> Bool
prop_sumaMaxima xs =
    sumaMaxima2 xs == n &&
    sumaMaxima3 xs == n &&
    sumaMaxima4 xs == n
    where n = sumaMaxima1 xs

-- La comprobación es
--    ghci> quickCheck prop_sumaMaxima
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

--    ghci> let n = 10^2 in sumaMaxima1 [-n..n] 
--    5050
--    (2.10 secs, 390,399,104 bytes)
--    ghci> let n = 10^2 in sumaMaxima2 [-n..n] 
--    5050
--    (0.02 secs, 0 bytes)
--    ghci> let n = 10^2 in sumaMaxima3 [-n..n] 
--    5050
--    (0.27 secs, 147,705,184 bytes)
--    ghci> let n = 10^2 in sumaMaxima4 [-n..n] 
--    5050
--    (0.04 secs, 11,582,520 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Una matriz de tipo par-nula es una matriz en la que
-- todas las posiciones cuya suma de índices es par tienen un valor
-- 0. Por ejemplo,
--
--        ( 0 3 0 )
--        ( 5 0 2 )
--
--        ( 0 3 )
--        ( 5 0 )
--        ( 0 4 )
--
--        ( 0 3 0 )
--        ( 5 0 2 )
--        ( 0 4 0 )
--
-- Representaremos las matrices mediante tablas de dos dimensiones cuyos 
-- índices son pares de números naturales comenzando desde 1.
-- 
-- Definir la función 
--    matrizParNula :: (Eq a,Num a) => Matriz a -> Bool
-- tal que (matrizParNula m) se verifica si la matriz m es de tipo
-- par-nula. Por ejemplo,
--    matrizParNula (listArray ((1,1),(2,3)) [0,3,0,5,0,2])        ==  True
--    matrizParNula (listArray ((1,1),(2,3)) [0,3,5,0,0,4])        ==  False
--    matrizParNula (listArray ((1,1),(3,2)) [0,3,5,0,0,4])        ==  True
--    matrizParNula (listArray ((1,1),(3,2)) [0,3,0,5,0,2])        ==  False
--    matrizParNula (listArray ((1,1),(3,3)) [0,3,0,5,0,2,0,4,0])  ==  True
-- ---------------------------------------------------------------------

type Matriz a = Array (Int,Int) a

matrizParNula :: (Eq a,Num a) => Matriz a -> Bool
matrizParNula m =
    all (== 0) [m!(i,j) | i <- [1..p], j <- [1..q], even (i+j)]
    where (p,q) = snd (bounds m)

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función
--    matrizParNulaDistancias :: Int -> Int -> Matriz Int
-- tal que (matrizParNulaDistancias p q) es la matriz de tipo par-nula que en
-- las posiciones no nulas tiene la distancia (número de casillas contando la
-- inicial) más corta desde dicha posición hasta uno de los bordes de la propia
-- matriz. Por ejemplo,
--    matrizParNulaDistancias 3 3  =>
--       ( 0 1 0 )
--       ( 1 0 1 )
--       ( 0 1 0 )
--    matrizParNulaDistancias 3 5  =>
--       ( 0 1 0 1 0 )
--       ( 1 0 2 0 1 )
--       ( 0 1 0 1 0 )
--    matrizParNulaDistancias 5 5  =>
--       ( 0 1 0 1 0 )
--       ( 1 0 2 0 1 )
--       ( 0 2 0 2 0 )
--       ( 1 0 2 0 1 )
--       ( 0 1 0 1 0 )
-- ---------------------------------------------------------------------

matrizParNulaDistancias :: Int -> Int -> Matriz Int
matrizParNulaDistancias p q =
    array ((1,1),(p,q))
          [((i,j),if odd (i+j)
                  then minimum [i,j,p+1-i,q+1-j]
                  else 0) | i <- [1..p], j <- [1..q]]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    permutacionesM :: (Eq a) => [a] -> [[a]]
-- tal que (permutacionesM xs) es la lista de las permutaciones de la
-- lista xs sin elementos consecutivos iguales. Por ejemplo,
--    permutacionesM "abab"   ==  ["abab","baba"]
--    permutacionesM "abc"    ==  ["abc","acb","bac","bca","cab","cba"]
--    permutacionesM "abcab"  ==  ["abcab","abcba","abacb","ababc","acbab",
--                                 "bacab","bacba","babca","babac","bcaba",
--                                 "cabab","cbaba"]
-- ---------------------------------------------------------------------

permutacionesM :: (Eq a) => [a] -> [[a]]
permutacionesM [] = [[]]
permutacionesM xs =
    concat [map (x:) (filter (\ ys -> null ys || head ys /= x)
                             (permutacionesM (borra x xs))) | x <- nub xs]

borra :: (Eq a) => a -> [a] -> [a]
borra x [] = []
borra x (y:xs)
    | x == y = xs
    | otherwise = y : borra x xs

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Un árbol de Fibonacci de grado N es un árbol binario
-- construido de la siguiente forma:
-- + El árbol de Fibonacci de grado 0 es una hoja con dicho valor: 
--      (0)
-- + El árbol de Fibonacci de grado 1 es:  
--         (1)
--        /   \
--      (0)   (0)
-- + El árbol de Fibonacci de grado N (> 1) es el árbol que en su raíz
--   tiene el valor N, su hijo izquierdo es el árbol de Fibonacci de
--   grado (N-2) y su hijo derecho es el árbol de Fibonacci de grado
--   (N-1). 
-- De esta forma, el árbol de Fibonacci de grado 2 es:
--         (2)
--        /   \
--      (0)   (1)
--           /   \
--         (0)   (0)
-- el árbol de Fibonacci de grado 3 es:
--               (3)
--            __/   \__
--           /         \
--         (1)         (2)
--        /   \       /   \
--      (0)   (0)   (0)   (1)
--                       /   \
--                     (0)   (0)
-- y el árbol de Fibonacci de grado 4 es:
--                      (4)
--               ______/   \______
--              /                 \
--            (2)                 (3)
--           /   \             __/   \__
--         (0)   (1)          /         \
--              /   \       (1)         (2)
--            (0)   (0)    /   \       /   \
--                       (0)   (0)   (0)   (1)
--                                        /   \
--                                      (0)   (0)
--
-- Definir la función
--    esArbolFib :: Arbol -> Bool
-- tal que (esArbolFib a) comprueba si el árbol a es un árbol de Fibonacci.
-- Por ejemplo,
--    esArbolFib (H 0)                          ==  True
--    esArbolFib (H 1)                          ==  False
--    esArbolFib (N 1 (H 0) (H 0))              ==  True
--    esArbolFib (N 2 (H 0) (H 1))              ==  False
--    esArbolFib (N 2 (H 0) (N 1 (H 0) (H 0)))  ==  True 
--    esArbolFib (N 3 (H 0) (N 1 (H 0) (H 0)))  ==  False
-- ---------------------------------------------------------------------

data Arbol = H Int
           | N Int Arbol Arbol
           deriving (Show, Eq)

raiz :: Arbol -> Int
raiz (H n) = n
raiz (N n _ _) = n

esArbolFib :: Arbol -> Bool
esArbolFib (H 0) = True
esArbolFib (N 1 (H 0) (H 0)) = True
esArbolFib (N n ai ad) =
    (n > 1) && raiz ai == n-2 && raiz ad == n-1 && 
    esArbolFib ai && esArbolFib ad
esArbolFib _ = False

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la constante
--    arbolesFib :: [Arbol]
-- tal que (arbolesFib) es la lista infinita que en la posición N tiene
-- el árbol de Fibonacci de grado N. Por ejemplo,
--    arbolesFib!!0  ==  H 0
--    arbolesFib!!1  ==  N 1 (H 0) (H 0)
--    arbolesFib!!2  ==  N 2 (H 0) (N 1 (H 0) (H 0))
--    arbolesFib!!3  ==  N 3 (N 1 (H 0) (H 0)) (N 2 (H 0) (N 1 (H 0) (H 0)))
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

arbolFib1 :: Int -> Arbol
arbolFib1 0 = H 0
arbolFib1 1 = N 1 (H 0) (H 0)
arbolFib1 n = N n (arbolFib1 (n-2)) (arbolFib1 (n-1))

arbolesFib1 :: [Arbol]
arbolesFib1 =
    [arbolFib1 k | k <- [0..]]

-- 2ª solución
-- ===========

arbolesFib2 :: [Arbol]
arbolesFib2 =
    H 0 : 
    N 1 (H 0) (H 0) : 
    zipWith3 N [2..] arbolesFib2 (tail arbolesFib2)

