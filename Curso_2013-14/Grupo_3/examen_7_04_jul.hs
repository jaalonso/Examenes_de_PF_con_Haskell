-- Informática (1º del Grado en Matemáticas)
-- Examen de la 1ª convocatoria (4 de julio de 2014)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1. [2 puntos] Una lista de longitud n > 0 es completa si el
-- valor absoluto de las diferencias de sus elementos consecutivos toma
-- todos los valores entre 1 y n-1 (sólo una vez). Por ejemplo,
-- [4,1,2,4] es completa porque los valores absolutos de las diferencias
-- de sus elementos consecutivos es [3,1,2]. 
--
-- Definir la función
--    esCompleta :: [Int] -> Bool
-- tal que (esCompleta xs) se verifica si xs es completa. Por ejemplo,
--    esCompleta [4,1,2,4]  ==  True
--    esCompleta [6]        ==  True
--    esCompleta [6,7]      ==  True
--    esCompleta [6,8]      ==  False
--    esCompleta [6,7,9]    ==  True
--    esCompleta [8,7,5]    ==  True
-- ---------------------------------------------------------------------

esCompleta :: [Int] -> Bool
esCompleta xs = 
    sort [abs (x-y) | (x,y) <- zip xs (tail xs)] == [1..length xs - 1]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función 
--    unionG :: Ord a => [[a]] -> [a]
-- tal que (unionG xss) es la unión de xss cuyos elementos son listas
-- estrictamente crecientes (posiblemente infinitas). Por ejemplo,
--    ghci> take 10 (unionG [[2,4..],[3,6..],[5,10..]])
--    [2,3,4,5,6,8,9,10,12,14]
--    ghci> take 10 (unionG [[2,5..],[3,8..],[4,10..],[16..]])
--    [2,3,4,5,8,10,11,13,14,16]
--    ghci> unionG [[3,8],[4,10],[2,5],[16]]
--    [2,3,4,5,8,10,16]
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión)
-- =============================
unionG1 :: Ord a => [[a]] -> [a]
unionG1 []          = []
unionG1 [xs]        = xs
unionG1 (xs:ys:zss) = xs `unionB` unionG1 (ys:zss)

unionB :: Ord a => [a] -> [a] -> [a]
unionB [] ys = ys
unionB xs [] = xs
unionB (x:xs) (y:ys) | x < y     = x : unionB xs (y:ys)
                     | x > y     = y : unionB (x:xs) ys
                     | otherwise = x : unionB xs ys

-- 2ª definición (por plegado)
-- ===========================
unionG2 :: Ord a => [[a]] -> [a]
unionG2 = foldr unionB []

-- ---------------------------------------------------------------------
-- Ejercicio 3. [2 puntos] Definir la función
--    noEsSuma :: [Integer] -> Integer
-- tal que (noEsSuma xs) es el menor entero positivo que no se puede
-- expresar como suma de elementos de la lista creciente de números
-- positivos xs (ningún elemento se puede usar más de una vez). Por
-- ejemplo, 
--    noEsSuma [1,2,3,8]      ==  7
--    noEsSuma (1:[2,4..10])  ==  32
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========
noEsSuma1 :: [Integer] -> Integer
noEsSuma1 xs = head [n | n <- [1..], n `notElem` sumas1 xs]

-- (sumas1 xs) es la lista de las sumas con los elementos de xs, donde
-- cada elemento se puede sumar como máximo una vez. Por ejemplo,
--    sumas1 [3,8]      ==  [11,3,8,0]
--    sumas1 [3,8,17]   ==  [28,11,20,3,25,8,17,0]
--    sumas1 [1,2,3,8]  ==  [14,6,11,3,12,4,9,1,13,5,10,2,11,3,8,0]
sumas1 :: [Integer] -> [Integer]
sumas1 [] = [0]
sumas1 (x:xs) = [x+y | y <- ys] ++ ys
    where ys = sumas1 xs

-- 2ª solución
-- ===========
noEsSuma2 :: [Integer] -> Integer
noEsSuma2 xs = head [n | n <- [1..], not (esSuma n xs)]

esSuma :: Integer -> [Integer] -> Bool
esSuma n [] = n == 0
esSuma n (x:xs) | n < x     = False
                | n == x    = True
                | otherwise = esSuma (n-x) xs || esSuma n xs

-- 3ª solución
-- ===========
noEsSuma3 :: [Integer] -> Integer
noEsSuma3 xs = aux xs 0
    where aux [] n     = n+1
          aux (x:xs) n | x <= n+1  = aux xs (n+x)
                       | otherwise = n+1

-- Comparaciones de eficiencia
-- ===========================

-- Las comparaciones son
--    ghci> noEsSuma1 ([1..10]++[12..20])
--    200
--    (8.28 secs, 946961604 bytes)
--    ghci> noEsSuma2 ([1..10]++[12..20])
--    200
--    (2.52 secs, 204156056 bytes)
--    ghci> noEsSuma3 ([1..10]++[12..20])
--    200
--    (0.01 secs, 520348 bytes)
--
--    ghci> noEsSuma2 (1:[2,4..30])
--    242
--    (4.97 secs, 399205788 bytes)
--    ghci> noEsSuma3 (1:[2,4..30])
--    242
--    (0.01 secs, 514340 bytes)
-- 
--    ghci> noEsSuma3 (1:[2,4..2014])
--    1015058
--    (0.01 secs, 1063600 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 4. [2 puntos] Los divisores medios de un número son los que
-- ocupan la posición media entre los divisores de n, ordenados de menor
-- a mayor. Por ejemplo, los divisores de 60 son
-- [1,2,3,4,5,6,10,12,15,20,30,60] y sus divisores medios son 6 y 10.
-- 
-- El árbol de factorización de un número compuesto n se construye de la
-- siguiente manera: 
--    * la raíz es el número n, 
--    * la rama izquierda es el árbol de factorización de su divisor
--      medio menor y
--    * la rama derecha es el árbol de factorización de su divisor
--      medio mayor
-- Si el número es primo, su árbol de factorización sólo tiene una hoja
-- con dicho número. Por ejemplo, el árbol de factorización de 60 es
--        60
--       /  \
--      6    10
--     / \   / \
--    2   3 2   5
--
-- Los árboles se representarán por
--    data Arbol = H Int
--               | N Int Arbol Arbol
--               deriving Show
--
-- Definir la función
--    arbolFactorizacion :: Int -> Arbol
-- tal que (arbolFactorizacion n) es el árbol de factorización de n. Por
-- ejemplo, 
--    ghci> arbolFactorizacion 60
--    N 60 (N 6 (H 2) (H 3)) (N 10 (H 2) (H 5))
--    ghci> arbolFactorizacion 45
--    N 45 (H 5) (N 9 (H 3) (H 3))
--    ghci> arbolFactorizacion 7
--    H 7
--    ghci> arbolFactorizacion 14
--    N 14 (H 2) (H 7)
--    ghci> arbolFactorizacion 28
--    N 28 (N 4 (H 2) (H 2)) (H 7)
--    ghci> arbolFactorizacion 84
--    N 84 (H 7) (N 12 (H 3) (N 4 (H 2) (H 2)))
-- ---------------------------------------------------------------------

data Arbol = H Int
           | N Int Arbol Arbol
           deriving Show

-- 1ª definición
-- =============
arbolFactorizacion :: Int -> Arbol
arbolFactorizacion n 
    | esPrimo n = H n
    | otherwise = N n (arbolFactorizacion x) (arbolFactorizacion y)
    where (x,y) = divisoresMedio n

-- (esPrimo n) se verifica si n es primo. Por ejemplo,
--    esPrimo 7  ==  True
--    esPrimo 9  ==  False
esPrimo :: Int -> Bool
esPrimo n = divisores n == [1,n]

-- (divisoresMedio n) es el par formado por los divisores medios de
-- n. Por ejemplo,
--    divisoresMedio 30  ==  (5,6)
--    divisoresMedio  7  ==  (1,7)
divisoresMedio :: Int -> (Int,Int)
divisoresMedio n = (n `div` x,x)
    where xs = divisores n
          x  = xs !! (length xs `div` 2)

-- (divisores n) es la lista de los divisores de n. Por ejemplo,
--    divisores 30  ==  [1,2,3,5,6,10,15,30]
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `rem` x == 0]

-- 2ª definición
-- =============
arbolFactorizacion2 :: Int -> Arbol
arbolFactorizacion2 n
    | x == 1    = H n
    | otherwise = N n (arbolFactorizacion x) (arbolFactorizacion y)
    where (x,y) = divisoresMedio n

-- (divisoresMedio2 n) es el par formado por los divisores medios de
-- n. Por ejemplo,
--    divisoresMedio2 30  ==  (5,6)
--    divisoresMedio2  7  ==  (1,7)
divisoresMedio2 :: Int -> (Int,Int)
divisoresMedio2 n = (n `div` x,x)
    where m  = ceiling (sqrt (fromIntegral n))
          x = head [y | y <- [m..n], n `rem` y == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 5. [2 puntos] El triángulo de Pascal es un triángulo de
-- números 
--          1
--         1 1
--        1 2 1
--      1  3 3  1
--     1 4  6  4 1
--    1 5 10 10 5 1
--   ...............
-- construido de la siguiente forma
-- * la primera fila está formada por el número 1;
-- * las filas siguientes se construyen sumando los números adyacentes
--   de la fila superior y añadiendo un 1 al principio y al final de la
--   fila. 
--
-- La matriz de Pascal es la matriz cuyas filas son los elementos de la
-- correspondiente fila del triángulo de Pascal completadas con
-- ceros. Por ejemplo, la matriz de Pascal de orden 6 es
--    |1 0  0  0 0 0|
--    |1 1  0  0 0 0|
--    |1 2  1  0 0 0|
--    |1 3  3  1 0 0|
--    |1 4  6  4 1 0|
--    |1 5 10 10 5 1|
-- 
-- Las matrices se definen mediante el tipo
--    type Matriz = Array (Int,Int) Int
--
-- Definir la función
--    matrizPascal :: Int -> Matriz 
-- tal que (matrizPascal n) es la matriz de Pascal de orden n. Por
-- ejemplo, 
--    ghci> matrizPascal 5
--    array ((1,1),(5,5)) 
--          [((1,1),1),((1,2),0),((1,3),0),((1,4),0),((1,5),0),
--           ((2,1),1),((2,2),1),((2,3),0),((2,4),0),((2,5),0),
--           ((3,1),1),((3,2),2),((3,3),1),((3,4),0),((3,5),0),
--           ((4,1),1),((4,2),3),((4,3),3),((4,4),1),((4,5),0),
--           ((5,1),1),((5,2),4),((5,3),6),((5,4),4),((5,5),1)]
-- ---------------------------------------------------------------------

type Matriz = Array (Int,Int) Int

-- 1ª solución
-- ===========

matrizPascal1 :: Int -> Matriz 
matrizPascal1 1 = array ((1,1),(1,1)) [((1,1),1)]
matrizPascal1 n = 
    array ((1,1),(n,n)) [((i,j), f i j) | i <- [1..n], j <- [1..n]]
    where f i j | i < n && j <  n  = p!(i,j)
                | i < n && j == n  = 0
                | j == 1 || j == n = 1
                | otherwise        = p!(i-1,j-1) + p!(i-1,j)
          p = matrizPascal2 (n-1)

-- 2ª solución
-- ===========

matrizPascal2 :: Int -> Matriz
matrizPascal2 n = listArray ((1,1),(n,n)) (concat xss)
    where yss = take n pascal
          xss = map (take n) (map (++ (repeat 0)) yss)
        
pascal :: [[Int]]
pascal = [1] : map f pascal
    where f xs = zipWith (+) (0:xs) (xs++[0])


-- 2ª solución
-- ===========

matrizPascal3 :: Int -> Matriz
matrizPascal3 n = 
    array ((1,1),(n,n)) [((i,j), f i j) | i <- [1..n], j <- [1..n]]
    where f i j | i >=  j   = comb (i-1) (j-1)
                | otherwise = 0

-- (comb n k) es el número de combinaciones (o coeficiente binomial) de
-- n sobre k. Por ejemplo,
comb :: Int -> Int -> Int
comb n k = product [n,n-1..n-k+1] `div` product [1..k]
