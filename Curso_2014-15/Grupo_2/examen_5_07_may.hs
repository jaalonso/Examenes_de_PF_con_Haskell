-- Informática (1º del Grado en Matemáticas)
-- Informática: 5º examen de evaluación continua (7 de mayo de 2015)
-- ---------------------------------------------------------------------

import Data.Array
import Data.List
import Data.Numbers.Primes
import I1M.Grafo
import I1M.Monticulo

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Un número tiene una inversión cuando existe un dígito
-- x a la derecha de otro dígito de forma que x es menor que y. Por
-- ejemplo, en el número 1745 hay dos inversiones ya que 4 es menor
-- que 7 y 5 es menor que 7 y están a la derecha de 7.
-- 
-- Definir la función 
--    nInversiones :: Integer -> Int
-- tal que (nInversiones n) es el número de inversiones de n. Por
-- ejemplo, 
--    nInversiones 1745  ==  2
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

nInversiones1 :: Integer -> Int
nInversiones1 = length . inversiones . show

-- (inversiones xs) es la lista de las inversiones de xs. Por ejemplo, 
--    inversiones "1745"   ==  [('7','4'),('7','5')]
--    inversiones "cbafd"  ==  [('c','b'),('c','a'),('b','a'),('f','d')]
inversiones :: Ord a => [a] -> [(a,a)]
inversiones []     = []
inversiones (x:xs) = [(x,y) | y <- xs, y < x] ++ inversiones xs

-- 2ª definición
-- =============

nInversiones2 :: Integer -> Int
nInversiones2 = aux . show
    where aux [] = 0
          aux (y:ys) | null xs   = aux ys
                     | otherwise = length xs + aux ys
                     where xs = [x | x <- ys, x < y]  

-- 3ª solución
-- ===========

nInversiones3 :: Integer -> Int
nInversiones3 x = sum $ map f xss
    where xss = init $ tails (show x)
          f (x:xs) = length $ filter (<x) xs

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    ghci> let f1000 = product [1..1000] 
--    ghci> nInversiones1 f1000
--    1751225
--    (2.81 secs, 452526504 bytes)
--    ghci> nInversiones2 f1000
--    1751225
--    (2.45 secs, 312752672 bytes)
--    ghci> nInversiones3 f1000
--    1751225
--    (0.71 secs, 100315896 bytes)

-- En lo sucesivo, se usa la 3ª definición
nInversiones :: Integer -> Int
nInversiones = nInversiones3

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Calcular cuántos números hay de 4 cifras con más de
-- dos inversiones. 
-- ---------------------------------------------------------------------

-- El cálculo es 
--    ghci> length [x |x <- [1000 ..9999], nInversiones x > 2]
--    5370

-- ---------------------------------------------------------------------
-- Ejercicio 2. La notas de un examen se pueden representar mediante un
-- vector en el que los valores son los pares formados por los nombres
-- de los alumnos y sus notas.
-- 
-- Definir la función 
--    aprobados :: (Num a, Ord a) => Array Int (String,a) -> Maybe [String]
-- tal que (aprobados p) es la lista de los nombres de los alumnos que
-- han aprobado y Nothing si todos están suspensos. Por ejemplo,
--    ghci> aprobados (listArray (1,3) [("Ana",5),("Pedro",3),("Lucia",6)])
--    Just ["Ana","Lucia"]
--    ghci> aprobados (listArray (1,3) [("Ana",4),("Pedro",3),("Lucia",4.9)])
--    Nothing
-- ---------------------------------------------------------------------

aprobados :: (Num a, Ord a) => Array Int (String,a) -> Maybe [String]
aprobados p | null xs   = Nothing
            | otherwise = Just xs
    where xs = [x | i <- indices p
                  , let (x,n) = p!i
                  , n >= 5]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función
--    refina :: Ord a => Monticulo a -> [a -> Bool] -> Monticulo a 
-- tal que (refina m ps) es el formado por los elementos del montículo m
-- que cumplen todos predicados de la lista ps. Por ejemplo,
--    ghci> refina (foldr inserta vacio [1..22]) [(<7), even]
--    M 2 1 (M 4 1 (M 6 1 Vacio Vacio) Vacio) Vacio
--    ghci> refina (foldr inserta vacio [1..22]) [(<1), even]
--    Vacio
-- ---------------------------------------------------------------------

refina :: Ord a => Monticulo a -> [a-> Bool] -> Monticulo a 
refina m ps | esVacio m   = vacio
            | cumple x ps = inserta x (refina r ps)
            | otherwise   = refina r ps
            where x = menor m
                  r = resto m

-- (cumple x ps) se verifica si x cumple todos los predicados de ps. Por
-- ejemplo,  
--    cumple 2 [(<7), even]  ==  True
--    cumple 3 [(<7), even]  ==  False
--    cumple 8 [(<7), even]  ==  False
cumple :: a -> [a -> Bool] -> Bool
cumple x ps = and [p x | p <- ps]

-- La función `cumple`se puede definir por recursión:
cumple2 x []     = True
cumple2 x (p:ps) = p x && cumple x ps

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función 
--    diferencia :: Ord a => Monticulo a -> Monticulo a -> Monticulo a
-- tal que (diferencia m1 m2) es el montículo formado por los elementos
-- de m1 que no están en m2. Por ejemplo,
--    ghci> diferencia (foldr inserta vacio [7,5,6]) (foldr inserta vacio [4,5])
--    M 6 1 (M 7 1 Vacio Vacio) Vacio
-- ---------------------------------------------------------------------

diferencia :: Ord a => Monticulo a -> Monticulo a -> Monticulo a
diferencia m1 m2 
    | esVacio m1           = vacio
    | esVacio m2           = m1
    | menor m1 <  menor m2 = inserta (menor m1) (diferencia (resto m1) m2) 
    | menor m1 == menor m2 = diferencia (resto m1) (resto m2)
    | menor m1 >  menor m2 = diferencia m1 (resto m2)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Una matriz latina de orden n es una matriz cuadrada de
-- orden n tal que todos sus elementos son cero salvo los de su fila y
-- columna central, si n es impar; o los de sus dos filas y columnas
-- centrales, si n es par.
-- 
-- Definir la función
--    latina :: Int -> Array (Int,Int) Int
-- tal que (latina n) es la siguiente matriz latina de orden n:
-- + Para n impar:
--      | 0  0... 0 1   0 ... 0   0|
--      | 0  0... 0 2   0 ... 0   0|
--      | 0  0... 0 3   0 ... 0   0|
--      | .........................|
--      | 1  2..............n-1   n|
--      | .........................|
--      | 0  0... 0 n-2 0 ... 0   0|
--      | 0  0... 0 n-1 0 ... 0   0|
--      | 0  0... 0 n   0 ... 0   0|
-- + Para n par:
--      | 0  0... 0 1   n    0 ...   0   0|
--      | 0  0... 0 2   n-1  0 ...   0   0|
--      | 0  0... 0 3   n-2  0 ...   0   0|
--      | ................................|
--      | 1  2.....................n-1   n|
--      | n n-1 .................... 2   1|
--      | ................................|
--      | 0  0... 0 n-2  3   0 ...   0   0|
--      | 0  0... 0 n-1  2   0 ...   0   0|
--      | 0  0... 0 n    1   0 ...   0   0|
-- Por ejemplo,
--    ghci> elems (latina 5)
--    [0,0,1,0,0,
--     0,0,2,0,0,
--     1,2,3,4,5,
--     0,0,4,0,0,
--     0,0,5,0,0]
--    ghci> elems (latina 6)
--    [0,0,1,6,0,0,
--     0,0,2,5,0,0,
--     1,2,3,4,5,6,
--     6,5,4,3,2,1,
--     0,0,5,2,0,0,
--     0,0,6,1,0,0]
-- ----------------------------------------------------------------------------

latina :: Int -> Array (Int,Int) Int
latina n | even n    = latinaPar n
         | otherwise = latinaImpar n

-- (latinaImpar n) es la matriz latina de orden n, siendo n un número
-- impar. Por ejemplo,
--    ghci> elems (latinaImpar 5)
--    [0,0,1,0,0,
--     0,0,2,0,0,
--     1,2,3,4,5,
--     0,0,4,0,0,
--     0,0,5,0,0]
latinaImpar :: Int -> Array (Int,Int) Int
latinaImpar n = 
    array ((1,1),(n,n)) [((i,j),f i j) | i <- [1..n], j <- [1..n]]
    where c = 1 + (n `div` 2)
          f i j | i == c    = j
                | j == c    = i 
                | otherwise = 0 

-- (latinaPar n) es la matriz latina de orden n, siendo n un número
-- par. Por ejemplo,
--    ghci> elems (latinaPar 6)
--    [0,0,1,6,0,0,
--     0,0,2,5,0,0,
--     1,2,3,4,5,6,
--     6,5,4,3,2,1,
--     0,0,5,2,0,0,
--     0,0,6,1,0,0]
latinaPar :: Int -> Array (Int,Int) Int
latinaPar n = 
    array ((1,1),(n,n)) [((i,j),f i j) | i <- [1..n], j <- [1..n]]
    where c = n `div` 2
          f i j | i == c    = j
                | i == c+1  = n-j+1
                | j == c    = i
                | j == c+1  = n-i+1
                | otherwise = 0 

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir las funciones
--    grafo   :: [(Int,Int)] -> Grafo Int Int
--    caminos :: Grafo Int Int -> Int -> Int -> [[Int]]
-- tales que 
-- + (grafo as) es el grafo no dirigido definido cuyas aristas son as. Por
--   ejemplo, 
--      ghci> grafo [(2,4),(4,5)]
--      G ND (array (2,5) [(2,[(4,0)]),(3,[]),(4,[(2,0),(5,0)]),(5,[(4,0)])])
-- + (caminos g a b) es la lista los caminos en el grafo g desde a hasta
--   b sin pasar dos veces por el mismo nodo. Por ejemplo,
--      ghci> caminos (grafo [(1,3),(2,5),(3,5),(3,7),(5,7)]) 1 7
--      [[1,3,7],[1,3,5,7]]
--      ghci> caminos (grafo [(1,3),(2,5),(3,5),(3,7),(5,7)]) 2 7
--      [[2,5,7],[2,5,3,7]]
--      ghci> caminos (grafo [(1,3),(2,5),(3,5),(3,7),(5,7)]) 1 2
--      [[1,3,7,5,2],[1,3,5,2]]
--      ghci> caminos (grafo [(1,3),(2,5),(3,5),(3,7),(5,7)]) 1 4
--      []
-- ---------------------------------------------------------------------

grafo :: [(Int,Int)] -> Grafo Int Int
grafo as = creaGrafo ND (m,n) [(x,y,0) | (x,y) <- as]
    where ns = map fst as ++ map snd as
          m  = minimum ns
          n  = maximum ns

caminos :: Grafo Int Int -> Int -> Int -> [[Int]]
caminos g a b = reverse (aux b a [])
    where aux a b vs 
              | a == b    = [[b]]
              | otherwise = [b:xs | c <- adyacentes g b
                                  , c `notElem` vs
                                  , xs <- aux a c (c:vs)]

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función 
--    sumaDePrimos :: Int -> [[Int]]
-- tal que (sumaDePrimos x) es la lista de las listas no crecientes de
-- números primos que suman x. Por ejemplo: 
--    sumaDePrimos 10  ==  [[7,3],[5,5],[5,3,2],[3,3,2,2],[2,2,2,2,2]]
-- ---------------------------------------------------------------------

sumaDePrimos :: Integer -> [[Integer]]
sumaDePrimos 1 = []
sumaDePrimos n = aux n (reverse (takeWhile (<=n) primes))
    where aux _ [] = []
          aux n (x:xs) | x > n     = aux n xs
                       | x == n    = [n] : aux n xs
                       | otherwise = map (x:) (aux (n-x) (x:xs)) ++ aux n xs
