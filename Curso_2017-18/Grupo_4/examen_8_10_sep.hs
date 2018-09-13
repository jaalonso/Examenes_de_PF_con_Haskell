-- Informática (1º del Grado en Matemáticas)
-- Examen de la 2ª convocatoria (10 de septiembre de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Matrix as M
import Data.Numbers.Primes 
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Un número de Hilbert es un entero positivo de la forma
-- 4n+1. Los primeros números de Hilbert son 1, 5, 9, 13, 17, 21, 25,
-- 29, 33, 37, 41, 45, 49, 53, 57, 61, 65, 69, ... 
--
-- Un primo de Hilbert es un número de Hilbert n que no es divisible
-- por ningún número de Hilbert menor que n (salvo el 1). Los primeros
-- primos de Hilbert son 5, 9, 13, 17, 21, 29, 33, 37, 41, 49, 53, 57,
-- 61, 69, 73, 77, 89, 93, 97, 101, 109, 113, 121, 129, 133, 137, ...
--
-- Definir la función
--    factorizacionesH :: Integer -> [[Integer]]
-- tal que (factorizacionesH n) es la lista de listas de primos de
-- Hilbert tales que el producto de los elementos de cada una de las
-- listas es el número de Hilbert n. Por ejemplo,
--   factorizacionesH  25  ==  [[5,5]]
--   factorizacionesH  45  ==  [[5,9]]
--   factorizacionesH 441  ==  [[9,49],[21,21]]
--
-- Comprobar con QuickCheck que todos los números de Hilbert son
-- factorizables como producto de primos de Hilbert (aunque la
-- factorización, como para el 441, puede no ser única).
-- ---------------------------------------------------------------------

factorizacionesH :: Integer -> [[Integer]]
factorizacionesH n = aux n primosH
  where aux n (x:xs)
          | x == n         = [[n]]
          | x > n          = []
          | n `mod` x == 0 = map (x:) (aux (n `div` x) (x:xs))
                             ++ aux n xs
          | otherwise      = aux n xs

-- primosH es la sucesión de primos de Hilbert. Por ejemplo,
--    take 15 primosH  ==  [5,9,13,17,21,29,33,37,41,49,53,57,61,69,73]
primosH :: [Integer]
primosH = filter esPrimoH (tail numerosH) 
  where esPrimoH n = all noDivideAn [5,9..m]
          where noDivideAn x = n `mod` x /= 0
                m            = ceiling (sqrt (fromIntegral n))

-- numerosH es la sucesión de los números de Hilbert. Por ejemplo,
--    take 15 numerosH  ==  [1,5,9,13,17,21,25,29,33,37,41,45,49,53,57]
numerosH :: [Integer]
numerosH = [1,5..]

-- La propiedad es
prop_factorizable :: Integer -> Property
prop_factorizable n =
  n > 0 ==> not (null (factorizacionesH (1 + 4 * n)))

-- La comprobación es
--    λ> quickCheck prop_factorizable
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2. Se llaman potencias de Fermi-Dirac los números de la
-- forma p^(2^k), donde p es un número primo y k es un número natural.
--
-- Definir la sucesión
--    potencias :: [Integer]
-- cuyos términos sean las potencias de Fermi-Dirac ordenadas de menor a
-- mayor. Por ejemplo,
--    take 14 potencias    ==  [2,3,4,5,7,9,11,13,16,17,19,23,25,29]
--    potencias !! 60      ==  241
--    potencias !! (10^6)  ==  15476303
-- ---------------------------------------------------------------------

potencias :: [Integer]
potencias = 2 : mezcla (tail primes) (map (^2) potencias)

-- (mezcla xs ys) es la lista obtenida mezclando las dos listas xs e ys,
-- que se suponen ordenadas y disjuntas. Por ejemplo,
--    ghci> take 15 (mezcla [2^n | n <- [1..]] [3^n | n <- [1..]])
--    [2,3,4,8,9,16,27,32,64,81,128,243,256,512,729]
mezcla :: Ord a => [a] -> [a] -> [a]
mezcla (x:xs) (y:ys) | x < y = x : mezcla xs (y:ys)
                     | x > y = y : mezcla (x:xs) ys

-- ---------------------------------------------------------------------
-- Ejercicio 3. Los árboles binarios con valores en las hojas y en los
-- nodos se definen por   
--    data Arbol a = H a
--                 | N a (Arbol a) (Arbol a) 
--      deriving (Eq, Show)
-- 
-- Por ejemplo, el árbol
--          10
--         /  \
--        /    \
--       8      1
--      / \    / \
--     3   9  2   6
-- se pueden representar por
--    ejArbol :: Arbol Int
--    ejArbol = N 10 (N 8 (H 3) (H 9))
--                   (N 1 (H 2) (H 6))
--
-- La distancia entre un padre y un hijo en el árbol es el valor
-- absoluto de la diferencia de sus valores. Por ejemplo, la distancia
-- de 10 a 8 es 2 y de 1 a 6 es 5.
--
-- Definir la función
--    maximaDistancia :: (Num a, Ord a) => Arbol a -> a
-- tal que (maximaDistancia a) es la máxima distancia entre un padre y
-- un hijo del árbol a. Por ejemplo,
--    maximaDistancia ejArbol                                     ==  9
--    maximaDistancia (N 1 (N 8 (H 3) (H 9)) (N 1  (H 2) (H 6)))  ==  7
--    maximaDistancia (N 8 (N 8 (H 3) (H 9)) (N 10 (H 2) (H 6)))  ==  8
-- ---------------------------------------------------------------------

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
  deriving Show

ejArbol :: Arbol Int
ejArbol = N 10 (N 8 (H 3) (H 9))
               (N 1 (H 2) (H 6))

maximaDistancia :: (Num a, Ord a) => Arbol a -> a
maximaDistancia (H _)     = 0
maximaDistancia (N x i d) = maximum [abs (x - raiz i)
                                    , maximaDistancia i
                                    , abs (x - raiz d)
                                    , maximaDistancia d]

raiz :: Arbol a -> a
raiz (H x)     = x
raiz (N x _ _) = x

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    mayor :: (Num a, Ord a) => Int -> Matrix a -> [[a]]
-- tal que (mayor n p) es la lista de los segmentos formados por n
-- elementos adyacentes en la misma fila, columna, diagonal principal o
-- diagonal secundaria de la matriz p cuyo productos son máximo. Por
-- ejemplo, 
--    mayor 3 (fromList 3 4 [1..12])                      == [[10,11,12]]
--    mayor 3 (fromLists [[1,3,4,5],[0,7,2,1],[3,9,2,1]]) == [[3,7,9]]
--    mayor 2 (fromLists [[1,3,4],[0,3,2]])               == [[3,4],[4,3]]
--    mayor 2 (fromLists [[1,2,1],[3,0,3]])               == [[2,3],[2,3]]
--    mayor 2 (fromLists [[1,2,1],[3,4,3]])               == [[3,4],[4,3]]
--    mayor 2 (fromLists [[1,5,1],[3,4,3]])               == [[5,4]]
--    mayor 3 (fromLists [[1,3,4,5],[0,7,2,1],[3,9,2,1]]) == [[3,7,9]]
-- ---------------------------------------------------------------------

mayor :: (Num a, Ord a) => Int -> Matrix a -> [[a]]
mayor n p = 
  [xs | xs <- segmentos, product xs == m]
  where segmentos = adyacentes n p 
        m         = maximum (map product segmentos)

-- (adyacentes n p) es la lista de los segmentos de longitud n de las
-- líneas de la matriz p. Por ejemplo,
--    ghci> adyacentes 3 (fromList 3 4 [1..12])
--    [[1,2,3],[2,3,4],[5,6,7],[6,7,8],[9,10,11],[10,11,12],[1,5,9],
--     [2,6,10],[3,7,11],[4,8,12],[1,6,11],[2,7,12],[3,6,9],[4,7,10]]
adyacentes :: Num a => Int -> Matrix a -> [[a]]
adyacentes n p = concatMap (segmentos n) (lineas p)

-- (lineas p) es la lista de las líneas de la matriz p. Por ejemplo, 
--    ghci> lineas (fromList 3 4 [1..12])
--    [[1,2,3,4],[5,6,7,8],[9,10,11,12],
--     [1,5,9],[2,6,10],[3,7,11],[4,8,12],
--     [9],[5,10],[1,6,11],[2,7,12],[3,8],[4],
--     [1],[2,5],[3,6,9],[4,7,10],[8,11],[12]]
lineas :: Num a => Matrix a -> [[a]]
lineas p = filas p ++ 
           columnas p ++ 
           diagonalesPrincipales p ++
           diagonalesSecundarias p

-- (filas p) es la lista de las filas de la matriz p. Por ejemplo,
--    ghci> filas (fromList 3 4 [1..12])
--    [[1,2,3,4],[5,6,7,8],[9,10,11,12]]
filas :: Num a => Matrix a -> [[a]]
filas = toLists 

-- (columnas p) es la lista de las columnas de la matriz p. Por ejemplo,
--    ghci> columnas (fromList 3 4 [1..12])
--    [[1,5,9],[2,6,10],[3,7,11],[4,8,12]]
columnas :: Num a => Matrix a -> [[a]]
columnas = toLists . M.transpose

-- (diagonalesPrincipales p) es la lista de las diagonales principales
-- de p. Por ejemplo, 
--    ghci> diagonalesPrincipales (fromList 3 4 [1..12])
--    [[9],[5,10],[1,6,11],[2,7,12],[3,8],[4]]
diagonalesPrincipales :: Matrix a -> [[a]]
diagonalesPrincipales p = 
  [[p!ij1 | ij1 <- extension ij] | ij <- iniciales] 
  where m = nrows p
        n = ncols p
        iniciales = [(i,1) | i <- [m,m-1..2]] ++ [(1,j) | j <- [1..n]] 
        extension (i,j) = [(i+k,j+k) | k <- [0..min (m-i) (n-j)]]

-- (diagonalesSecundarias p) es la lista de las diagonales secundarias
-- de p. Por ejemplo, 
--    ghci> diagonalesSecundarias (fromList 3 4 [1..12])
--    [[1],[2,5],[3,6,9],[4,7,10],[8,11],[12]]
diagonalesSecundarias p = 
  [[p!ij1 | ij1 <- extension ij] | ij <- iniciales]
  where m = nrows p
        n = ncols p
        iniciales = [(1,j) | j <- [1..n]] ++ [(i,n) | i <- [2..m]] 
        extension (i,j) = [(i+k,j-k) | k <- [0..min (j-1) (m-i)]]

-- (segmentos n xs) es la lista de los segmentos de longitud n de la
-- lista xs. Por ejemplo,
--    segmentos 2 [3,5,4,6]  ==  [[3,5],[5,4],[4,6]]
segmentos :: Int -> [a] -> [[a]]
segmentos n xs = take (length xs - n + 1) (map (take n) (tails xs))

-- Se puede definir por recursión
segmentos2 :: Int -> [a] -> [[a]]
segmentos2 n xs 
  | length xs < n = []
  | otherwise     = take n xs : segmentos2 n (tail xs)

