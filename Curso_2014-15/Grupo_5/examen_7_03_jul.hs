-- Informática (1º del Grado en Matemáticas)
-- Examen de julio (3 de julio de 2015)
-- ---------------------------------------------------------------------

import Data.List
import Data.Matrix
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    minimales :: Eq a => [[a]] -> [[a]]
-- tal que (minimales xss) es la lista de los elementos de xss que no
-- están contenidos en otros elementos de xss. Por ejemplo,
--    minimales [[1,3],[2,3,1],[3,2,5]]        ==  [[2,3,1],[3,2,5]]
--    minimales [[1,3],[2,3,1],[3,2,5],[3,1]]  ==  [[2,3,1],[3,2,5]]
-- ---------------------------------------------------------------------

minimales :: Eq a => [[a]] -> [[a]]
minimales xss = 
    [xs | xs <- xss, null [ys | ys <- xss, subconjuntoPropio xs ys]]

-- (subconjuntoPropio xs ys) se verifica si xs es un subconjunto propio
-- de ys. Por ejemplo, 
--    subconjuntoPropio [1,3] [3,1,3]    ==  False
--    subconjuntoPropio [1,3,1] [3,1,2]  ==  True
subconjuntoPropio :: Eq a => [a] -> [a] -> Bool
subconjuntoPropio xs ys = subconjuntoPropio' (nub xs) (nub ys)
    where
      subconjuntoPropio' _xs [] = False
      subconjuntoPropio' [] _ys = True
      subconjuntoPropio' (x:xs) ys = 
          x `elem` ys && subconjuntoPropio xs (delete x ys)  

-- ---------------------------------------------------------------------
-- Ejercicio 2. Un mínimo local de una lista es un elemento de la lista
-- que es menor que su predecesor y que su sucesor en la lista. Por
-- ejemplo, 1 es un mínimo local de [8,2,1,3,7,6,4,0,5] ya que es menor
-- que 2 (su predecesor) y que 3 (su sucesor).  
--
-- Análogamente se definen los máximos locales. Por ejemplo, 7 es un
-- máximo local de [8,2,1,3,7,6,4,0,5] ya que es mayor que 7 (su
-- predecesor) y que 6 (su sucesor).
--
-- Los extremos locales están formados por los mínimos y máximos
-- locales. Por ejemplo, los extremos locales de [8,2,1,3,7,6,4,0,5] son 
-- el 1, el 7 y el 0.
-- 
-- Definir la función
--    extremos :: Ord a => [a] -> [a]
-- tal que (extremos xs) es la lista de los extremos locales de la
-- lista xs. Por ejemplo,
--    extremos [8,2,1,3,7,6,4,0,5]  ==  [1,7,0]
--    extremos [8,2,1,3,7,7,4,0,5]  ==  [1,7,0]
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión)
-- ===============================
extremos1 :: Ord a => [a] -> [a]
extremos1 xs = 
    [y | (x,y,z) <- zip3 xs (tail xs) (drop 2 xs), extremo x y z]

-- (extremo x y z) se verifica si y es un extremo local de [x,y,z]. Por
-- ejemplo,
--    extremo 2 1 3  ==  True
--    extremo 3 7 6  ==  True
--    extremo 7 6 4  ==  False
--    extremo 5 6 7  ==  False
--    extremo 5 5 7  ==  False
extremo :: Ord a => a -> a -> a -> Bool
extremo x y z = (y < x && y < z) || (y > x && y > z)

-- 2ª definición (por recursión)
-- =============================

extremos2 :: Ord a => [a] -> [a]
extremos2 (x:y:z:xs) 
    | extremo x y z = y : extremos2 (y:z:xs)
    | otherwise     = extremos2 (y:z:xs)
extremos2 _ = []

-- ---------------------------------------------------------------------
-- Ejercicio 3. Los árboles, con un número variable de hijos, se pueden
-- representar mediante el siguiente tipo de dato 
--    data Arbol a = N a [Arbol a]
--                   deriving Show
-- Por ejemplo, los árboles
--      1               3
--     / \             /|\ 
--    6   3           / | \
--        |          5  4  7
--        5          |     /\ 
--                   6    2  1
-- se representan por
--    ej1, ej2 :: Arbol Int
--    ej1 = N 1 [N 6 [],N 3 [N 5 []]]
--    ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]
-- 
-- Definir la función
--    emparejaArboles :: (a -> b -> c) -> Arbol a -> Arbol b -> Arbol c
-- tal que (emparejaArboles f a1 a2) es el árbol obtenido aplicando la
-- función f a los elementos de los árboles a1 y a2 que se encuentran en
-- la misma posición. Por ejemplo,
--    ghci> emparejaArboles (+) (N 1 [N 2 [], N 3[]]) (N 1 [N 6 []])
--    N 2 [N 8 []]
--    ghci> emparejaArboles (+) ej1 ej2
--    N 4 [N 11 [],N 7 []]
--    ghci> emparejaArboles (+) ej1 ej1
--    N 2 [N 12 [],N 6 [N 10 []]]
-- ---------------------------------------------------------------------

data Arbol a = N a [Arbol a]
               deriving (Show, Eq)

emparejaArboles :: (a -> b -> c) -> Arbol a -> Arbol b -> Arbol c
emparejaArboles f (N x l1) (N y l2) = 
    N (f x y) (zipWith (emparejaArboles f) l1 l2)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la lista
--    antecesoresYsucesores :: [[Integer]]       
-- cuyos elementos son
--    [[1],[0,2],[-1,1,1,3],[-2,2,0,0,2,0,2,2,4],...]
-- donde cada una de las listas se obtiene de la anterior sustituyendo
-- cada elemento por su antecesor y su sucesor; es decir, el 1 por el 0
-- y el 2, el 0 por el -1 y el 1, el 2 por el 1 y el 3, etc. Por
-- ejemplo, 
--    ghci> take 4 antecesoresYsucesores
--    [[1],[0,2],[-1,1,1,3],[-2,0,0,2,0,2,2,4]]
--
-- Comprobar con Quickcheck que la suma de los elementos de la lista
-- n-ésima de antecesoresYsucesores es 2^n.
--
-- Nota. Limitar la búsqueda a ejemplos pequeños usando
--    quickCheckWith (stdArgs {maxSize=7}) prop_suma
-- ---------------------------------------------------------------------

-- 1ª solución
antecesoresYsucesores :: [[Integer]]       
antecesoresYsucesores = 
    [1] : map (concatMap (\x -> [x-1,x+1])) antecesoresYsucesores

-- 2ª solución
antecesoresYsucesores2 :: [[Integer]]       
antecesoresYsucesores2 = 
    iterate (concatMap (\x -> [x-1,x+1])) [1]

-- La propiedad es
prop_suma :: Int -> Property
prop_suma n =
    n >= 0 ==> sum (antecesoresYsucesores2 !! n) == 2^n

-- La comprobación es
--    ghci> quickCheckWith (stdArgs {maxSize=7}) prop_suma
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Los grafos no dirigidos puede representarse mediante
-- matrices de adyacencia y también mediante listas de adyacencia. Por
-- ejemplo, el grafo 
--    1 ----- 2
--    | \     |
--    |  3    |
--    | /     |
--    4 ----- 5
-- se puede representar por la matriz de adyacencia 
--    |0 1 1 1 0|
--    |1 0 0 0 1|
--    |1 0 0 1 0|
--    |1 0 1 0 1|
--    |0 1 0 1 0|
-- donde el elemento (i,j) es 1 si hay una arista entre los vértices i y
-- j y es 0 si no la hay. También se puede representar por la lista de
-- adyacencia 
--    [(1,[2,3,4]),(2,[1,5]),(3,[1,4]),(4,[1,3,5]),(5,[2,4])]   
-- donde las primeras componentes son los vértices y las segundas
-- la lista de los vértices conectados. 
-- 
-- Definir la función
--    matrizAlista :: Matrix Int -> [(Int,[Int])]
-- tal que (matrizAlista a) es la lista de adyacencia correspondiente a
-- la matriz de adyacencia a. Por ejemplo, definiendo la matriz anterior
-- por
--    ejMatriz :: Matrix Int
--    ejMatriz = fromLists [[0,1,1,1,0],
--                          [1,0,0,0,1],
--                          [1,0,0,1,0],
--                          [1,0,1,0,1],
--                          [0,1,0,1,0]]
-- se tiene que 
--    ghci> matrizAlista ejMatriz
--    [(1,[2,3,4]),(2,[1,5]),(3,[1,4]),(4,[1,3,5]),(5,[2,4])]
-- ---------------------------------------------------------------------

ejMatriz :: Matrix Int
ejMatriz = fromLists [[0,1,1,1,0],
                      [1,0,0,0,1],
                      [1,0,0,1,0],
                      [1,0,1,0,1],
                      [0,1,0,1,0]]

matrizAlista :: Matrix Int -> [(Int,[Int])]
matrizAlista a = 
    [(i,[j | j <- [1..n], a!(i,j) == 1]) | i <- [1..n]]
    where n = nrows a

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir la función
--    listaAmatriz :: [(Int,[Int])] -> Matrix Int
-- tal que (listaAmatriz ps) es la matriz de adyacencia correspondiente
-- a la lista de adyacencia ps. Por ejemplo, 
--    ghci> listaAmatriz [(1,[2,3,4]),(2,[1,5]),(3,[1,4]),(4,[1,3,5]),(5,[2,4])]
--    ( 0 1 1 1 0 )
--    ( 1 0 0 0 1 )
--    ( 1 0 0 1 0 )
--    ( 1 0 1 0 1 )
--    ( 0 1 0 1 0 )
--    ghci> matrizAlista it
--    [(1,[2,3,4]),(2,[1,5]),(3,[1,4]),(4,[1,3,5]),(5,[2,4])]
-- ---------------------------------------------------------------------

listaAmatriz :: [(Int,[Int])] -> Matrix Int
listaAmatriz ps = fromLists [fila n xs | (_,xs) <- sort ps]
    where n = length ps
          fila n xs = [f i | i <- [1..n]]
              where f i | i `elem` xs = 1
                        | otherwise   = 0
