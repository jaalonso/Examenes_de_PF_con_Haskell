-- Informática (1º del Grado en Matemáticas)
-- 7º examen de evaluación continua (3 de julio de 2013)
-- ---------------------------------------------------------------------

import Data.List
import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1. [2 puntos] Dos listas son cíclicamente iguales si tienen
-- el mismo número de elementos en el mismo orden. Por ejemplo, son
-- cíclicamente iguales los siguientes pares de listas
--    [1,2,3,4,5] y [3,4,5,1,2],
--    [1,1,1,2,2] y [2,1,1,1,2],
--    [1,1,1,1,1] y [1,1,1,1,1]
-- pero no lo son
--    [1,2,3,4] y [1,2,3,5],
--    [1,1,1,1] y [1,1,1],
--    [1,2,2,1] y [2,2,1,2]
-- Definir la función 
--    iguales :: Eq a => [a] -> [a] -> Bool
-- tal que (iguales xs ys) se verifica si xs es ys son cíclicamente
-- iguales. Por ejemplo,
--    iguales [1,2,3,4,5] [3,4,5,1,2]  ==  True
--    iguales [1,1,1,2,2] [2,1,1,1,2]  ==  True
--    iguales [1,1,1,1,1] [1,1,1,1,1]  ==  True
--    iguales [1,2,3,4] [1,2,3,5]      ==  False
--    iguales [1,1,1,1] [1,1,1]        ==  False
--    iguales [1,2,2,1] [2,2,1,2]      ==  False
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

iguales1 :: Ord a => [a] -> [a] -> Bool
iguales1 xs ys = 
    permutacionApares xs == permutacionApares ys

-- (permutacionApares xs) es la lista ordenada de los pares de elementos
-- consecutivos de elementos de xs. Por ejemplo,
--    permutacionApares [2,1,3,5,4]  ==  [(1,3),(2,1),(3,5),(4,2),(5,4)]
permutacionApares :: Ord a => [a] -> [(a, a)]
permutacionApares xs = 
    sort (zip xs (tail xs) ++ [(last xs, head xs)])

-- 2ª solucion
-- ===========

-- (iguales2 xs ys) se verifica si las listas xs e ys son cíclicamente
-- iguales. Por ejemplo,
iguales2 :: Eq a => [a] -> [a] -> Bool
iguales2 xs ys = 
    elem ys (ciclos xs)

-- (ciclo xs) es la lista obtenida pasando el último elemento de xs al
-- principio. Por ejemplo,
--    ciclo [2,1,3,5,4]  ==  [4,2,1,3,5]
ciclo :: [a] -> [a]
ciclo xs = (last xs): (init xs)

-- (kciclo k xs) es la lista obtenida pasando los k últimos elementos de
-- xs al principio. Por ejemplo, 
--    kciclo 2 [2,1,3,5,4]  ==  [5,4,2,1,3]
kciclo :: (Eq a, Num a) => a -> [a1] -> [a1]
kciclo 1 xs = ciclo xs
kciclo k xs = kciclo (k-1) (ciclo xs)

-- (ciclos xs) es la lista de las listas cíclicamente iguales a xs. Por
-- ejemplo, 
--    ghci> ciclos [2,1,3,5,4]
--    [[4,2,1,3,5],[5,4,2,1,3],[3,5,4,2,1],[1,3,5,4,2],[2,1,3,5,4]]
ciclos :: [a] -> [[a]]
ciclos xs = [kciclo k xs | k <- [1..length xs]]

-- 3º solución
-- ===========

iguales3 :: Eq a => [a] -> [a] -> Bool
iguales3 xs ys = 
    length xs == length ys && isInfixOf xs (ys ++ ys)

-- ---------------------------------------------------------------------
-- Ejercicio ?. Un número natural n es casero respecto de f si las
-- cifras de f(n) es una sublista de las de n. Por ejemplo,
-- * 1234 es casero repecto de resto de dividir por 173, ya que el resto
--   de dividir 1234 entre 173 es 23 que es una sublista de 1234; 
-- * 1148 es casero respecto de la suma de cifras, ya que la suma de las
--   cifras de 1148 es 14 que es una sublista de 1148.
-- Definir la función
--    esCasero :: (Integer -> Integer) -> Integer -> Bool
-- tal que (esCasero f x) se verifica si x es casero respecto de f. Por
-- ejemplo,
--    esCasero (\x -> rem x 173) 1234  ==  True
--    esCasero (\x -> rem x 173) 1148  ==  False
--    esCasero sumaCifras 1148         ==  True
--    esCasero sumaCifras 1234         ==  False
-- donde (sumaCifras n) es la suma de las cifras de n.
--
-- ¿Cuál es el menor número casero respecto de la suma de cifras mayor
-- que 2013? 
-- ---------------------------------------------------------------------

esCasero :: (Integer -> Integer) -> Integer -> Bool
esCasero f x =
    esSublista (cifras (f x)) (cifras x)

-- (esSublista xs ys) se verifica si xs es una sublista de ys; es decir,
-- si existen dos listas as y bs tales que 
--    ys = as ++ xs ++ bs
esSublista :: Eq a => [a] -> [a] -> Bool
esSublista = isInfixOf

-- Se puede definir por
esSublista2 :: Eq a => [a] -> [a] -> Bool
esSublista2 xs ys = 
    or [esPrefijo xs zs | zs <- sufijos ys]

-- (esPrefijo xs ys) se verifica si xs es un prefijo de ys. Por 
-- ejemplo,
--    esPrefijo "ab" "abc"  ==  True
--    esPrefijo "ac" "abc"  ==  False
--    esPrefijo "bc" "abc"  ==  False
esPrefijo :: Eq a => [a] -> [a] -> Bool
esPrefijo [] _          = True
esPrefijo _  []         = False
esPrefijo (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- (sufijos xs) es la lista de sufijos de xs. Por ejemplo,
--    sufijos "abc"  ==  ["abc","bc","c",""]
sufijos :: [a] -> [[a]]
sufijos xs = [drop i xs | i <- [0..length xs]]

-- (cifras x) es la lista de las cifras de x. Por ejemplo,
--    cifras 325  ==  [3,2,5]
cifras :: Integer -> [Integer]
cifras x = [read [d] | d <- show x]

-- (sumaCifras x) es la suma de las cifras de x. Por ejemplo,
--    sumaCifras 325  ==  10
sumaCifras :: Integer -> Integer
sumaCifras = sum . cifras

-- El cálculo del menor número casero respecto de la suma mayor que 2013
-- es 
--    ghci> head [n | n <- [2014..], esCasero sumaCifras n]
--    2099

-- ---------------------------------------------------------------------
-- Ejercicio 3. [2 puntos] Definir la función
--    interseccion :: Ord a => [a] -> [a] -> [a]
-- tal que (interseccion xs ys) es la intersección de las dos listas,
-- posiblemente infinitas, ordenadas de menor a mayor xs e ys. Por ejemplo,
--    take 5 (interseccion [2,4..] [3,6..])  ==  [6,12,18,24,30]
-- ---------------------------------------------------------------------

interseccion :: Ord a => [a] -> [a] -> [a]
interseccion [] _ = []
interseccion _ [] = []
interseccion (x:xs) (y:ys)
    | x == y    = x : interseccion xs ys
    | x < y     = interseccion (dropWhile (<y) xs) (y:ys)
    | otherwise = interseccion (x:xs) (dropWhile (<x) ys)

-- ---------------------------------------------------------------------
-- Ejercicio 4. [2 puntos] Los árboles binarios se pueden representar
-- mediante el tipo Arbol definido por 
--    data Arbol = H Int 
--               | N Int Arbol Arbol
-- Por ejemplo, el árbol
--         1
--        / \ 
--       /   \
--      2     5
--     / \   / \
--    3   4 6   7
-- se puede representar por 
--    N 1 (N 2 (H 3) (H 4)) (N 5 (H 6) (H 7))
-- Definir la función
--    esSubarbol :: Arbol -> Arbol -> Bool
-- tal que (esSubarbol a1 a2) se verifica si a1 es un subárbol de
-- a2. Por ejemplo,
--    esSubarbol (H 2) (N 2 (H 2) (H 4))              ==  True
--    esSubarbol (H 5) (N 2 (H 2) (H 4))              ==  False
--    esSubarbol (N 2 (H 2) (H 4)) (N 2 (H 2) (H 4))  ==  True
--    esSubarbol (N 2 (H 4) (H 2)) (N 2 (H 2) (H 4))  ==  False
-- ---------------------------------------------------------------------

data Arbol= H Int
          | N Int Arbol Arbol

esSubarbol :: Arbol -> Arbol -> Bool
esSubarbol (H x) (H y) = x == y
esSubarbol a@(H x) (N y i d) = esSubarbol a i || esSubarbol a d
esSubarbol (N _ _ _) (H _) = False
esSubarbol a@(N r1 i1 d1) (N r2 i2 d2) 
    | r1 == r2  = (igualArbol i1 i2 && igualArbol d1 d2) ||
                  esSubarbol a i2 || esSubarbol a d2
    | otherwise = esSubarbol a i2 || esSubarbol a d2

-- (igualArbol a1 a2) se verifica si los árboles a1 y a2 son iguales.
igualArbol :: Arbol -> Arbol -> Bool
igualArbol (H x) (H y) = x == y
igualArbol (N r1 i1 d1) (N r2 i2 d2) = 
    r1 == r2 && igualArbol i1 i2 && igualArbol d1 d2
igualArbol _ _ = False

-- ---------------------------------------------------------------------
-- Ejercicio 5. [2 puntos] Las matrices enteras se pueden representar
-- mediante tablas con índices enteros:
--    type Matriz = Array (Int,Int) Int
-- Por ejemplo, las matrices          
--    | 1  2  3  4  5 |    | 1 2 3 |
--    | 2  6  8  9  4 |    | 2 6 8 |
--    | 3  8  0  8  3 |    | 3 8 0 |
--    | 4  9  8  6  2 |
--    | 5  4  3  2  1 |
-- se puede definir por
--    ejM1, ejM2 :: Matriz
--    ejM1 = listArray ((1,1),(5,5)) [1,2,3,4,5,
--                                    2,6,8,9,4,
--                                    3,8,0,8,3,
--                                    4,9,8,6,2,
--                                    5,4,3,2,1]
--    
--    ejM2 = listArray ((1,1),(3,3)) [1,2,3,
--                                    2,6,8,
--                                    3,8,0]
-- Una matriz cuadrada es bisimétrica si es simétrica respecto de su
-- diagonal principal y de su diagonal secundaria. Definir la función
--    esBisimetrica :: Matriz -> Bool
-- tal que (esBisimetrica p) se verifica si p es bisimétrica. Por
-- ejemplo,           
--    esBisimetrica ejM1  ==  True
--    esBisimetrica ejM2  ==  False
-- ---------------------------------------------------------------------

type Matriz = Array (Int,Int) Int

ejM1, ejM2 :: Matriz
ejM1 = listArray ((1,1),(5,5)) [1,2,3,4,5,
                                2,6,8,9,4,
                                3,8,0,8,3,
                                4,9,8,6,2,
                                5,4,3,2,1]

ejM2 = listArray ((1,1),(3,3)) [1,2,3,
                                2,6,8,
                                3,8,0]

-- 1ª definición:
esBisimetrica :: Matriz -> Bool
esBisimetrica p =
    and [p!(i,j) == p!(j,i) | i <- [1..n], j <- [1..n]] &&
    and [p!(i,j) == p!(n+1-j,n+1-i) | i <- [1..n], j <- [1..n]]
    where ((_,_),(n,_)) = bounds p

-- 2ª definición:
esBisimetrica2 :: Matriz -> Bool
esBisimetrica2 p = p == simetrica p && p == simetricaS p
        
-- (simetrica p) es la simétrica de la matriz p respecto de la diagonal
-- principal. Por ejemplo,
--    ghci> simetrica (listArray ((1,1),(4,4)) [1..16])
--    array ((1,1),(4,4)) [((1,1),1),((1,2),5),((1,3), 9),((1,4),13),
--                         ((2,1),2),((2,2),6),((2,3),10),((2,4),14),
--                         ((3,1),3),((3,2),7),((3,3),11),((3,4),15),
--                         ((4,1),4),((4,2),8),((4,3),12),((4,4),16)]
simetrica :: Matriz -> Matriz
simetrica p =
    array ((1,1),(n,n)) [((i,j),p!(j,i)) | i <- [1..n], j <- [1..n]]
    where ((_,_),(n,_)) = bounds p

-- (simetricaS p) es la simétrica de la matriz p respecto de la diagonal
-- secundaria. Por ejemplo,
--    ghci> simetricaS (listArray ((1,1),(4,4)) [1..16])
--    array ((1,1),(4,4)) [((1,1),16),((1,2),12),((1,3),8),((1,4),4),
--                         ((2,1),15),((2,2),11),((2,3),7),((2,4),3),
--                         ((3,1),14),((3,2),10),((3,3),6),((3,4),2),
--                         ((4,1),13),((4,2), 9),((4,3),5),((4,4),1)]
simetricaS :: Matriz -> Matriz
simetricaS p =
    array ((1,1),(n,n)) [((i,j),p!(n+1-j,n+1-i)) | i <- [1..n], j <- [1..n]]
    where ((_,_),(n,_)) = bounds p
