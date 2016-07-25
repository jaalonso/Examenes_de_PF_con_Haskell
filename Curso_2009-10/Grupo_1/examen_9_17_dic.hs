-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- Examen de la 3ª convocatoria (17 de diciembre de 2010)
-- ---------------------------------------------------------------------

import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    ullman :: (Num a, Ord a) => a -> Int -> [a] -> Bool
-- tal que (ullman t k xs) se verifica si xs tiene un subconjunto con k 
-- elementos cuya suma sea menor que t. Por ejemplo,
--    ullman 9 3 [1..10] == True
--    ullman 5 3 [1..10] == False
-- ---------------------------------------------------------------------

-- 1ª solución (corta y eficiente)
ullman :: (Ord a, Num a) => a -> Int -> [a] -> Bool
ullman t k xs = sum (take k (sort xs)) < t

-- 2ª solución (larga e ineficiente)
ullman2 :: (Num a, Ord a) => a -> Int -> [a] -> Bool
ullman2 t k xs = 
    [ys | ys <- subconjuntos xs, length ys == k, sum ys < t] /= []

-- (subconjuntos xs) es la lista de los subconjuntos de xs. Por 
-- ejemplo,
--    subconjuntos "bc"  ==  ["","c","b","bc"]
--    subconjuntos "abc" ==  ["","c","b","bc","a","ac","ab","abc"]
subconjuntos :: [a] -> [[a]]
subconjuntos [] = [[]]
subconjuntos (x:xs) = zss++[x:ys | ys <- zss]
    where zss = subconjuntos xs

-- Los siguientes ejemplos muestran la diferencia en la eficencia:
--    ghci> ullman 9 3 [1..20]
--    True
--    (0.02 secs, 528380 bytes)
--    ghci> ullman2 9 3 [1..20]
--    True
--    (4.08 secs, 135267904 bytes)
--    ghci> ullman 9 3 [1..100]
--    True
--    (0.02 secs, 526360 bytes)
--    ghci> ullman2 9 3 [1..100]
--      C-c C-cInterrupted.
--    Agotado

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    sumasDe2Cuadrados :: Integer -> [(Integer, Integer)]
-- tal que (sumasDe2Cuadrados n) es la lista de los pares de números
-- tales que la suma de sus cuadrados es n y el primer elemento del par
-- es mayor o igual que el segundo. Por ejemplo,
--    sumasDe2Cuadrados 25  ==  [(5,0),(4,3)]
-- ---------------------------------------------------------------------

-- 1ª definición:
sumasDe2Cuadrados_1 :: Integer -> [(Integer, Integer)]
sumasDe2Cuadrados_1 n = 
    [(x,y) | x <- [n,n-1..0],
             y <- [0..x],
             x*x+y*y == n]

-- 2ª definición:
sumasDe2Cuadrados2 :: Integer -> [(Integer, Integer)]
sumasDe2Cuadrado_2 n = 
    [(x,y) | x <- [a,a-1..0],
             y <- [0..x],
             x*x+y*y == n]
    where a = ceiling (sqrt (fromIntegral n))

-- 3ª definición:
sumasDe2Cuadrados3 :: Integer -> [(Integer, Integer)]
sumasDe2Cuadrado_3 n = aux (ceiling (sqrt (fromIntegral n))) 0 
    where aux x y | x < y          = [] 
                  | x*x + y*y <  n = aux x (y+1)
                  | x*x + y*y == n = (x,y) : aux (x-1) (y+1)
                  | otherwise      = aux (x-1) y

-- Comparación
--    +----------+---------------+---------------+---------------+
--    | n        | 1ª definición | 2ª definición | 3ª definición |
--    +----------+---------------+---------------+---------------+
--    |      999 | 2.17 segs     |   0.02 segs   | 0.01 segs     |  
--    | 48612265 |               | 140.38 segs   | 0.13 segs     |
--    +----------+---------------+---------------+---------------+

-- ---------------------------------------------------------------------
-- Ejercicio 3. Los árboles binarios pueden representarse mediante el
-- tipo de datos Arbol definido por 
--    data Arbol a = Nodo (Arbol a) (Arbol a) 
--                 | Hoja a
--                 deriving Show
-- Por ejemplo, los árboles 
--    árbol1          árbol2       árbol3     árbol4 
--       o              o           o	        o    
--      / \            / \         / \	       / \   
--     1   o          o   3       o   3	      o   1  
--        / \        / \         / \	     / \     
--       2   3      1   2       1   4	    2   3    
-- se representan por
--    arbol1, arbol2, arbol3, arbol4 :: Arbol Int
--    arbol1 = Nodo (Hoja 1) (Nodo (Hoja 2) (Hoja 3))
--    arbol2 = Nodo (Nodo (Hoja 1) (Hoja 2)) (Hoja 3)
--    arbol3 = Nodo (Nodo (Hoja 1) (Hoja 4)) (Hoja 3)
--    arbol4 = Nodo (Nodo (Hoja 2) (Hoja 3)) (Hoja 1)
-- Definir la función
--    igualBorde :: Eq a => Arbol a -> Arbol a -> Bool
-- tal que (igualBorde t1 t2) se verifica si los bordes de los árboles
-- t1 y t2 son iguales. Por ejemplo,
--    igualBorde arbol1 arbol2  ==  True
--    igualBorde arbol1 arbol3  ==  False
--    igualBorde arbol1 arbol4  ==  False
-- ---------------------------------------------------------------------

data Arbol a = Nodo (Arbol a) (Arbol a) 
             | Hoja a
             deriving Show

arbol1, arbol2, arbol3, arbol4 :: Arbol Int
arbol1 = Nodo (Hoja 1) (Nodo (Hoja 2) (Hoja 3))
arbol2 = Nodo (Nodo (Hoja 1) (Hoja 2)) (Hoja 3)
arbol3 = Nodo (Nodo (Hoja 1) (Hoja 4)) (Hoja 3)
arbol4 = Nodo (Nodo (Hoja 2) (Hoja 3)) (Hoja 1)

igualBorde :: Eq a => Arbol a -> Arbol a -> Bool
igualBorde t1 t2 = borde t1 == borde t2

-- (borde t) es el borde del árbol t; es decir, la lista de las hojas
-- del árbol t leídas de izquierda a derecha. Por ejemplo, 
--    borde arbol4  ==  [2,3,1]
borde :: Arbol a -> [a]
borde (Nodo i d) = borde i ++ borde d
borde (Hoja x)   = [x]

-- ---------------------------------------------------------------------
-- Ejercicio 4. (Basado en el problema 145 del Proyecto Euler). 
-- Se dice que un número n es reversible si su última cifra es
-- distinta de 0 y la suma de n y el número obtenido escribiendo las
-- cifras de n en orden inverso es un número que tiene todas sus cifras
-- impares. Por ejemplo, 
-- * 36  es reversible porque 36+63=99 tiene todas sus cifras impares, 
-- * 409 es reversible porque 409+904=1313 tiene todas sus cifras
--       impares,   
-- * 243 no es reversible porque 243+342=585 no tiene todas sus cifras
--       impares.
--   
-- Definir la función 
--    reversiblesMenores :: Int -> Int
-- tal que (reversiblesMenores n) es la cantidad de números reversibles
-- menores que n. Por ejemplo,
--    reversiblesMenores 10   == 0
--    reversiblesMenores 100  == 20
--    reversiblesMenores 1000 == 120
-- ---------------------------------------------------------------------

-- (reversiblesMenores n) es la cantidad de números reversibles menores
-- que n. Por ejemplo,
--    reversiblesMenores 10   == 0
--    reversiblesMenores 100  == 20
--    reversiblesMenores 1000 == 120
reversiblesMenores :: Int -> Int
reversiblesMenores n = length [x | x <- [1..n-1], esReversible x]

-- (esReversible n) se verifica si n es reversible; es decir, si su
-- última cifra es distinta de 0 y la suma de n y el número obtenido
-- escribiendo las cifras de n en orden inverso es un número que tiene
-- todas sus cifras impares. Por ejemplo,
--    esReversible 36  == True
--    esReversible 409 == True
esReversible :: Int -> Bool
esReversible n = rem n 10 /= 0 && impares (cifras (n + (inverso n)))

-- (impares xs) se verifica si xs es una lista de números impares. Por
-- ejemplo, 
--    impares [3,5,1] == True
--    impares [3,4,1] == False
impares :: [Int] -> Bool
impares xs = and [odd x | x <- xs]

-- (inverso n) es el número obtenido escribiendo las cifras de n en
-- orden inverso. Por ejemplo,
--    inverso 3034 == 4303
inverso :: Int -> Int
inverso n = read (reverse (show n))

-- (cifras n) es la lista de las cifras del número n. Por ejemplo,
--    cifras 3034 == [3,0,3,4]
cifras :: Int -> [Int]
cifras n = [read [x] | x <- show n]
