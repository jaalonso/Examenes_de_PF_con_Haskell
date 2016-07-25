-- Informática (1º del Grado en Matemáticas, Grupo 2)
-- 4º examen de evaluación continua (1 de marzo de 2011)
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    verificaP :: (a -> Bool) -> [[a]] -> Bool
-- tal que (verificaP p xs) se verifica si cada elemento de la lista xss
-- contiene algún elemento que cumple el predicado p. Por ejemplo,
--    verificaP odd [[1,3,4,2], [4,5], [9]] == True
--    verificaP odd [[1,3,4,2], [4,8], [9]] == False
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
verificaP :: (a -> Bool) -> [[a]] -> Bool
verificaP p xss = and [any p xs | xs <- xss]

-- 2ª definición (por recursión):
verificaP2 :: (a -> Bool) -> [[a]] -> Bool
verificaP2 p []       = True
verificaP2 p (xs:xss) = any p xs && verificaP2 p xss

-- 3ª definición (por plegado):
verificaP3 :: (a -> Bool) -> [[a]] -> Bool
verificaP3 p = foldr ((&&) . any p) True

-- ---------------------------------------------------------------------
-- Ejercicio 2. Se consideran los árboles binarios
-- definidos por  
--    data Arbol = H Int 
--               | N Arbol Int Arbol
--               deriving (Show, Eq)
-- Por ejemplo, el árbol
--         5 
--        / \
--       /   \
--      9     7
--     / \   / \  
--    1   4 6   8  
-- se representa por
--    N (N (H 1) 9 (H 4)) 5 (N (H 6) 7 (H 8))
-- 
-- Definir la función
--    mapArbol :: (Int -> Int) -> Arbol -> Arbol
-- tal que (mapArbol f a) es el árbol que resulta de aplicarle f a los
-- nodos y las hojas de a. Por ejemplo,
--    ghci> mapArbol (^2) (N (N (H 1) 9 (H 4)) 5 (N (H 6) 7 (H 8)))
--    N (N (H 1) 81 (H 16)) 25 (N (H 36) 49 (H 64))
-- ---------------------------------------------------------------------

data Arbol = H Int 
           | N Arbol Int Arbol
           deriving (Show, Eq)

mapArbol :: (Int -> Int) -> Arbol -> Arbol
mapArbol f (H x)     = H (f x)
mapArbol f (N i x d) = N (mapArbol f i) (f x) (mapArbol f d)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    separaSegunP :: (a -> Bool) -> [a] -> [[a]]
-- tal que (separaSegunP p xs) es la lista obtenida separando los
-- elementos de xs en segmentos según que verifiquen o no el predicado
-- p. Por jemplo,
--    ghci> separaSegunP odd [1,2,3,4,5,6,7,8]
--    [[1],[2],[3],[4],[5],[6],[7],[8]]
--    ghci> separaSegunP odd [1,1,3,4,6,7,8,10]
--    [[1,1,3],[4,6],[7],[8,10]]
-- ---------------------------------------------------------------------

separaSegunP :: (a -> Bool) -> [a] -> [[a]]
separaSegunP p [] = []
separaSegunP p xs = 
    takeWhile p xs : separaSegunP (not . p) (dropWhile p xs)

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Un número poligonal es un número que puede
-- recomponerse en un polígono regular. 
-- Los números triangulares (1, 3, 6, 10, 15, ...) son enteros del tipo  
--    1 + 2 + 3 + ... + n.
-- Los números cuadrados (1, 4, 9, 16, 25, ...) son enteros del tipo 
--    1 + 3 + 5 + ... + (2n-1).
-- Los números pentagonales (1, 5, 12, 22, ...) son enteros del tipo
--    1 + 4 + 7 + ... + (3n-2). 
-- Los números hexagonales (1, 6, 15, 28, ...) son enteros del tipo 
--    1 + 5 + 9 + ... + (4n-3). 
-- Y así sucesivamente. 
-- 
-- Según Fermat, todo número natural se puede expresar como la suma de n 
-- números poligonales de n lados. Gauss lo demostró para los
-- triangulares y Cauchy para todo tipo de polígonos. 
-- 
-- Para este ejercicio, decimos que un número poligonal de razón n es
-- un número del tipo 
--    1 + (1+n) + (1+2*n)+...
-- Es decir, los números triángulares son números poligonales de razón
-- 1, los números cuadrados son números poligonales de razón 2, los
-- pentagonales de razón 3, etc.
-- 
-- Definir la constante 
--    triangulares :: [Integer]
-- tal que es la lista de todos los números triángulares. Por ejemplo,
--    ghci> take 20 triangulares
--    [1,3,6,10,15,21,28,36,45,55,66,78,91,105,120,136,153,171,190,210]
-- ---------------------------------------------------------------------

triangulares :: [Integer]
triangulares = [sum [1..k] | k <- [1..]]

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función
--    esTriangular :: Integer -> Bool
-- tal que (esTriangular n) se verifica si n es un número es triangular. 
-- Por ejemplo, 
--    esTriangular 253  == True
--    esTriangular 234  == False
-- ---------------------------------------------------------------------

esTriangular :: Integer -> Bool
esTriangular x = x `elem` takeWhile (<=x) triangulares

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Definir la función 
--    poligonales:: Integer -> [Integer]
-- tal que (poligonales n) es la lista de los números poligonales de
-- razón n. Por ejemplo,
--    take 10 (poligonales 1) == [1,3,6,10,15,21,28,36,45,55]
--    take 10 (poligonales 3) == [1,5,12,22,35,51,70,92,117,145]
-- ---------------------------------------------------------------------

poligonales:: Integer -> [Integer]
poligonales n = [sum [1+j*n | j <- [0..k]] | k <- [0..]]

-- ---------------------------------------------------------------------
-- Ejercicio 4.4. Definir la función
--    esPoligonalN :: Integer -> Integer -> Bool
-- tal que (esPoligonalN x n) se verifica si x es poligonal de razón n. 
-- Por ejemplo,
--    esPoligonalN 12 3  ==  True
--    esPoligonalN 12 1  ==  False
-- ---------------------------------------------------------------------

esPoligonalN :: Integer -> Integer -> Bool
esPoligonalN x n = x `elem` takeWhile (<= x) (poligonales n)

-- ---------------------------------------------------------------------
-- Ejercicio 4.5. Definir la función
--    esPoligonal :: Integer -> Bool
-- tal que (esPoligonalN x) se verifica si x es un número poligonal. Por
-- ejemplo, 
--    esPoligonal 12  ==  True
-- ---------------------------------------------------------------------

esPoligonal :: Integer -> Bool
esPoligonal x = or [esPoligonalN x n | n <- [1..x]]

-- ---------------------------------------------------------------------
-- Ejercicio 4.6. Calcular el primer número natural no poligonal.
-- ---------------------------------------------------------------------

primerNoPoligonal :: Integer
primerNoPoligonal = head [x | x <- [1..], not (esPoligonal x)]

-- El cálculo es
--    ghci> primerNoPoligonal
--    2

-- ---------------------------------------------------------------------
-- Ejercicio 4.7. Definir la función
--    descomposicionTriangular :: Integer -> (Integer, Integer, Integer) 
-- tal que que (descomposicionTriangular n) es la descomposición de un
-- número natural en la suma de, a lo sumo 3 números triángulares. Por
-- ejemplo, 
--    descomposicionTriangular 20  == (0,10,10)
--    descomposicionTriangular 206 == (1,15,190)
--    descomposicionTriangular 6   == (0,0,6)
--    descomposicionTriangular 679 == (1,300,378)          
-- ---------------------------------------------------------------------

descomposicionTriangular :: Integer -> (Integer, Integer, Integer) 
descomposicionTriangular n =         
  head [(x,y,z) | x <- xs, 
                  y <- x : dropWhile (<x) xs, 
                  z <- y : dropWhile (<y) xs, 
                  x+y+z == n]
    where xs = 0 : takeWhile (<=n) triangulares
