-- Informática (1º del Grado en Matemáticas, Grupo 2)
-- 4º examen de evaluación continua (12 de marzo de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías                                                        --
-- ---------------------------------------------------------------------

import Data.List
import Data.Array
-- import Data.Matrix as M
-- import Data.Vector

-- ---------------------------------------------------------------------
-- Ejercicio 1. Hay 3 números (el 2, 3 y 4) cuyos factoriales son
-- divisibles por 2 pero no por 5. Análogamente, hay números 5 (el 5, 6,
-- 7, 8, 9) cuyos factoriales son divisibles por 15 pero no por 25.
--
-- Definir la función 
--    nNumerosConFactorialesDivisibles :: Integer -> Integer -> Integer
-- tal que (nNumerosConFactorialesDivisibles x y) es la cantidad de
-- números cuyo factorial es divisible por x pero no  por y. Por ejemplo,
--   nNumerosConFactorialesDivisibles 2  5      ==  3
--   nNumerosConFactorialesDivisibles 15 25     ==  5
--   nNumerosConFactorialesDivisibles 100 2000  ==  5
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

nNumerosConFactorialesDivisibles :: Integer -> Integer -> Integer
nNumerosConFactorialesDivisibles x y =
  genericLength (numerosConFactorialesDivisibles x y)

-- (numerosConFactorialesDivisibles x y) es la lista de números
-- divisibles por el factorial de x pero no divisibles por el 
-- factorial de y. Por ejemplo,
--   numerosConFactorialesDivisibles 2  5   ==  [2,3,4]
--   numerosConFactorialesDivisibles 15 25  ==  [5,6,7,8,9]
numerosConFactorialesDivisibles :: Integer -> Integer -> [Integer]
numerosConFactorialesDivisibles x y =
  [z | z <- [0..y-1]
     , factorial z `mod` x == 0
     , factorial z `mod` y /= 0]

-- (factorial n) es el factorial de n. Por ejemplo, 
--   factorial 4  ==  24
factorial :: Integer -> Integer
factorial n = product [1..n]

-- 2ª solución (usando la función de Smarandache)
-- ==============================================

nNumerosConFactorialesDivisibles2 :: Integer -> Integer -> Integer
nNumerosConFactorialesDivisibles2 x y =
  max 0 (smarandache y - smarandache x)

--(smarandache n) es el menor número cuyo factorial es divisible por
-- n. Por ejemplo,   
--    smarandache 8   ==  4
--    smarandache 10  ==  5
--    smarandache 16  ==  6
smarandache :: Integer -> Integer
smarandache x =
  head [n | (n,y) <- zip [0..] factoriales
          , y `mod` x == 0]

-- factoriales es la lista de los factoriales. Por ejemplo, 
--    λ> take 12 factoriales
--    [1,1,2,6,24,120,720,5040,40320,362880,3628800,39916800]
factoriales :: [Integer]
factoriales = 1 : scanl1 (*) [1..]

-- Comparación de ediciencia
--    λ> nNumerosConFactorialesDivisibles 100 2000
--    5
--    (2.70 secs, 3,933,938,648 bytes)
--    λ> nNumerosConFactorialesDivisibles2 100 2000
--    5
--    (0.01 secs, 148,200 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Una matriz centro simétrica es una matriz cuadrada que
-- es simétrica respecto de su centro. Por ejemplo, de las siguientes
-- matrices, las dos primeras son simétricas y las otras no lo son
--    (1 2)   (1 2 3)   (1 2 3)   (1 2 3)    
--    (2 1)   (4 5 4)   (4 5 4)   (4 5 4)
--            (3 2 1)   (3 2 2)
--
-- Definir la función
--    esCentroSimetrica :: Eq t => Array (Int,Int) t -> Bool
-- tal que (esCentroSimetrica a) se verifica si la matriz a es centro
-- simétrica. Por ejemplo,
--    λ> esCentroSimetrica (listArray ((1,1),(2,2)) [1,2, 2,1])
--    True
--    λ> esCentroSimetrica (listArray ((1,1),(3,3)) [1,2,3, 4,5,4, 3,2,1])
--    True
--    λ> esCentroSimetrica (listArray ((1,1),(3,3)) [1,2,3, 4,5,4, 3,2,2])
--    False
--    λ> esCentroSimetrica (listArray ((1,1),(2,3)) [1,2,3, 4,5,4])
--    False
-- ---------------------------------------------------------------------

esCentroSimetrica :: Eq t => Array (Int,Int) t -> Bool
esCentroSimetrica a =
  n == m && and [a!(i,j) == a!(n-i+1,n-j+1) | i <- [1..n], j <- [i..n]] 
  where (_,(n,m)) = bounds a

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
--       8      2
--      / \    / \
--     3   5  2   0
-- se pueden representar por
--    ejArbol :: Arbol Int
--    ejArbol = N 10 (N 8 (H 3) (H 5))
--                   (N 2 (H 2) (H 0))
--
-- Un árbol cumple la propiedad de la suma si el valor de cada nodo es
-- igual a la suma de los valores de sus hijos. Por ejemplo, el árbol
-- anterior cumple la propiedad de la suma.
--
-- Definir la función
--    propSuma :: Arbol Int -> Bool
-- tal que (propSuma a) se verifica si el árbol a cumple la propiedad de
-- la suma. Por ejemplo,
--    λ> propSumaG (NG 10 [NG 8 [NG 3 [], NG 5 [] ], NG 2 [NG 2 [], NG 0 []]])
--    True
--    λ> propSumaG (NG 10 [NG 8 [NG 4 [], NG 5 [] ], NG 2 [NG 2 [], NG 0 []]])
--    False
--    λ> propSumaG (NG 10 [NG 8 [NG 3 [], NG 5 [] ], NG 2 [NG 2 [], NG 1 []]])
--    False
-- ---------------------------------------------------------------------

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
  deriving Show

propSuma :: Arbol Int -> Bool
propSuma (H _)     = True
propSuma (N x i d) = x == raiz i + raiz d && propSuma i && propSuma d
             
raiz :: Arbol Int -> Int
raiz (H x)     = x
raiz (N x _ _) = x

-- ---------------------------------------------------------------------
-- Ejercicio 4. Los árboles generales se pueden definir mediante el
-- siguiente tipo de datos
--    data ArbolG a = NG a [ArbolG a] deriving Show
-- Por ejemplo, el árbol
--          10
--         /  \
--        /    \
--       8      2
--      / \    / \
--     3   5  2   0
-- se puede representar por
--    ejArbolG :: ArbolG Int
--    ejArbolG = NG 10 [NG 8 [NG 3 [], NG 5 [] ], NG 2 [NG 2 [], NG 0 []]]
-- 
-- Definir la función
--    propSumaG :: ArbolG Int -> Bool
-- tal que (propSumaG a) se verifica si el árbol general a cumple la
-- propiedad de la suma. Por ejemplo,
--    λ> propSuma (N 10 (N 8 (H 3) (H 5)) (N 2 (H 2) (H 0)))
--    True
--    λ> propSuma (N 10 (N 8 (H 4) (H 5)) (N 2 (H 2) (H 0)))
--    False
--    λ> propSuma (N 10 (N 8 (H 3) (H 5)) (N 2 (H 2) (H 1)))
--    False
-- ---------------------------------------------------------------------

data ArbolG a = NG a [ArbolG a] deriving Show

propSumaG :: ArbolG Int -> Bool
propSumaG (NG _ []) = True
propSumaG (NG x as) = x == sum [raizG a | a <- as] 
                      && all propSumaG as

raizG :: ArbolG a -> a
raizG (NG x _) = x

