-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 1º examen de evaluación continua (6 de noviembre de 2015)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función
--    repiteElementos :: Int -> [a] -> [a]
-- tal que (repiteElementos k xs) es la lista obtenida repitiendo cada
-- elemento de xs k veces. Por ejemplo,
--    repiteElementos 3 [5,2,7,4]  ==  [5,5,5,2,2,2,7,7,7,4,4,4]
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
repiteElementos1 :: Int -> [a] -> [a]
repiteElementos1 k xs = concat [replicate k x | x <- xs]

-- 2ª definición (con map)
repiteElementos2 :: Int -> [a] -> [a]
repiteElementos2 k xs = concat (map (replicate k) xs)

-- 3ª definición (con concatMap):
repiteElementos3 :: Int -> [a] -> [a]
repiteElementos3 k = concatMap (replicate k)

-- 4ª definición (por recursión):
repiteElementos4 :: Int -> [a] -> [a]
repiteElementos4 k []     = []
repiteElementos4 k (x:xs) = replicate k x ++ repiteElementos4 k xs

-- 5ª definición (por plegado):
repiteElementos5 :: Int -> [a] -> [a]
repiteElementos5 k = foldr ((++) . replicate k) []

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que, para todo número natural
-- k y toda lista xs, el número de elementos de (repiteElementos k xs)
-- es k veces el número de elementos de xs.
--
-- Nota. Al hacer la comprobación limitar el tamaño de las pruebas como
-- se indica a continuación
--    quickCheckWith (stdArgs {maxSize=7}) prop_repiteElementos
-- ---------------------------------------------------------------------

-- La propiedad es
prop_repiteElementos :: Int -> [Int] -> Property
prop_repiteElementos k xs =
    k >= 0 ==> length (repiteElementos1 k xs) == k * length xs 

-- La comprobación es
--    ghci> quickCheckWith (stdArgs {maxSize=7}) prop_repiteElementos
--    +++ OK, passed 100 tests.

-- ----------------------------------------------------------------------
-- Ejercicio 2. Todo número entero positivo n se puede escribir como
-- 2^k*m, con m impar. Se dice que m es la parte impar de n. Por
-- ejemplo, la parte impar de 40 es 5 porque 40 = 5*2^3.
-- 
-- Definir la función 
--    parteImpar :: Integer -> Integer
-- tal que (parteImpar n) es la parte impar de n. Por ejemplo,
--    parteImpar 40  ==  5
-- ----------------------------------------------------------------------

parteImpar :: Integer -> Integer
parteImpar n | even n    = parteImpar (n `div` 2)
             | otherwise = n

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    refinada :: [Float] -> [Float]
-- tal que (refinada xs) es la lista obtenida intercalando entre cada
-- dos elementos consecutivos de xs su media aritmética. Por ejemplo,
--    refinada [2,7,1,8]  ==  [2.0,4.5,7.0,4.0,1.0,4.5,8.0]
--    refinada [2]        ==  [2.0]
--    refinada []         ==  []
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión):
refinada :: [Float] -> [Float]
refinada (x:y:zs) = x : (x+y)/2 : refinada (y:zs)
refinada xs       = xs

-- 2ª definición (por comprensión);
refinada2 :: [Float] -> [Float]
refinada2 []     = []
refinada2 (x:xs) = x : concat [[(a+b)/2,b] | (a,b) <- zip (x:xs) xs]  

-- ---------------------------------------------------------------------
-- Ejercicio 4. Se dice que en una sucesión de números x(1),x(2),...,x(n) 
-- hay una inversión cuando existe un par de números x(i) > x(j), siendo
-- i < j. Por ejemplo, en la sucesión 2, 1, 4, 3 hay dos inversiones (2
-- antes que 1 y 4 antes que 3) y en la sucesión 4, 3, 1, 2 hay cinco
-- inversiones (4 antes 3, 4 antes 1, 4 antes 2, 3 antes 1, 3 antes 2).
-- 
-- Definir la función 
--    numeroInversiones :: Ord a => [a] -> Int  
-- tal que (numeroInversiones xs) es el número de inversiones de xs. Por
-- ejemplo, 
--    numeroInversiones [2,1,4,3]  ==  2
--    numeroInversiones [4,3,1,2]  ==  5
-- ---------------------------------------------------------------------

-- 1ª solución (por recursión)
numeroInversiones1 :: Ord a => [a] -> Int  
numeroInversiones1 [] = 0
numeroInversiones1 (x:xs) =
    length [y | y <- xs, y < x] + numeroInversiones1 xs

-- 2ª solución (por comprensión)
numeroInversiones2 :: Ord a => [a] -> Int  
numeroInversiones2 xs =
    length [(i,j) | i <- [0..n-2], j <- [i+1..n-1], xs!!i > xs!!j]
    where n = length xs

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir la función
--    producto :: [[a]] -> [[a]]
-- tal que (producto xss) es el producto cartesiano de los conjuntos xss. 
-- Por ejemplo,
--    ghci> producto [[1,3],[2,5]]
--    [[1,2],[1,5],[3,2],[3,5]]
--    ghci> producto [[1,3],[2,5],[6,4]]
--    [[1,2,6],[1,2,4],[1,5,6],[1,5,4],[3,2,6],[3,2,4],[3,5,6],[3,5,4]]
--    ghci> producto [[1,3,5],[2,4]]
--    [[1,2],[1,4],[3,2],[3,4],[5,2],[5,4]]
--    ghci> producto []
--    [[]]
--    ghci> producto [[x] | x <- [1..10]]
--    [[1,2,3,4,5,6,7,8,9,10]]
-- ---------------------------------------------------------------------

producto :: [[a]] -> [[a]]
producto []       = [[]]
producto (xs:xss) = [x:ys | x <- xs, ys <- producto xss]

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Comprobar con QuickCheck que el número de elementos de
-- (producto xss) es el producto de los números de elementos de los
-- elementos de xss.
--
-- Nota. Al hacer la comprobación limitar el tamaño de las pruebas como
-- se indica a continuación
--    quickCheckWith (stdArgs {maxSize=7}) prop_producto
-- ---------------------------------------------------------------------

-- La propiedad es
prop_producto :: [[Int]] -> Bool
prop_producto xss = 
    length (producto xss) == product [length xs | xs <- xss]

-- La comprobación es
--    ghci> quickCheckWith (stdArgs {maxSize=7}) prop_producto
--    +++ OK, passed 100 tests.
