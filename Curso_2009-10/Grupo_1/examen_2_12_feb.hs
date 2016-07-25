-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 2º examen de evaluación continua (12 de febrero de 2010)
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por recursión, la función 
--    diferenciasR :: Num a => [a] -> [a]
-- tal que (diferenciasR xs) es la lista de las diferencias entre los
-- elementos consecutivos de xs. Por ejemplo, 
--    diferenciasR [5,3,8,7]  == [2,-5,1]
-- ---------------------------------------------------------------------

diferenciasR :: Num a => [a] -> [a]
diferenciasR []         = []
diferenciasR [_]        = []
diferenciasR (x1:x2:xs) = (x1-x2) : diferenciasR (x2:xs)

-- La definición anterior puede simplificarse
diferenciasR' :: Num a => [a] -> [a]
diferenciasR' (x1:x2:xs) = (x1-x2) : diferenciasR' (x2:xs)
diferenciasR' _          = []

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, por comprensión, la función 
--    diferenciasC :: Num a => [a] -> [a]
-- tal que (diferenciasC xs) es la lista de las diferencias entre los
-- elementos consecutivos de xs. Por ejemplo, 
--    diferenciasC [5,3,8,7]  == [2,-5,1]
-- ---------------------------------------------------------------------

diferenciasC :: Num a => [a] -> [a]
diferenciasC xs = [a-b | (a,b) <- zip xs (tail xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    producto :: [[a]] -> [[a]]
-- tal que (producto xss) es el producto cartesiano de los conjuntos
-- xss. Por ejemplo,
--    ghci> producto [[1,3],[2,5]]
--    [[1,2],[1,5],[3,2],[3,5]]
--    ghci> producto [[1,3],[2,5],[6,4]]
--    [[1,2,6],[1,2,4],[1,5,6],[1,5,4],[3,2,6],[3,2,4],[3,5,6],[3,5,4]]
--    ghci> producto [[1,3,5],[2,4]]
--    [[1,2],[1,4],[3,2],[3,4],[5,2],[5,4]]
--    ghci> producto []
--    [[]]
-- ---------------------------------------------------------------------

producto :: [[a]] -> [[a]]
producto []       = [[]]
producto (xs:xss) = [x:ys | x <- xs, ys <- producto xss]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir el predicado
--    comprueba :: [[Int]] -> Bool
-- tal que tal que (comprueba xss) se verifica si cada elemento de la
-- lista de listas xss contiene algún número par. Por ejemplo, 
--    comprueba [[1,2],[3,4,5],[8]]  ==  True
--    comprueba [[1,2],[3,5]]        ==  False
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
comprueba :: [[Int]] -> Bool
comprueba xss = and [or [even x | x <- xs] | xs <- xss]

-- 2ª definición (por recursión):
compruebaR :: [[Int]] -> Bool
compruebaR [] = True
compruebaR (xs:xss) = tienePar xs && compruebaR xss

-- (tienePar xs) se verifica si xs contiene algún número par. 
tienePar  :: [Int] -> Bool
tienePar []     = False
tienePar (x:xs) = even x || tienePar xs

-- 3ª definición (por plegado):
compruebaP :: [[Int]] -> Bool
compruebaP = foldr ((&&) . tienePar) True 

-- (tieneParP xs) se verifica si xs contiene algún número par. 
tieneParP  :: [Int] -> Bool
tieneParP = foldr ((||) . even) False 

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    pertenece :: Ord a => a -> [a] -> Bool
-- tal que (pertenece x ys) se verifica si x pertenece a la lista
-- ordenada creciente, finita o infinita, ys. Por ejemplo,
--    pertenece 22 [1,3,22,34]  ==  True
--    pertenece 22 [1,3,34]     ==  False
--    pertenece 23 [1,3..]      ==  True
--    pertenece 22 [1,3..]      ==  False
-- ---------------------------------------------------------------------

pertenece :: Ord a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys) | x >  y    = pertenece x ys
                   | x == y    = True
                   | otherwise = False

-- La definición de pertenece puede simplificarse
pertenece' :: Ord a => a -> [a] -> Bool
pertenece' x ys = x `elem` takeWhile (<= x) ys
