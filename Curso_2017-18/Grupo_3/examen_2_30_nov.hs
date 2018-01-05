-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 2º examen de evaluación continua (30 de noviembre de 2017)      
-- =====================================================================

-- ---------------------------------------------------------------------
-- § Librerías                                                        --
-- ---------------------------------------------------------------------

import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Dados un número x, una relación r y una lista ys, el
-- índice de relación de x en ys con respecto a r, es el número de
-- elementos de ys tales que x está relacionado con y con respecto a
-- r. Por ejemplo, el índice de relación del número 2 en la lista
-- [1,2,3,5,1,4] con respecto a la relación < es 3, pues en la lista hay
-- 3 elementos (el 3, el 5 y el 4) tales que 2 < 3, 2 < 5 y 2 < 4.
--
-- Definir la función
--    listaIndicesRelacion :: [Int] -> (Int -> Int -> Bool) -> [Int]
--                            -> [Int]
-- tal que (listaIndicesRelacion xs r ys) es la lista de los índices de
-- relación de los elementos de la lista xs en la lista ys con respecto
-- a la relación r. Por ejemplo,
--    λ> listaIndicesRelacion [2] (<) [1,2,3,5,1,4]
--    [3]
--    λ> listaIndicesRelacion [4,2,5] (<) [1,6,3,6,2]
--    [2,3,2]
--    λ> listaIndicesRelacion [4,2,5] (/=) [1,6,3,6,2]
--    [5,4,5]
--    λ> listaIndicesRelacion [4,2,5] (\ x y -> odd (x+y)) [1,6,3,6,2]
--    [2,2,3]
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

listaIndicesRelacion :: [Int] -> (Int -> Int -> Bool) -> [Int] -> [Int]
listaIndicesRelacion xs r ys =
  [indice x r ys | x <- xs]

indice :: Int -> (Int -> Int -> Bool) -> [Int] -> Int
indice x r ys =
  length [y | y <- ys, r x y]

-- 2ª solución
-- ===========

listaIndicesRelacion2 :: [Int] -> (Int -> Int -> Bool) -> [Int] -> [Int]
listaIndicesRelacion2 xs r ys =
  map (\x -> length (filter (r x) ys)) xs

-- ---------------------------------------------------------------------
-- Ejercicio 2. Decimos que dos listas xs e ys encajan, si hay un trozo
-- no nulo al final de la lista xs que aparece al comienzo de la lista 
-- ys. Por ejemplo [1,2,3,4,5,6] y [5,6,7,8] encajan, pues el trozo con
-- los dos últimos elementos de la primera lista, [5,6], aparece al
-- comienzo de la segunda lista.
--
-- Consideramos la función
--   encajadas :: Eq a => [a] -> [a] -> [a]
-- tal que (encajadas xs ys) se verifica si las listas xs e ys encajan.
-- Por ejemplo, 
--   encajadas [1,2,3,4,5,6] [5,6,7,8]  ==  True
--   encajadas [4,5,6] [6,7,8]          ==  True
--   encajadas [4,5,6] [4,3,6,8]        ==  False
--   encajadas [4,5,6] [7,8]            ==  False
-- ---------------------------------------------------------------------

-- 1ª solución
encajadas1 :: Eq a => [a] -> [a] -> Bool
encajadas1 [] ys = False
encajadas1 (x:xs) ys =
  (x:xs) == take (length (x:xs)) ys || encajadas1 xs ys

-- 2ª solución
encajadas2 :: Eq a => [a] -> [a] -> Bool
encajadas2 xs ys = aux xs ys [1..length ys]
  where aux xs ys []     = False
        aux xs ys (n:ns) = drop (length xs - n) xs == take n ys ||
                           aux xs ys ns

-- 3ª solución
encajadas3 :: Eq a => [a] -> [a] -> Bool
encajadas3 xs ys =
  foldr (\ n r -> drop (length xs - n) xs == take n ys || r)
        False [1..length ys]

-- 4ª solución
encajadas4 :: Eq a => [a] -> [a] -> Bool
encajadas4 xs ys =
  any (`isPrefixOf` ys) (init (tails xs))

-- ---------------------------------------------------------------------
-- Ejercicio 3. Se considera la función
--   repeticionesConsecutivas :: [Int] -> [Int]
-- tal que (repeticionesConsecutivas xs) es la lista con el número de
-- veces que se repiten los elementos de la lista xs de forma
-- consecutiva. Por ejemplo,
--   repeticionesConsecutivas [1,1,1,3,3,2,2]    ==  [3,2,2]
--   repeticionesConsecutivas [1,2,2,2,3,3]      ==  [1,3,2]
--   repeticionesConsecutivas [1,1,3,3,3,1,1,1]  ==  [2,3,3]
--   repeticionesConsecutivas []                 ==  []
-- ---------------------------------------------------------------------

-- 1ª solución
repeticionesConsecutivas1 :: [Int] -> [Int]
repeticionesConsecutivas1 xs = [length ys | ys <- group xs]

-- 2ª solución
repeticionesConsecutivas2 :: [Int] -> [Int]
repeticionesConsecutivas2 = map length . group

-- 3ª solución
repeticionesConsecutivas3 :: [Int] -> [Int]
repeticionesConsecutivas3 [] = []
repeticionesConsecutivas3 (x:xs) =
  1 + length (takeWhile (==x) xs) :
  repeticionesConsecutivas3 (dropWhile (==x) xs)

-- 4ª solución
repeticionesConsecutivas4 :: [Int] -> [Int]
repeticionesConsecutivas4 []     = []
repeticionesConsecutivas4 (x:xs) =
  1 + length ys : repeticionesConsecutivas3 zs
  where (ys,zs) = span (==x) xs

-- ---------------------------------------------------------------------
-- Ejercicio 4. Un número triangular es aquel que tiene la forma
-- n*(n+1)/2 para algún n. Un número pentagonal es aquel que tiene la
-- forma n*(3*n-1)/2 para algún n.
--
-- Definir la constante
--   numerosTriangulares :: [Integer]
-- cuyo valor es la lista infinita de todos los números  triangulares.
-- Por ejemplo,
--   take 10 numerosTriangulares  ==  [1,3,6,10,15,21,28,36,45,55]
--   numerosTriangulares !! 1000  ==  501501
--
-- Definir la constante
--   numerosPentagonales :: [Integer]
-- cuyo valor es la lista infinita de todos los números pentagonales.
-- Por ejemplo, 
--   take 10 numerosPentagonales  ==  [1,5,12,22,35,51,70,92,117,145]
--   numerosPentagonales !! 1000  ==  1502501
--
-- Calcular los 5 primeros números que son al mismo tiempo triangulares
-- y pentagonales. 
-- ---------------------------------------------------------------------

numerosTriangulares :: [Integer]
numerosTriangulares =
  [n * (n + 1) `div` 2 | n <- [1..]]

numerosPentagonales :: [Integer]
numerosPentagonales =
  [n * (3 * n - 1) `div` 2 | n <- [1..]]

interseccionInfinitaCreciente :: [Integer] -> [Integer] -> [Integer]
interseccionInfinitaCreciente (x:xs) (y:ys)
  | x == y    = x : interseccionInfinitaCreciente xs ys
  | x < y     = interseccionInfinitaCreciente xs (y:ys)
  | otherwise = interseccionInfinitaCreciente (x:xs) ys

lista5primerosTriangularesPentagonales :: [Integer]
lista5primerosTriangularesPentagonales =
  take 5 (interseccionInfinitaCreciente numerosTriangulares
                                        numerosPentagonales)

-- Los 5 primeros números que son al mismo tiempo triangulares y
-- pentagonales son: [1,210,40755,7906276,1533776805]
