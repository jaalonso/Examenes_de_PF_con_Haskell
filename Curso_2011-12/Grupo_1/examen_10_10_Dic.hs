-- Informática (1º del Grado en Matemáticas, Grupo 1)  
-- Examen de la convocatoria de Diciembre de 2012
-- ---------------------------------------------------------------------

import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir una función verificanP 
--    verificanP :: Int -> Integer -> (Integer -> Bool) -> Bool
-- tal que (verificanP k n p) se cumple si los primeros k dígitos del
-- número n verifican la propiedad p y el (k+1)-ésimo no la verifica. 
-- Por ejemplo, 
--   verificanP 3 224119 even == True
--   verificanP 3 265119 even == False
--   verificanP 3 224619 even == False
-- ---------------------------------------------------------------------

digitos:: Integer -> [Integer]
digitos n = [read [x]| x <- show n]

verificanP :: Int -> Integer -> (Integer -> Bool) -> Bool
verificanP k n p = length (takeWhile p (digitos n)) == k

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función 
--   primosPK :: (Integer -> Bool) -> Int -> [Integer]
-- tal que (primosPK p k) es la lista de los números primos cuyos
-- primeros k dígitos verifican la propiedad p y el (k+1)-ésimo no la
-- verifica. Por ejemplo,  
--   take 10 (primosPK even 4)
--   [20021,20023,20029,20047,20063,20089,20201,20249,20261,20269]
-- ---------------------------------------------------------------------

primosPK :: (Integer -> Bool) -> Int -> [Integer]
primosPK p k = [n | n <- primos, verificanP k n p]

primos :: [Integer]
primos = criba [2..]
    where criba []     = []
          criba (n:ns) = n : criba (elimina n ns)
          elimina n xs = [x | x <- xs, x `mod` n /= 0]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función  
--    suma2 :: Int -> [Int] -> Maybe (Int,Int)
-- tal que (suma2 n xs) es un par de elementos (x,y) de la lista xs cuya
-- suma es n, si éstos existe. Por ejemplo,
--    suma2 27 [1..6] == Nothing
--    suma2 7 [1..6]  == Just (1,6)
-- ---------------------------------------------------------------------

suma2 :: Int -> [Int] -> Maybe (Int,Int)
suma2 _ []                     =  Nothing
suma2 _ [_]                    =  Nothing
suma2 y (x:xs) | y-x `elem` xs = Just (x,y-x)
               | otherwise     = suma2 y xs

-- ---------------------------------------------------------------------
-- Ejercicio 3. Consideremos el tipo de los árboles binarios definido
-- por 
--    data Arbol = H Int
--               | N Int Arbol Arbol
--               deriving Show
-- Por ejemplo,
--         5
--        / \
--       /   \
--      3     7
--     / \   / \
--    1   4 6   9 
-- se representa por
--    ejArbol = N 5 (N 3 (H 1) (H 4)) (N 7 (H 6) (H 9))
-- 
-- Definir la función 
--    aplica :: (Int -> Int) -> Arbol -> Arbol
-- tal que (aplica f a) es el árbol obtenido aplicando la función f a
-- los elementos del árbol a. Por ejemplo,
--    ghci> aplica (+2) ejArbol
--    N 7 (N 5 (H 3) (H 6)) (N 9 (H 8) (H 11))
--    ghci> aplica (*5) ejArbol
--    N 25 (N 15 (H 5) (H 20)) (N 35 (H 30) (H 45))
-- ---------------------------------------------------------------------
          
data Arbol = H Int
           | N Int Arbol Arbol
           deriving Show

ejArbol :: Arbol
ejArbol = N 5 (N 3 (H 1) (H 4)) (N 7 (H 6) (H 9))

aplica :: (Int -> Int) -> Arbol -> Arbol
aplica f (H x)     = H (f x)
aplica f (N x i d) = N (f x) (aplica f i) (aplica f d)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Las matrices puede representarse mediante tablas cuyos
-- índices son pares de números naturales:    
--    type Matriz = Array (Int,Int) Int
-- Definir la función 
--    algunMenor :: Matriz -> [Int]
-- tal que (algunMenor p) es la lista de los elementos de p que tienen
-- algún vecino menor que él. Por ejemplo,  
--    algunMenor (listArray ((1,1),(3,4)) [9,4,6,5,8,1,7,3,4,2,5,4])
--    [9,4,6,5,8,7,4,2,5,4]          
-- pues sólo el 1 y el 3 no tienen ningún vecino menor en la matriz
--    |9 4 6 5|
--    |8 1 7 3|
--    |4 2 5 4|
-- ---------------------------------------------------------------------        

type Matriz = Array (Int,Int) Int

algunMenor :: Matriz -> [Int]
algunMenor p = 
    [p!(i,j) | (i,j) <- indices p,
               or [p!(a,b) < p!(i,j) | (a,b) <- vecinos (i,j)]] 
    where (_,(m,n)) = bounds p
          vecinos (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)],
                                   b <- [max 1 (j-1)..min n (j+1)],
                                   (a,b) /= (i,j)]
