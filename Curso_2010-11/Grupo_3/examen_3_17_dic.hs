-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 3º examen de evaluación continua (17 de diciembre de 2010)
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    grafoReducido_1:: (Eq a, Eq b) => (a->b)->(a-> Bool) -> [a] -> [(a,b)]
-- tal que (grafoReducido f p xs) es la lista (sin repeticiones) de los
-- pares formados por los elementos de xs que verifican el predicado p y
-- sus imágenes. Por ejemplo, 
--    grafoReducido (^2) even [1..9]  ==  [(2,4),(4,16),(6,36),(8,64)]
--    grafoReducido (+4) even (replicate 40 1) == []
--    grafoReducido (*5) even (replicate 40 2) == [(2,10)]
-- ---------------------------------------------------------------------

-- 1ª definición
grafoReducido1:: (Eq a, Eq b) => (a->b)->(a-> Bool) -> [a] -> [(a,b)]
grafoReducido1 f p xs = nub (map (\x -> (x,f x)) (filter p xs))

-- 2ª definición
grafoReducido2:: (Eq a, Eq b) => (a->b)->(a-> Bool) -> [a] -> [(a,b)]
grafoReducido2 f p xs = zip as (map f as) 
    where as = filter p (nub xs)

-- 3ª definición
grafoReducido3:: (Eq a, Eq b) => (a->b)->(a-> Bool) -> [a] -> [(a,b)]
grafoReducido3 f p xs = nub [(x,f x) | x <- xs, p x]

-- -------------------------------------------------------------------
-- Ejercicio 2.1. Un número natural n se denomina semiperfecto si es la
-- suma de algunos de sus divisores propios. Por ejemplo, 18 es
-- semiperfecto ya que sus divisores son 1, 2, 3, 6, 9 y se cumple que
-- 3+6+9=18.  
-- 
-- Definir la función 
--    esSemiPerfecto:: Int -> Bool
-- tal que (esSemiPerfecto n) se verifica si n es semiperfecto. Por
-- ejemplo, 
--    esSemiPerfecto 18 == True
--    esSemiPerfecto 9  == False
--    esSemiPerfecto 24 == True
-- ---------------------------------------------------------------------

-- 1ª solución:
esSemiPerfecto:: Int -> Bool
esSemiPerfecto n = any p (sublistas (divisores n))
    where p xs = sum xs == n

-- (divisores n) es la lista de los divisores de n. Por ejemplo,
--    divisores 18  ==  [1,2,3,6,9]
divisores :: Int -> [Int]
divisores n = [m | m <- [1..n-1], n `mod` m == 0]

-- (sublistas xs) es la lista de las sublistas de xs. por ejemplo,
--    sublistas [3,2,5]  ==  [[],[5],[2],[2,5],[3],[3,5],[3,2],[3,2,5]]
sublistas :: [a] -> [[a]]
sublistas []     = [[]]
sublistas (x:xs) = yss ++ [x:ys | ys <- yss]
    where yss = sublistas xs

-- 2ª solución:
esSemiPerfecto2:: Int -> Bool
esSemiPerfecto2 n = or [sum xs == n  | xs <- sublistas (divisores n)]  

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la constante 
--    primerSemiPerfecto :: Int
-- tal que su valor es el primer número semiperfecto. 
-- ---------------------------------------------------------------------

primerSemiPerfecto :: Int
primerSemiPerfecto = head [n| n<- [1..], esSemiPerfecto n]

-- Su cálculo es
--    ghci> primerSemiPerfecto
--    6

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Definir la función 
--    semiPerfecto :: Int -> Int
-- tal que (semiPerfecto n) es el n-ésimo número semiperfecto. Por
-- ejemplo, 
--    semiPerfecto 1   == 6
--    semiPerfecto 4   == 20
--    semiPerfecto 100 == 414
-- ---------------------------------------------------------------------

semiPerfecto :: Int -> Int
semiPerfecto n = [n| n <- [1..], esSemiPerfecto n] !! (n-1)

-- -------------------------------------------------------------------
-- Ejercicio 3.1. Definir mediante plegado la función 
--    producto :: Num a => [a] -> a
-- tal que (producto xs) es el producto de los elementos de la lista
-- xs. Por ejemplo, 
--    producto [2,1,-3,4,5,-6] == 720
-- ---------------------------------------------------------------------

producto :: Num a => [a] -> a
producto = foldr (*) 1

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir mediante plegado la función 
--    productoPred :: Num a => (a -> Bool) -> [a] -> a
-- tal que (productoPred p xs) es el producto de los elementos de la
-- lista xs que verifican el predicado p. Por ejemplo, 
--    productoPred even [2,1,-3,4,5,-6] == -48
-- ---------------------------------------------------------------------

productoPred :: Num a => (a -> Bool) -> [a] -> a
productoPred p = foldr f 1
    where f x y | p x       = x*y
                | otherwise = y

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir la función la función 
--    productoPos :: (Num a, Ord a) => [a] -> a
-- tal que (productoPos xs) es el producto de los elementos
-- estríctamente positivos de la lista xs. Por ejemplo,
--    productoPos [2,1,-3,4,5,-6] == 40
-- ---------------------------------------------------------------------

productoPos :: (Num a, Ord a) => [a] -> a
productoPos = productoPred (>0)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Las relaciones finitas se pueden representar mediante
-- listas de pares. Por ejemplo,
--    r1, r2, r3 :: [(Int, Int)]
--    r1 = [(1,3), (2,6), (8,9), (2,7)]
--    r2 = [(1,3), (2,6), (8,9), (3,7)]
--    r3 = [(1,3), (2,6), (8,9), (3,6)]
-- 
-- Definir la función 
--    esFuncion :: [(Int,Int)] -> Bool
-- tal que (esFuncion r) se verifica si la relación r es una función (es
-- decir, a cada elemento del dominio de la relación r le corresponde un
-- único elemento). Por ejemplo, 
--    esFuncion r1 == False
--    esFuncion r2 == True
--    esFuncion r3 == True
-- ---------------------------------------------------------------------

r1, r2, r3 :: [(Int, Int)]
r1 = [(1,3), (2,6), (8,9), (2,7)]
r2 = [(1,3), (2,6), (8,9), (3,7)]
r3 = [(1,3), (2,6), (8,9), (3,6)]

-- 1ª definición:
esFuncion :: [(Int,Int)] -> Bool
esFuncion r = and [length (imagenes x r) == 1 | x <- dominio r]

-- (dominio r) es el dominio de la relación r. Por ejemplo,
--    dominio r1  ==  [1,2,8]
dominio :: [(Int, Int)] -> [Int]
dominio r = nub [x | (x,_) <- r]

-- (imagenes x r) es la lista de las imágenes de x en la relación r. Por
-- ejemplo, 
--    imagenes 2 r1  ==  [6,7]
imagenes :: Int -> [(Int, Int)] -> [Int]
imagenes x r = nub [y | (z,y) <- r, z == x]

-- 2ª definición:
esFuncion2 :: (Eq a, Eq b) => [(a, b)] -> Bool
esFuncion2 r = [fst x | x <- nub r] == nub [fst x | x <- nub r]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Se denomina cola de una lista xs a una sublista no vacía   
-- de xs formada por un elemento y los siguientes hasta el final. Por
-- ejemplo, [3,4,5] es una cola de la lista [1,2,3,4,5].
--  
-- Definir la función 
--    colas :: [a] -> [[a]]
-- tal que (colas xs) es la lista de las colas de la lista xs. Por
-- ejemplo,  
--    colas []        == []
--    colas [1,2]     == [[1,2],[2]]
--    colas [4,1,2,5] == [[4,1,2,5],[1,2,5],[2,5],[5]]
-- ---------------------------------------------------------------------

colas :: [a] -> [[a]]
colas []     = []
colas (x:xs) = (x:xs) : colas xs

-- -------------------------------------------------------------------
-- Ejercicio 6.1. Se denomina cabeza de una lista xs a una sublista no
-- vacía de xs formada por el primer elemento y los siguientes hasta uno
-- dado. Por ejemplo, [1,2,3] es una cabeza de [1,2,3,4,5]. 
-- 
-- Definir, por recursión, la función 
--    cabezasR :: [a] -> [[a]] 
-- tal que (cabezasR xs) es la lista de las cabezas de la lista xs. Por
-- ejemplo, 
--    cabezasR []          == [] 
--    cabezasR [1,4]       == [[1],[1,4]] 
--    cabezasR [1,4,5,2,3] == [[1],[1,4],[1,4,5],[1,4,5,2],[1,4,5,2,3]] 
-- ---------------------------------------------------------------------

cabezasR :: [a] -> [[a]] 
cabezasR []     = [] 
cabezasR (x:xs) = [x] : [x:ys | ys <- cabezasR xs]
 
-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Definir, por plegado, la función 
--    cabezasP :: [a] -> [[a]] 
-- tal que (cabezasP xs) es la lista de las cabezas de la lista xs. Por
-- ejemplo, 
--    cabezasP []          == [] 
--    cabezasP [1,4]       == [[1],[1,4]] 
--    cabezasP [1,4,5,2,3] == [[1],[1,4],[1,4,5],[1,4,5,2],[1,4,5,2,3]] 
-- ---------------------------------------------------------------------

cabezasP :: [a] -> [[a]]
cabezasP = foldr (\x ys -> [x] : [x:y | y <- ys]) [] 

-- ---------------------------------------------------------------------
-- Ejercicio 6.3. Definir, por composición, la función 
--    cabezasC :: [a] -> [[a]] 
-- tal que (cabezasC xs) es la lista de las cabezas de la lista xs. Por
-- ejemplo, 
--    cabezasC []          == [] 
--    cabezasC [1,4]       == [[1],[1,4]] 
--    cabezasC [1,4,5,2,3] == [[1],[1,4],[1,4,5],[1,4,5,2],[1,4,5,2,3]] 
-- ---------------------------------------------------------------------

cabezasC :: [a] -> [[a]]
cabezasC = reverse . map reverse . colas . reverse
