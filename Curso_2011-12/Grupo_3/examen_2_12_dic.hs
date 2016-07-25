-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 2º examen de evaluación continua (12 de diciembre de 2011)
-- ---------------------------------------------------------------------
 
import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir, por comprensión, la función 
--    filtraAplicaC :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaC f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo, 
--    filtraAplicaC (4+) (< 3) [1..7] == [5,6]
-- ---------------------------------------------------------------------

filtraAplicaC :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaC f p xs = [f x | x <- xs, p x]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir, por recursión, la función 
--    filtraAplicaR :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaR f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo, 
--    filtraAplicaR (4+) (< 3) [1..7] == [5,6]
-- ---------------------------------------------------------------------

filtraAplicaR :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaR _ _ [] = []
filtraAplicaR f p (x:xs) | p x       = f x : filtraAplicaR f p xs
                         | otherwise = filtraAplicaR f p xs

-- ---------------------------------------------------------------------
-- Ejercicio 3. Define la función 
--    masDeDos :: Eq a => a -> [a] -> Bool
-- tal que (masDeDos x ys) se verifica si x aparece más de dos veces en
-- ys. Por ejemplo,
--    masDeDos 1 [2,1,3,1,4,1,1] == True
--    masDeDos 1 [1,1,2,3]       == False
-- ---------------------------------------------------------------------

masDeDos :: Eq a => a -> [a] -> Bool
masDeDos x ys = ocurrencias x ys > 2

-- (ocurrencias x ys) es el número de ocurrencias de x en ys. Por
-- ejemplo, 
--    ocurrencias 1 [2,1,3,1,4,1,1]  ==  4
--    ocurrencias 1 [1,1,2,3]        ==  2
ocurrencias :: Eq a => a -> [a] -> Int
ocurrencias x ys = length [y | y <- ys, y == x]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función 
--    sinMasDeDos :: Eq a => [a] -> [a]
-- tal que (sinMasDeDos xs) es la la lista que resulta de eliminar en xs los
-- elementos muy repetidos, dejando que aparezcan dos veces a lo
-- sumo. Por ejemplos,
--    sinMasDeDos [2,1,3,1,4,1,1] == [2,3,4,1,1]
--    sinMasDeDos [1,1,2,3,2,2,5] == [1,1,3,2,2,5] 
-- ---------------------------------------------------------------------

sinMasDeDos :: Eq a => [a] -> [a]
sinMasDeDos [] = []
sinMasDeDos (y:ys) | masDeDos y (y:ys) = sinMasDeDos ys
       	           | otherwise         = y : sinMasDeDos ys

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función 
--    sinRepetidos :: Eq a => [a] -> [a]
-- tal que (sinRepetidos xs) es la lista que resulta de quitar todos los
-- elementos repetidos de xs. Por ejemplo,
--    sinRepetidos [2,1,3,2,1,3,1]   ==  [2,3,1]
-- ---------------------------------------------------------------------

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) | x `elem` xs = sinRepetidos xs
	 	    | otherwise   = x : sinRepetidos xs

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función 
--    repetidos :: Eq a => [a] -> [a]
-- tal que (repetidos xs) es la lista de los elementos repetidos de
-- xs. Por ejemplo, 
--    repetidos [1,3,2,1,2,3,4] == [1,3,2]
--    repetidos [1,2,3]         == []
-- ---------------------------------------------------------------------

repetidos :: Eq a => [a] -> [a]
repetidos [] = []
repetidos (x:xs) | x `elem` xs = x : repetidos xs
	  	 | otherwise   =  repetidos xs

-- ---------------------------------------------------------------------
-- Ejercicio 7. Comprobar con QuickCheck que si una lista xs no tiene
-- elementos repetidos, entonces (sinMasDeDos xs) y (sinRepetidos xs)
-- son iguales.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_limpia :: [Int] -> Property
prop_limpia xs = 
    null (repetidos xs) ==> sinMasDeDos xs == sinRepetidos xs

-- La comprobación es
--    ghci> quickCheck prop_limpia
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 8.1. Definir, por recursión, la función 
--    seleccionaElementoPosicionR :: Eq a => a -> Int -> [[a]]-> [[a]]
-- (seleccionaElementoPosicionR x n xss) es la lista de elementos de xss
-- en las que x aparece en la posición n. Por ejemplo,
--    ghci> seleccionaElementoPosicionR 'a' 1 ["casa","perro", "bajo"]
--    ["casa","bajo"]
-- ---------------------------------------------------------------------

seleccionaElementoPosicionR :: Eq a => a -> Int -> [[a]]-> [[a]]
seleccionaElementoPosicionR x n []= []
seleccionaElementoPosicionR x n (xs:xss) 
    | ocurreEn x n xs = xs : seleccionaElementoPosicionR x n xss
    | otherwise       = seleccionaElementoPosicionR x n xss

-- (ocurreEn x n ys) se verifica si x ocurre en ys en la posición n. Por
-- ejemplo, 
--    ocurreEn 'a' 1 "casa"  ==  True
--    ocurreEn 'a' 2 "casa"  ==  False
--    ocurreEn 'a' 7 "casa"  ==  False
ocurreEn :: Eq a => a -> Int -> [a] -> Bool
ocurreEn x n ys = 0 <= n && n < length ys && ys!!n == x 

-- ---------------------------------------------------------------------
-- Ejercicio 8.2. Definir, por comprensión, la función 
--    seleccionaElementoPosicionC :: Eq a => a -> Int -> [[a]]-> [[a]]
-- (seleccionaElementoPosicionC x n xss) es la lista de elementos de xss
-- en las que x aparece en la posición n. Por ejemplo,
--    ghci> seleccionaElementoPosicionC 'a' 1 ["casa","perro", "bajo"]
--    ["casa","bajo"]
-- ---------------------------------------------------------------------

seleccionaElementoPosicionC :: Eq a => a -> Int -> [[a]]-> [[a]]
seleccionaElementoPosicionC x n xss = 
    [xs | xs <- xss,  ocurreEn x n xs]
