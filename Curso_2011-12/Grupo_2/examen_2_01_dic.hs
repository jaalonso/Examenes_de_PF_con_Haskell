-- Informática (1º del Grado en Matemáticas, Grupo 2)
-- 2º examen de evaluación continua (1 de diciembre de 2011)
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por recursión, la función  
--     todosIgualesR:: Eq a => [a] -> Bool
-- tal que (todosIgualesR xs) se verifica si los elementos de la 
-- lista xs son todos iguales. Por ejemplo,   
--     todosIgualesR [1..5]    == False
--     todosIgualesR [2,2,2]   == True
--     todosIgualesR ["a","a"] == True
-- ---------------------------------------------------------------------

todosIgualesR:: Eq a => [a] -> Bool
todosIgualesR []  = True
todosIgualesR [_] = True
todosIgualesR (x:y:xs) = x == y && todosIgualesR (y:xs)

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, por comprensión, la función  
--     todosIgualesC:: Eq a => [a] -> Bool
-- tal que (todosIgualesC xs) se verifica si los elementos de la 
-- lista xs son todos iguales. Por ejemplo,   
--     todosIgualesC [1..5]    == False
--     todosIgualesC [2,2,2]   == True
--     todosIgualesC ["a","a"] == True
-- ---------------------------------------------------------------------

todosIgualesC:: Eq a => [a] -> Bool
todosIgualesC xs = and [x==y | (x,y) <- zip xs (tail xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comprobar con QuickCheck que ambas definiciones
-- coinciden. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_todosIguales :: [Int] -> Bool
prop_todosIguales xs = todosIgualesR xs == todosIgualesC xs

-- La comprobación es
--    ghci> quickCheck prop_todosIguales
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir la función 
--    intercalaCero :: [Int] -> [Int]
-- tal que (intercalaCero xs) es la lista que resulta de intercalar un 0 
-- entre cada dos elementos consecutivos x e y, cuando x es mayor que
-- y. Por ejemplo, 
--    intercalaCero [2,1,8,3,5,1,9] == [2,0,1,8,0,3,5,0,1,9]
--    intercalaCero [1..9]          == [1,2,3,4,5,6,7,8,9]
-- ---------------------------------------------------------------------

intercalaCero :: [Int] -> [Int]
intercalaCero []  = []
intercalaCero [x] = [x]
intercalaCero (x:y:xs) | x > y     = x : 0 : intercalaCero (y:xs)
                       | otherwise = x : intercalaCero (y:xs)

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Comprobar con QuickCheck la siguiente propiedad: para 
-- cualquier lista de enteros xs, la longitud de la lista que resulta 
-- de intercalar ceros es mayor o igual que la longitud de xs.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_intercalaCero :: [Int] -> Bool
prop_intercalaCero xs = 
    length (intercalaCero xs) >= length xs 

-- La comprobación es
--    ghci> quickCheck prop_intercalaCero
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Una lista social es una lista de números enteros 
-- x_1,...,x_n tales que para cada índice i se tiene que la suma de los 
-- divisores  propios de x_i es x_(i+1), para i=1,...,n−1 y la suma de
-- los divisores propios de x_n es x_1. Por ejemplo,  
-- [12496,14288,15472,14536,14264] es una lista social.
-- 
-- Definir la función 
--    esListaSocial :: [Int] -> Bool
-- tal que (esListaSocial xs) se verifica si xs es una lista social. 
-- Por ejemplo, 
--    esListaSocial [12496, 14288, 15472, 14536, 14264] == True
--    esListaSocial [12, 142, 154]                      == False
-- ---------------------------------------------------------------------

esListaSocial :: [Int] -> Bool
esListaSocial xs = 
    (and [asociados x y | (x,y) <- zip xs (tail xs)]) && 
    asociados (last xs) (head xs)
    where asociados :: Int -> Int -> Bool                    
          asociados x y = sum [k | k <- [1..x-1], rem x k == 0] == y

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. ¿Existen listas sociales de un único elemento? Si
-- crees que existen, busca una de ellas.
-- ---------------------------------------------------------------------

listasSocialesUnitarias :: [[Int]]
listasSocialesUnitarias = [[n] | n <- [1..], esListaSocial [n]]

-- El cálculo es
--    ghci> take 4 listasSocialesUnitarias
--    [[6],[28],[496],[8128]]

-- Se observa que [n] es una lista social syss n es un número perfecto.

-- ---------------------------------------------------------------------
-- Ejercicio 4. (Problema 358 del proyecto Euler) Un número x con n
-- cifras se denomina número circular si tiene la siguiente propiedad:
-- si se multiplica por 1, 2, 3, 4, ..., n, todos los números que
-- resultan tienen exactamente las mismas cifras que x, pero en distinto
-- orden. Por ejemplo, el número 142857 es circular, ya que
--    142857 * 1 = 142857
--    142857 * 2 = 285714
--    142857 * 3 = 428571
--    142857 * 4 = 571428
--    142857 * 5 = 714285
--    142857 * 6 = 857142 
-- 
-- Definir la función 
--    esCircular :: Int -> Bool
-- tal que (esCircular x) se verifica si x es circular. Por ejemplo,
--    esCircular 142857 == True
--    esCircular 14285  == False
-- ---------------------------------------------------------------------

esCircular :: Int -> Bool
esCircular x = and [esPermutacionCifras y x | y <- ys]
  where n = numeroDeCifras x
        ys = [k*x | k <- [1..n]]

-- (numeroDeCifras x) es el número de cifras de x. Por ejemplo,        
--    numeroDeCifras 142857  ==  6
numeroDeCifras :: Int -> Int
numeroDeCifras = length . cifras

-- (cifras n) es la lista de las cifras de n. Por ejemplo,        
--    cifras 142857  ==  [1,4,2,8,5,7]
cifras :: Int -> [Int]
cifras n = [read [x]| x <- show n]

-- (esPermutacion xs ys) se verifica si xs es una permutación de ys. Por
-- ejemplo,                   
--    esPermutacion [2,5,3] [3,5,2]    ==  True
--    esPermutacion [2,5,3] [2,3,5,2]  ==  False
esPermutacion :: Eq a => [a] -> [a] -> Bool
esPermutacion []     [] = True
esPermutacion []     _  = False
esPermutacion (x:xs) ys = elem x ys && esPermutacion xs (delete x ys)

-- (esPermutacion x y) se verifica si las cifras de x es una permutación
-- de las de y. Por ejemplo,           
--    esPermutacionCifras 253 352  ==  True
--    esPermutacionCifras 253 2352  ==  False
esPermutacionCifras :: Int -> Int -> Bool
esPermutacionCifras x y =
   esPermutacion (cifras x) (cifras y)
