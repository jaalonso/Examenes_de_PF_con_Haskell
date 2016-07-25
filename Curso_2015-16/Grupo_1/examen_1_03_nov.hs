-- Informática (1º del Grado en Matemáticas)
-- 1º examen de evaluación continua (3 de noviembre de 2015)
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List 

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. La suma de la serie
--    1/3 + 1/15 + 1/35 + 1/63 + ...+ 1/(4*x^2-1) + ...
-- es 1/2. 
-- 
-- Definir la función 
--    sumaS2:: Double -> Double
-- tal que (sumaS2 n) es la aproximación de 1/2 obtenida mediante n
-- términos de la serie. Por ejemplo, 
--    sumaS2 10  == 0.4761904761904761
--    sumaS2 100 == 0.49751243781094495
-- ---------------------------------------------------------------------

sumaS2 :: Double -> Double
sumaS2 n = sum [1/(4*x^2-1) | x <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que la sumas finitas de la
-- serie siempre son menores que 1/2.
-- ---------------------------------------------------------------------

-- La propiedad es
propSumaS2 :: Double -> Bool
propSumaS2 n = sumaS2 n < 0.5

-- La comprobación es
--    ghci> quickCheck propSumaS2
--    +++ OK, passed 100 tests.

-- --------------------------------------------------------------------
-- Ejercicio 1.3. Definir la función 
--    menorNError :: Double -> Double
-- tal que (menorNError x) es el menor número de términos de la serie
-- anterior necesarios para obtener 1/2 con un error menor que x. Por
-- ejemplo,
--    menorNError 0.01  == 25.0 
--    menorNError 0.001 == 250.0
-- ---------------------------------------------------------------------

menorNError :: Double -> Double
menorNError x = 
    head [n | n <- [1..], 0.5 - sumaS2 n < x] 

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Decimos que un número n es "muy divisible por 3" si es 
-- divisible por 3 y sigue siendo divisible por 3 si vamos quitando
-- dígitos por la derecha. Por ejemplo, 96060 es muy divisible por 3
-- porque 96060, 9606, 960, 96 y 9 son todos divisibles por 3.
-- 
-- Definir la función
--    muyDivPor3 :: Integer -> Bool
-- tal que (muyDivPor3 n) se verifica si n es muy divisible por 3. Por
-- ejemplo, 
--    muyDivPor3 96060 == True
--    muyDivPor3 90616 == False
-- ---------------------------------------------------------------------

muyDivPor3 :: Integer -> Bool
muyDivPor3 n 
    | n < 10    = n `rem` 3 == 0
    | otherwise = n `rem` 3 == 0 && muyDivPor3 (n `div` 10)

-- --------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función
--    numeroMuyDivPor3Cifras :: Integer -> Integer
-- tal que (numeroMuyDivPor3CifrasC k) es la cantidad de números de k
-- cifras muy divisibles por 3. Por ejemplo,
--    numeroMuyDivPor3Cifras 5 == 768
--    numeroMuyDivPor3Cifras 7 == 12288
--    numeroMuyDivPor3Cifras 9 == 196608
-- --------------------------------------------------------------------

-- 1ª definición
numeroMuyDivPor3Cifras :: Integer -> Integer
numeroMuyDivPor3Cifras k = 
    genericLength [x | x <- [10^(k-1)..10^k-1], muyDivPor3 x]

-- 2ª definición
numeroMuyDivPor3Cifras2 :: Integer -> Integer
numeroMuyDivPor3Cifras2 k = 
    genericLength [x | x <- [n,n+3..10^k-1], muyDivPor3 x]
    where n = k*10^(k-1)

-- 3ª definición
-- =============

numeroMuyDivPor3Cifras3 :: Integer -> Integer
numeroMuyDivPor3Cifras3 k = genericLength (numeroMuyDivPor3Cifras3' k)

numeroMuyDivPor3Cifras3' :: Integer -> [Integer]
numeroMuyDivPor3Cifras3' 1 = [3,6,9] 
numeroMuyDivPor3Cifras3' k = 
    [10*x+y | x <- numeroMuyDivPor3Cifras3' (k-1),
              y <- [0,3..9]]

-- 4ª definición
-- =============
numeroMuyDivPor3Cifras4 :: Integer -> Integer
numeroMuyDivPor3Cifras4 1 = 3
numeroMuyDivPor3Cifras4 k = 4 * numeroMuyDivPor3Cifras4 (k-1)

-- 5ª definición
numeroMuyDivPor3Cifras5 :: Integer -> Integer
numeroMuyDivPor3Cifras5 k = 3 * 4^(k-1)

-- Comparación de eficiencia
-- =========================

--    ghci> numeroMuyDivPor3Cifras 6
--    3072
--    (3.47 secs, 534,789,608 bytes)
--    ghci> numeroMuyDivPor3Cifras2 6
--    2048
--    (0.88 secs, 107,883,432 bytes)
--    ghci> numeroMuyDivPor3Cifras3 6
--    3072
--    (0.01 secs, 0 bytes)
--    
--    ghci> numeroMuyDivPor3Cifras2 7
--    0
--    (2.57 secs, 375,999,336 bytes)
--    ghci> numeroMuyDivPor3Cifras3 7
--    12288
--    (0.02 secs, 0 bytes)
--    ghci> numeroMuyDivPor3Cifras4 7
--    12288
--    (0.00 secs, 0 bytes)
--    ghci> numeroMuyDivPor3Cifras5 7
--    12288
--    (0.01 secs, 0 bytes)
--
--    ghci> numeroMuyDivPor3Cifras4 (10^5) `rem` 100000
--    32032
--    (5.74 secs, 1,408,600,592 bytes)
--    ghci> numeroMuyDivPor3Cifras5 (10^5) `rem` 100000
--    32032
--    (0.02 secs, 0 bytes)

-- --------------------------------------------------------------------
-- Ejercicio 3. Definir una función 
--    intercala:: [a] -> [a] -> [a]
-- tal que (intercala xs ys) es la lista que resulta de intercalar los
-- elementos de xs con los de ys. Por ejemplo,
--    intercala [1..7] [9,2,0,3] == [1,9,2,2,3,0,4,3,5,6,7]
--    intercala [9,2,0,3] [1..7] == [9,1,2,2,0,3,3,4,5,6,7]
--    intercala "hola" "adios"   == "haodliaos"
-- ---------------------------------------------------------------------

intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x:xs) (y:ys) = x : y : intercala xs ys

-- --------------------------------------------------------------------
-- Ejercicio 4. La diferencia simétrica de dos conjuntos es el conjunto
-- cuyos elementos son aquellos que pertenecen a alguno de los conjuntos
-- iniciales, sin pertenecer a ambos a la vez. Por ejemplo, la
-- diferencia simétrica de {2,5,3} y {4,2,3,7} es {5,4,7}.

-- Definir la función
--    diferenciaSimetrica :: Eq a => [a] -> [a] -> [a]
-- tal que (diferenciaSimetrica xs ys) es la diferencia simétrica de xs
-- e ys. Por ejemplo,
--    diferenciaSimetrica [2,5,3] [4,2,3,7]    ==  [5,4,7]
--    diferenciaSimetrica [2,5,3] [5,2,3]      ==  []
--    diferenciaSimetrica [2,5,2] [4,2,3,7]    ==  [5,4,3,7]
--    diferenciaSimetrica [2,5,2] [4,2,4,7]    ==  [5,4,4,7]
--    diferenciaSimetrica [2,5,2,4] [4,2,4,7]  ==  [5,7]
-- ---------------------------------------------------------------------

diferenciaSimetrica :: Eq a => [a] -> [a] -> [a]
diferenciaSimetrica xs ys = 
    [x | x <- xs, x `notElem` ys] ++ [y | y <- ys, y `notElem` xs] 

-- ---------------------------------------------------------------------
-- Ejercicio 5. (Basado en el problema 4 del Proyecto Euler)
-- El número 9009 es capicúa y es producto de dos números de dos dígitos,
-- pues 9009 = 91*99. 
-- 
-- Definir la función
--    numerosC2Menores :: Int -> [Int]
-- tal que (numerosC2Menores n) es la lista de números capicúas menores
-- que n que son producto de 2 números de dos dígitos. Por ejemplo,
--    numerosC2Menores 100           == []
--    numerosC2Menores 400           == [121,242,252,272,323,363]
--    length (numerosC2Menores 1000) == 38
-- ---------------------------------------------------------------------

numerosC2Menores :: Int -> [Int]
numerosC2Menores n = [x | x <- productos n, esCapicua x]

-- (productos n) es la lista de números menores que n que son productos
-- de 2 números de dos dígitos. 
productos :: Int -> [Int]
productos n = 
    sort (nub [x*y | x <- [10..99], y <- [x..99], x*y < n])

-- 2ª definición de productos
productos2 :: Int -> [Int]
productos2 n = 
    init (nub (sort [x*y | x <- [10..min 99 (n `div` 10)], 
                           y <- [x..min 99 (n `div` x)]]))

-- (esCapicua x) se verifica si x es capicúa.
esCapicua :: Int -> Bool
esCapicua x = xs == reverse xs
    where xs = show x
