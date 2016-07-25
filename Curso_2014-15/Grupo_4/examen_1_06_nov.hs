-- Informática (1º del Grado en Matemáticas)
-- 1º examen de evaluación continua (6 de noviembre de 2014)
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. La suma de la serie
--    1/1 + 1/2 + 1/4 + 1/8 + 1/16 + 1/32 ...
-- es 2. 
-- 
-- Definir la función 
--    aproxima2:: Integer -> Float
-- tal que (aproxima2 n) es la aproximación de 2 obtenida mediante n
-- términos de la serie. Por ejemplo, 
--    aproxima2 10  == 1.9990234
--    aproxima2 100 == 2.0
-- ---------------------------------------------------------------------

aproxima2:: Integer -> Float
aproxima2 n = sum [1 / (2^k) | k <- [0..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Decimos que los números m y n están relacionados si
-- tienen los mismos divisores primos.
-- 
-- Definir la función 
--    relacionados :: Integer -> Integer -> Bool
-- tal que (relacionados m n) se verifica si m y n están relacionados.
-- Por ejemplo,
--    relacionados 24 32 == False
--    relacionados 24 12 == True
--    relacionados 24 2  == False
--    relacionados 18 12 == True
-- ---------------------------------------------------------------------

relacionados:: Integer -> Integer -> Bool
relacionados m n = 
    nub (divisoresPrimos n) == nub (divisoresPrimos m)

divisoresPrimos :: Integer -> [Integer]
divisoresPrimos n = 
    [x | x <- [1..n], n `rem` x == 0, esPrimo x]
 
esPrimo :: Integer -> Bool
esPrimo n = divisores n == [1,n]

divisores :: Integer -> [Integer]
divisores n = 
    [x | x <- [1..n], n `rem` x == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. ¿Es cierto que si dos enteros positivos están
-- relacionados entonces uno es múltiplo del otro? Comprobarlo con
-- QuickCheck. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_rel :: Integer -> Integer -> Property
prop_rel m n = 
    m > 0 && n > 0 && relacionados m n ==>
    rem m n == 0 || rem n m == 0

-- La comprobación es
--    ghci> quickCheck prop_rel
--    *** Failed! Falsifiable (after 20 tests): 
--    18
--    12

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Comprobar con QuickCheck que si p es primo, los
-- números relacionados con p son las potencias de p.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_rel_primos :: Integer -> Integer -> Property
prop_rel_primos p n = 
    esPrimo p ==> esPotencia n p == relacionados n p

esPotencia :: Integer -> Integer -> Bool
esPotencia n p = nub (divisoresPrimos n) == [p]

-- La comprobación es
--    ghci> quickCheck prop_rel_primos
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función 
--    mcdLista :: [Integer] -> Integer
-- tal que (mcdLista xs) es el máximo común divisor de los elementos de 
-- xs. Por ejemplo, 
--    mcdLista [3,4,5]  == 1
--    mcdLista [6,4,12] == 2
-- ---------------------------------------------------------------------

mcdLista :: [Integer] -> Integer
mcdLista [x]    = x
mcdLista (x:xs) = gcd x (mcdLista xs)

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con QuickCheck que el resultado de 
-- (mcdLista xs) divide a todos los elementos de xs, para cualquier
-- lista de enteros. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_mcd_1 :: [Integer] -> Bool
prop_mcd_1 xs = and [rem x d == 0 | x <- xs]
    where d = mcdLista xs

-- La comprobación es
--    ghci> quickCheck prop_mcd_1
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Comprobar con QuickCheck que, para n > 1, el máximo
-- común divisor de los n primeros primos es 1.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_mcd_2 :: Int -> Property
prop_mcd_2 n = n > 1 ==> mcdLista (take n primos) == 1

primos = [x | x <- [2..], esPrimo x]

-- La comprobación es
--    ghci> quickCheck prop_mcd_2
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la función
--    menorLex:: (Ord a) => [a] -> [a] -> Bool
-- tal que menorLex sea el orden lexicográfico entre listas. Por
-- ejemplo,
--    menorLex "hola" "adios"          == False
--    menorLex "adios" "antes"         == True
--    menorLex "antes" "adios"         == False
--    menorLex [] [3,4]                == True
--    menorLex [3,4] []                == False
--    menorLex [1,2,3] [1,3,4,5]       == True
--    menorLex [1,2,3,3,3,3] [1,3,4,5] == True
-- ---------------------------------------------------------------------

menorLex:: Ord a => [a] -> [a] -> Bool
menorLex [] _          = True
menorLex _ []          = False
menorLex (x:xs) (y:ys) = x < y || (x == y && menorLex xs ys)

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Comprobar con QuickCheck que menorLex coincide con la
-- relación predefinida <=.
-- ---------------------------------------------------------------------

-- La propiedad es
prop :: Ord a => [a] -> [a] -> Bool
prop xs ys = menorLex xs ys == (xs <= ys)

-- La comprobación es:
--    quickCheck prop
--    +++ OK, passed 100 tests.
