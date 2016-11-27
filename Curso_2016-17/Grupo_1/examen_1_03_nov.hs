-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 1º examen de evaluación continua (3 de noviembre de 2016)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. La intersección de dos listas xs, ys es la lista
-- formada por los elementos de xs que también pertenecen a ys.
--
-- Definir, por comprensión, la función
--    interseccionC :: Eq a => [a] -> [a] -> [a]
-- tal que (interseccionC xs ys) es la intersección de xs e ys. Por
-- ejemplo, 
--    interseccionC [2,7,4,1] [1,3,6] == [1]
--    interseccionC [2..10] [3..12]   == [3,4,5,6,7,8,9,10]
--    interseccionC [2..10] [23..120] == []
-- ---------------------------------------------------------------------

interseccionC :: Eq a => [a] -> [a] -> [a]
interseccionC xs ys = [x | x <- xs, x `elem` ys]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, por recursión, la función
--    interseccionR :: Eq a => [a] -> [a] -> [a]
-- tal que (interseccionR xs ys) es la intersección de xs e ys. Por
-- ejemplo, 
--    interseccionR [2,7,4,1] [1,3,6] == [1]
--    interseccionR [2..10] [3..12]   == [3,4,5,6,7,8,9,10]
--    interseccionR [2..10] [23..120] == []
-- ---------------------------------------------------------------------

interseccionR ::Eq a => [a] -> [a] -> [a]
interseccionR [] ys = []
interseccionR (x:xs) ys
  | x `elem` ys = x : interseccionR xs ys
  | otherwise   = interseccionR xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comprobar con QuikCheck que ambas definiciones
-- coinciden.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_interseccion :: (Eq a) => [a] -> [a] -> Bool
prop_interseccion xs ys =
  interseccionC xs ys == interseccionR xs ys

-- La comprobación es
--    ghci> quickCheck prop_interseccion
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. El doble factorial de un número n se define por
--    n!! = n*(n-2)* ... * 3 * 1, si n es impar
--    n!! = n*(n-2)* ... * 4 * 2, si n es par
--    1!! = 1
--    0!! = 1
-- Por ejemplo,
--    8!! = 8*6*4*2 = 384
--    9!! = 9*7*5*3*1 = 945
-- 
-- Definir, por comprensión, la función
--    dobleFactorialC :: Integer -> Integer
-- tal que (dobleFactorialC n) es el doble factorial de n. Por ejemplo,
--    dobleFactorialC 8 == 384
--    dobleFactorialC 9 == 945
-- ---------------------------------------------------------------------

dobleFactorialC :: Integer -> Integer
dobleFactorialC n = product [n,n-2..2]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, por recursión, la función
--    dobleFactorialR :: Integer -> Integer
-- tal que (dobleFactorialR n) es el doble factorial de n. Por ejemplo,
--    dobleFactorialR 8 == 384
--    dobleFactorialR 9 == 945
-- ---------------------------------------------------------------------

dobleFactorialR :: Integer -> Integer
dobleFactorialR 0 = 1
dobleFactorialR 1 = 1
dobleFactorialR n = n * dobleFactorialR (n-2)

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Comprobar con QuikCheck que ambas definiciones
-- coinciden para n >= 0.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_dobleFactorial :: Integer -> Property
prop_dobleFactorial n =
  n >= 0 ==> dobleFactorialC n == dobleFactorialR n

-- La comprobación es
--    ghci> quickCheck prop_dobleFactorial
--    +++ OK, passed 100 tests.  

-- ---------------------------------------------------------------------
-- Ejercicio 3. Un número n es especial si es mayor que 9 y la suma de
-- cualesquiera dos dígitos consecutivos es un número primo. Por
-- ejemplo, 4116743 es especial pues se cumple que la suma de dos
-- dígitos consecutivos es un primo, ya que 4+1, 1+1, 1+6, 6+7, 7+4 y
-- 4+3 son primos.
-- 
-- Definir una función 
--    especial :: Integer -> Bool
-- tal que (especial n) reconozca si n es especial. Por ejemplo,
--    especial 4116743  == True
--    especial 41167435 == False
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

especial1 :: Integer -> Bool
especial1 n = 
  n > 9 && and [primo (x+y) | (x,y) <- zip xs (tail xs)]
  where xs = digitos n

-- (digitos n) es la lista con los dígitos de n. Por ejemplo,
--    digitos 325  ==  [3,2,5]
digitos :: Integer -> [Int]
digitos n = [read [x] | x <- show n]


-- (primo x) se verifica si x es primo. Por ejemplo,
--    primo 7  ==  True
--    primo 9  ==  False
primo :: Int -> Bool
primo x = factores x == [1,x]

-- (factores x) es la lista de los factores de x. Por ejemplo,
--    factores 12  ==  [1,2,3,4,6,12]
factores :: Int -> [Int]
factores x = [y | y <- [1..x] , x `rem` y == 0]

-- 2ª definición
-- =============

especial2 :: Integer -> Bool
especial2 n = n > 9 && all primo (zipWith (+) xs (tail xs))
  where xs = digitos n

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    acumula :: Num a => [a] -> [a]
-- tal que (acumula xs) es la lista obtenida sumando a cada elemento de
-- xs todos los posteriores. Por ejemplo:
--    acumula [1..5]    == [15,14,12,9,5]
--    acumula [3,5,1,2] == [11,8,3,2]
--    acumula [-1,0]    == [-1,0]
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión):
acumula :: Num a => [a] -> [a]
acumula []     = []
acumula (x:xs) = (x + sum xs): acumula xs

-- 2ª definición (por comprensión):
acumula2 :: Num a => [a] -> [a]
acumula2 xs = [sum (drop k xs) | k <- [0..length xs - 1]]

-- 3ª definición (por composición):
acumula3 :: Num a => [a] -> [a]
acumula3 = init . map sum . tails
