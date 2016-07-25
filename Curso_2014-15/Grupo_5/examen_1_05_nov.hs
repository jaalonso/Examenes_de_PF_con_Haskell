-- Informática (1º del Grado en Matemáticas)
-- 1º examen de evaluación continua (5 de noviembre de 2014)
-- ---------------------------------------------------------------------

import Data.Char
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función
--    esPotencia :: Integer -> Integer -> Bool
-- tal que (esPotencia x a) se verifica si x es una potencia de a. Por
-- ejemplo, 
--    esPotencia 32 2  ==  True
--    esPotencia 42 2  ==  False
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
esPotencia :: Integer -> Integer -> Bool
esPotencia x a = x `elem` [a^n | n <- [0..x]]

-- 2ª definición (por recursión):
esPotencia2 :: Integer -> Integer -> Bool
esPotencia2 x a = aux x a 0
    where aux x a b | b > x     = False
                    | otherwise = x == a ^ b || aux x a (b+1)

-- 3ª definición (por recursión):
esPotencia3 :: Integer -> Integer -> Bool
esPotencia3 0 _ = False
esPotencia3 1 a = True
esPotencia3 _ 1 = False
esPotencia3 x a = rem x a == 0 && esPotencia3 (div x a) a

-- La propiedad de equivalencia es
prop_equiv_esPotencia :: Integer -> Integer -> Property
prop_equiv_esPotencia x a =
    x > 0 && a > 0 ==> 
    esPotencia2 x a == b &&
    esPotencia3 x a == b
    where b = esPotencia x a

-- La comprobación es
--    ghci> quickCheck prop_equiv_esPotencia
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que, para cualesquiera números
-- enteros positivos x y a, x es potencia de a si y sólo si x² es
-- potencia de a². 
-- ---------------------------------------------------------------------

-- Propiedad de potencia
prop_potencia :: Integer -> Integer -> Property
prop_potencia x a =
    x > 0 && a > 0 ==> esPotencia x a == esPotencia (x*x) (a*a)

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir la función 
--    intercambia :: String -> String
-- tal que (intercambia xs) es la cadena obtenida poniendo la mayúsculas
-- de xs en minúscula y las minúsculas en mayúscula. Por ejemplo,
--    intercambia "Hoy es 5 de Noviembre"  ==  "hOY ES 5 DE nOVIEMBRE"
--    intercambia "hOY ES 5 DE nOVIEMBRE"  ==  "Hoy es 5 de Noviembre"
-- ---------------------------------------------------------------------

intercambia :: String -> String
intercambia xs = [intercambiaCaracter x | x <- xs]

intercambiaCaracter :: Char -> Char
intercambiaCaracter c | isLower c = toUpper c
                      | otherwise = toLower c

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Comprobar con QuickCheck que, para cualquier cadena xs
-- se tiene que (intercambia (intercambia ys)) es igual a ys, siendo ys
-- la lista de las letras (mayúsculas o minúsculas no acentuadas) de xs.
-- ---------------------------------------------------------------------

prop_intercambia :: String -> Bool
prop_intercambia xs = intercambia (intercambia ys) == ys
    where ys = [x | x <- xs, x `elem` ['a'..'z'] ++ ['A'..'Z']]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función
--    primosEquidistantes :: Integer -> [(Integer,Integer)]
-- tal que (primosEquidistantes n) es la lista de pares de primos
-- equidistantes de n y con la primera componente menor que la
-- segunda. Por ejemplo,
--    primosEquidistantes 8   ==  [(3,13),(5,11)]
--    primosEquidistantes 12  ==  [(5,19),(7,17),(11,13)]
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
primosEquidistantes :: Integer -> [(Integer,Integer)]
primosEquidistantes n =
    [(x,n+(n-x)) | x <- [2..n-1], esPrimo x, esPrimo (n+(n-x))]

esPrimo :: Integer -> Bool
esPrimo n = [x | x <- [1..n], rem n x == 0] == [1,n]

-- 2ª definición (con zip):
primosEquidistantes2 :: Integer -> [(Integer,Integer)]
primosEquidistantes2 n =
    reverse [(x,y) | (x,y) <- zip [n-1,n-2..1] [n+1..], esPrimo x, esPrimo y]

-- Propiedad de equivalencia de las definiciones:
prop_equiv_primosEquidistantes :: Integer -> Property
prop_equiv_primosEquidistantes n =
    n > 0 ==> primosEquidistantes n == primosEquidistantes2 n

-- La comprobación es
--    ghci> quickCheck prop_equiv_primosEquidistantes
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con QuickCheck si se cumple la siguiente
-- propiedad: "Todo número entero positivo mayor que 4 es equidistante
-- de dos primos" 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_suma2Primos :: Integer -> Property
prop_suma2Primos n =
    n > 4 ==> primosEquidistantes n /= []

-- La comprobación es
--    ghci> quickCheck prop_suma2Primos
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la función 
--    triangulo :: [a] -> [(a,a)]
-- tal que (triangulo xs) es la lista de los pares formados por cada uno
-- de los elementos de xs junto con sus siguientes en xs. Por ejemplo,
--    ghci> triangulo [3,2,5,9,7]
--    [(3,2),(3,5),(3,9),(3,7),
--           (2,5),(2,9),(2,7),
--                 (5,9),(5,7),
--                       (9,7)]
-- ---------------------------------------------------------------------

-- 1ª solución
triangulo :: [a] -> [(a,a)]
triangulo []     = []
triangulo (x:xs) = [(x,y) | y <- xs] ++ triangulo xs

-- 2ª solución
triangulo2 :: [a] -> [(a,a)]
triangulo2 []     = [] 
triangulo2 (x:xs) = zip (repeat x) xs ++ triangulo2 xs

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Comprobar con QuickCheck que la longitud de 
-- (triangulo xs) es la suma desde 1 hasta n-1, donde n es el número de
-- elementos de xs. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_triangulo :: [Int] -> Bool
prop_triangulo xs =
    length (triangulo xs) == sum [1..length xs - 1]

-- La comprobación es
--    ghci> quickCheck prop_triangulo
--    +++ OK, passed 100 tests.

