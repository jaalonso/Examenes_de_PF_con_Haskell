-- Informática (1º del Grado en Matemáticas y en Estadística)
-- 2º examen de evaluación continua (16 de diciembre de 2013) 
-- ---------------------------------------------------------------------

import Test.QuickCheck

prop_equivalencia :: [Int] -> Bool
prop_equivalencia xs =
    numeroConsecutivosC xs == numeroConsecutivosC2 xs

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por comprensión, la función
-- numeroConsecutivosC tal que (numeroConsecutivosC xs) es la cantidad
-- de números 
-- consecutivos que aparecen al comienzo de la lista xs. Por ejemplo, 
--    numeroConsecutivosC [1,3,5,7,9]      ==  1
--    numeroConsecutivosC [1,2,3,4,5,7,9]  ==  5
--    numeroConsecutivosC []               ==  0
--    numeroConsecutivosC [4,5]            ==  2
--    numeroConsecutivosC [4,7]            ==  1
--    numeroConsecutivosC [4,5,0,4,5,6]    ==  2
-- ---------------------------------------------------------------------

-- 1ª solución (con índices)
numeroConsecutivosC :: (Num a, Eq a) => [a] -> Int
numeroConsecutivosC xs 
    | null ys   = length xs
    | otherwise = head ys
    where ys = [n | n <- [1..length xs -1], xs !! n /= 1 + xs !! (n-1)]

-- 2ª solución (con zip3)
numeroConsecutivosC2 :: (Num a, Eq a) => [a] -> Int
numeroConsecutivosC2 [] = 0
numeroConsecutivosC2 [x] = 1
numeroConsecutivosC2 [x,y] | x+1 == y  = 2
                           | otherwise = 1
numeroConsecutivosC2 xs =
    head [k | (x,y,k) <- zip3 xs (tail xs) [1..], y /= x+1]

-- 3ª solución (con takeWhile)
numeroConsecutivosC3 [] = 0
numeroConsecutivosC3 xs =
    1 + length (takeWhile (==1) [y-x | (x,y) <- zip xs (tail xs)])

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, por recursión, la función numeroConsecutivosR 
-- tal que (numeroConsecutivosR xs) es la cantidad de números
-- consecutivos que aparecen al comienzo de la lista xs. Por ejemplo, 
--    numeroConsecutivosC [1,3,5,7,9]      ==  1
--    numeroConsecutivosC [1,2,3,4,5,7,9]  ==  5
--    numeroConsecutivosC []               ==  0
--    numeroConsecutivosC [4,5]            ==  2
--    numeroConsecutivosC [4,7]            ==  1
--    numeroConsecutivosC [4,5,0,4,5,6]    ==  2
-- ---------------------------------------------------------------------

numeroConsecutivosR []  = 0
numeroConsecutivosR [x] = 1
numeroConsecutivosR (x:y:ys) 
    | y == x+1  = 1 + numeroConsecutivosR (y:ys)
    | otherwise = 1

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Una sustitución es una lista de parejas 
-- [(x1,y1),...,(xn,yn)] que se usa para indicar que hay que reemplazar
-- cualquier ocurrencia de cada uno de los xi, por el correspondiente
-- yi. Por ejemplo, 
--    sustitucion = [('1','a'),('2','n'),('3','v'),('4','i'),('5','d')]
-- es la sustitución que reemplaza '1' por 'a', '2' por 'n', ... 
-- 
-- Definir, por comprensión, la función sustitucionEltC tal que
-- (sustitucionEltC xs z) es el resultado de aplicar la sustitución xs
-- al elemento z. Por ejemplo,
--    sustitucionEltC sustitucion '4'  ==  'i'
--    sustitucionEltC sustitucion '2'  ==  'n'
--    sustitucionEltC sustitucion '0'  ==  '0'
-- ---------------------------------------------------------------------

sustitucion = [('1','a'),('2','n'),('3','v'),('4','i'),('5','d')]

sustitucionEltC xs z = head [y | (x,y) <- xs, x == z] ++ [z]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, por recursión, la función sustitucionEltR tal
-- que (sustitucionEltR xs z) es el resultado de aplicar la sustitución
-- xs al elemento z. Por ejemplo,
--    sustitucionEltR sustitucion '4'  ==  'i'
--    sustitucionEltR sustitucion '2'  ==  'n'
--    sustitucionEltR sustitucion '0'  ==  '0'
-- ---------------------------------------------------------------------

sustitucionEltR [] z = z
sustitucionEltR ((x,y):xs) z 
    | x == z    = y
    | otherwise = sustitucionEltR xs z

-- ---------------------------------------------------------------------
-- Ejercicio 2.3, Definir, por comprensión, la función sustitucionLstC
-- tal que (sustitucionLstC xs zs) es el resultado de aplicar la
-- sustitución xs a los elementos de la lista zs. Por ejemplo,
--    sustitucionLstC sustitucion "2151"      ==  "nada"
--    sustitucionLstC sustitucion "3451"     ==  "vida"
--    sustitucionLstC sustitucion "2134515"  ==  "navidad"
-- ---------------------------------------------------------------------

sustitucionLstC xs zs = [sustitucionEltC xs z | z <- zs]

-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Definir, por recursión, la función sustitucionLstR tal
-- que (sustitucionLstR xs zs) es el resultado de aplicar la sustitución
-- xs a los elementos de la lista zs. Por ejemplo,
--    sustitucionLstR sustitucion "2151"     ==  "nada"
--    sustitucionLstR sustitucion "3451"     ==  "vida"
--    sustitucionLstR sustitucion "2134515"  ==  "navidad"
-- ---------------------------------------------------------------------

sustitucionLstR xs []     = []
sustitucionLstR xs (z:zs) =
     sustitucionEltR xs z : sustitucionLstR xs zs

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir, por recursión, la función sublista tal que 
-- (sublista xs ys) se verifica si todos los elementos de xs aparecen en
-- ys en el mismo orden aunque no necesariamente consecutivos. Por ejemplo,
--    sublista "meta"   "matematicas"  ==  True
--    sublista "temas"  "matematicas"  ==  True
--    sublista "mitica" "matematicas"  ==  False
-- ---------------------------------------------------------------------

sublista []     ys = True
sublista (x:xs) [] = False
sublista (x:xs) (y:ys) 
    | x == y           = sublista xs ys
    | otherwise        = sublista (x:xs) ys

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir, por recursión, la función numeroDigitosPares
-- tal que (numeroDigitosPares n) es la cantidad de dígitos pares que
-- hay en el número natural n. Por ejemplo,
--    numeroDigitosPares      0  ==  1
--    numeroDigitosPares      1  ==  0
--    numeroDigitosPares    246  ==  3
--    numeroDigitosPares    135  ==  0
--    numeroDigitosPares 123456  ==  3
-- ---------------------------------------------------------------------


numeroDigitosPares2 n
    | n < 10    = aux n
    | otherwise = (aux n `rem` 10) + numeroDigitosPares2 (n `div` 10)
    where aux n | even n    = 1
                | otherwise = 0
