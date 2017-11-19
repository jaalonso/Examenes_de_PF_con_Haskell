-- Informática (1º del Grado en Matemáticas)
-- 2º examen de evaluación continua (1 de diciembre de 2014)            
-- =====================================================================

-- ---------------------------------------------------------------------
-- § Librería auxiliar                                                --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Decimos que una lista está equilibrada con respecto a
-- una propiedad si el número de elementos de la lista que cumplen la
-- propiedad es igual al número de elementos de la lista que no la
-- cumplen. Por ejemplo, la lista [1,2,3,4,5,6,7,8] está equilibrada con
-- respecto a la propiedad 'ser par' y con respecto a la propiedad 'ser
-- primo', pero no con respecto a la propiedad 'ser múltiplo de 3'.
-- 
-- Definir, por comprensión, la función
--    listaEquilibradaC :: [a] -> (a -> Bool) -> Bool
-- tal que (listaEquilibradaC xs p) se verifica si la lista xs está
-- equilibrada con respecto a la propiedad p. Por ejemplo,
--    listaEquilibradaC [1..8] even                  ==  True
--    listaEquilibradaC [1..8] (\x -> mod x 3 == 0)  ==  False
-- ---------------------------------------------------------------------

listaEquilibradaC :: [a] -> (a -> Bool) -> Bool
listaEquilibradaC xs p =
    length [x | x <- xs, p x] == length [x | x <- xs, not (p x)]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, usando funciones de orden superior, la
-- función 
--    listaEquilibradaS :: [a] -> (a -> Bool) -> Bool
-- tal que (listaEquilibradaS xs p) se verifica si la lista xs está
-- equilibrada con respecto a la propiedad 'p'. Por ejemplo,
--    listaEquilibradaS [1..8] even                   ==  True
--    listaEquilibradaS [1..8] (\ x -> mod x 3 == 0)  ==  False
-- ---------------------------------------------------------------------

listaEquilibradaS :: [a] -> (a -> Bool) -> Bool
listaEquilibradaS xs p =
    length (filter p xs) == length (filter (not . p) xs)

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comprobar con QuickCheck que la longitud de las listas
-- que están equilibradas respecto a la propiedad 'ser impar' es pae
-- ---------------------------------------------------------------------

-- La propiedad es
prop_listaEquilibradaImpar :: [Int] -> Property
prop_listaEquilibradaImpar xs =
    listaEquilibradaC xs odd ==> even (length xs)

-- La comprobación es
--    ghci> quickCheck prop_listaEquilibradaImpar
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir, por recursión, la función
--    diferenciasParidadR :: [Int] -> [Int]
-- tal que (diferenciasParidadR xs) es la lista de las diferencias entre
-- elementos consecutivos de xs que tengan la misma paridad. Por ejemplo,
--    diferenciasParidadR [1,2,3,4,5,6,7,8]   ==  []
--    diferenciasParidadR [1,2,4,5,9,6,12,9]  ==  [2,4,6]
--    diferenciasParidadR [1,7,3]             ==  [6,-4]
-- ---------------------------------------------------------------------

-- 1ª definición:
diferenciasParidadR :: [Int] -> [Int]
diferenciasParidadR (x1:x2:xs)
    | even x1 == even x2 = x2-x1 : diferenciasParidadR (x2:xs)
    | otherwise          = diferenciasParidadR (x2:xs)
diferenciasParidadR _    = []

-- 2ª definición:
diferenciasParidadR2 :: [Int] -> [Int]
diferenciasParidadR2 xs = aux (zip xs (tail xs))
    where aux [] = []
          aux ((x,y):ps) | even x == even y = y-x : aux ps
                         | otherwise        = aux ps

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, usando plegado con flodr, la función
--    diferenciasParidadP :: [Int] -> [Int]
-- tal que (diferenciasParidadP xs) es la lista de las diferencias entre
-- elementos consecutivos de xs que tengan la misma paridad. Por ejemplo,
--    diferenciasParidadP [1,2,3,4,5,6,7,8]   ==  []
--    diferenciasParidadP [1,2,4,5,9,6,12,9]  ==  [2,4,6]
--    diferenciasParidadP [1,7,3]             ==  [6,-4]
-- ---------------------------------------------------------------------

-- 1ª definición:
diferenciasParidadP :: [Int] -> [Int]
diferenciasParidadP xs = 
    foldr (\ (x,y) r -> if even x == even y 
                        then y-x : r 
                        else r) [] (zip xs (tail xs))

-- 2ª definición:
diferenciasParidadP2 :: [Int] -> [Int]
diferenciasParidadP2 xs = 
    foldr f [] (zip xs (tail xs))
    where f (x,y) r | even x == even y = y-x : r 
                    | otherwise        = r  

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Comprobar con QuickCheck que todos los elementos de la
-- lista de las diferencias entre elementos consecutivos de xs que
-- tengan igual paridad son pares. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_diferenciasParidad :: [Int] -> Bool
prop_diferenciasParidad xs =
    all even (diferenciasParidadP xs)

-- La comprobación es
--    ghci> quickCheck prop_diferenciasParidad
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3. La sucesión generalizada de Fibonacci de grado N 
-- (N >= 1) se construye comenzando con el número 1 y calculando el
-- resto de términos como la suma de los N términos anteriores (si
-- existen). Por ejemplo, 
-- + la sucesión generalizada de Fibonacci de grado 2 es:
--      1, 1, 2, 3, 5, 8, 13, 21, 34, 55
-- + la sucesión generalizada de Fibonacci de grado 4 es:
--      1, 1, 2, 4, 8, 15, 29, 56, 108, 208
-- + la sucesión generalizada de Fibonacci de grado 6 es:
--      1, 1, 2, 4, 8, 16, 32, 63, 125, 248
-- 
-- Ejercicio 3.1. Definir, por recursión con acumulador, la función
--    fibsGenAR :: Int -> Int -> Int
-- tal que (fibsGenAR n m) es el término m de la sucesión generalizada
-- de Fibonacci de grado n. Por ejemplo,
--    fibsGenAR 4 3  ==  4
--    fibsGenAR 4 4  ==  8
--    fibsGenAR 4 5  ==  15
--    fibsGenAR 4 6  ==  29
--    fibsGenAR 4 7  ==  56
-- ---------------------------------------------------------------------

fibsGenAR :: Int -> Int -> Int
fibsGenAR = fibsGenARAux [1] 

fibsGenARAux :: [Int] -> Int -> Int -> Int
fibsGenARAux ac _ 0 = head ac
fibsGenARAux ac n m =
    fibsGenARAux (sum (take n ac) : ac) n (m-1)

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir, usando plegado con foldl, la función
--    fibsGenAP :: Int -> Int -> Int
-- tal que (fibsGenAP n m) es el término m de la sucesión generalizada
-- de Fibonacci de grado n. Por ejemplo,
--    fibsGenAP 4 3  ==  4
--    fibsGenAP 4 4  ==  8
--    fibsGenAP 4 5  ==  15
--    fibsGenAP 4 6  ==  29
--    fibsGenAP 4 7  ==  56
-- ---------------------------------------------------------------------

fibsGenAP :: Int -> Int -> Int
fibsGenAP n m =
    head (foldl (\ac x -> (sum (take n ac): ac)) [1] [1..m])

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir, por recursión, la función
--    fibsGen :: Int -> [Int]
-- tal que (fibsGen n) es la sucesión (infinita) generalizada de
-- Fibonacci de grado n. Por ejemplo,
--    take 10 (fibsGen 2)  ==  [1,1,2,3,5,8,13,21,34,55]
--    take 10 (fibsGen 4)  ==  [1,1,2,4,8,15,29,56,108,208]
--    take 10 (fibsGen 6)  ==  [1,1,2,4,8,16,32,63,125,248]
-- ---------------------------------------------------------------------

fibsGen :: Int -> [Int]
fibsGen = fibsGenAux [1]

fibsGenAux :: [Int] -> Int -> [Int]
fibsGenAux ac n = head ac : fibsGenAux (sum bc : bc) n
    where bc = take n ac
