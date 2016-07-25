-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 3º examen de evaluación continua (16 de enero de 2012)
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Dada una lista de números enteros, definiremos el
-- mayor salto como el mayor valor de las diferencias (en valor
-- absoluto) entre números consecutivos de la lista. Por ejemplo, dada
-- la lista [2,5,-3] las distancias son 
--    3 (valor absoluto de la resta 2 - 5) y
--    8 (valor absoluto de la resta de 5 y (-3))
-- Por tanto, su mayor salto es 8. No está definido el mayor salto para
-- listas con menos de 2 elementos
--
-- Definir, por compresión, la función 
--    mayorSaltoC :: [Integer] -> Integer
-- tal que (mayorSaltoC xs) es el mayor salto de la lista xs. Por
-- ejemplo, 
--    mayorSaltoC [1,5]              == 4
--    mayorSaltoC [10,-10,1,4,20,-2] == 22
-- ---------------------------------------------------------------------

mayorSaltoC :: [Integer] -> Integer
mayorSaltoC xs = maximum [abs (x-y) | (x,y) <- zip xs (tail xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, por recursión, la función 
--    mayorSaltoR :: [Integer] -> Integer
-- tal que (mayorSaltoR xs) es el mayor salto de la lista xs. Por
-- ejemplo, 
--    mayorSaltoR [1,5]              == 4
--    mayorSaltoR [10,-10,1,4,20,-2] == 22
-- ---------------------------------------------------------------------

mayorSaltoR :: [Integer] -> Integer
mayorSaltoR [x,y]    = abs (x-y)
mayorSaltoR (x:y:ys) = max (abs (x-y)) (mayorSaltoR (y:ys))

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comprobar con QuickCheck que mayorSaltoC y mayorSaltoR
-- son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_mayorSalto :: [Integer] -> Property
prop_mayorSalto xs = 
    length xs > 1 ==> mayorSaltoC xs == mayorSaltoR xs

-- La comprobación es
--    ghci> quickCheck prop_mayorSalto
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función 
--    acumulada :: [Int] -> [Int]
-- que (acumulada xs) es la lista que tiene en cada posición i el valor
-- que resulta de sumar los elementos de la lista xs desde la posicion 0 
-- hasta la i. Por ejemplo,
--    acumulada [2,5,1,4,3] == [2,7,8,12,15]
--    acumulada [1,-1,1,-1] == [1,0,1,0]
-- ---------------------------------------------------------------------

-- 1ª definición (pro comprensión):
acumulada :: [Int] -> [Int]
acumulada xs = [sum (take n xs) | n <- [1..length xs]]

-- 2ª definición (por recursión)
acumuladaR :: [Int] -> [Int]
acumuladaR [] = []
acumuladaR xs = acumuladaR (init xs) ++ [sum xs]

-- 3ª definición (por recursión final):
acumuladaRF :: [Int] -> [Int]
acumuladaRF [] = []
acumuladaRF (x:xs) = reverse (aux xs [x])
    where aux [] ys = ys
          aux (x:xs) (y:ys) = aux xs (x+y:y:ys)

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Dada una lista de números reales, la lista de
-- porcentajes contendrá el porcentaje de cada elemento de la lista
-- original en relación con la suma total de elementos. Por ejemplo, 
-- la lista de porcentajes de [1,2,3,4] es [10.0,20.0,30.0,40.0],
-- ya que 1 es el 10% de la suma (1+2+3+4 = 10), y así sucesivamente.
--
-- Definir, por recursión, la función
--    porcentajesR :: [Float] -> [Float]
-- tal que (porcentajesR xs) es la lista de porcentaje de xs. Por
-- ejemplo, 
--    porcentajesR [1,2,3,4] == [10.0,20.0,30.0,40.0]
--    porcentajesR [1,7,8,4]  ==  [5.0,35.0,40.0,20.0]
-- ---------------------------------------------------------------------

porcentajesR :: [Float] -> [Float]
porcentajesR xs = aux xs (sum xs)
    where aux [] _     = []
          aux (x:xs) s = (x*100/s) : aux xs s

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir, por comprensión, la función
--    porcentajesC :: [Float] -> [Float]
-- tal que (porcentajesC xs) es la lista de porcentaje de xs. Por
-- ejemplo, 
--    porcentajesC [1,2,3,4] == [10.0,20.0,30.0,40.0]
--    porcentajesC [1,7,8,4]  ==  [5.0,35.0,40.0,20.0]
-- ---------------------------------------------------------------------

porcentajesC :: [Float] -> [Float]
porcentajesC xs = [x*100/s | x <- xs]
    where s = sum xs

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir, usando map, la función
--    porcentajesS :: [Float] -> [Float]
-- tal que (porcentajesS xs) es la lista de porcentaje de xs. Por
-- ejemplo, 
--    porcentajesS [1,2,3,4] == [10.0,20.0,30.0,40.0]
--    porcentajesS [1,7,8,4]  ==  [5.0,35.0,40.0,20.0]
-- ---------------------------------------------------------------------

porcentajesS :: [Float] -> [Float]
porcentajesS xs = map (*(100/sum xs)) xs

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir, por plegado, la función
--    porcentajesP :: [Float] -> [Float]
-- tal que (porcentajesP xs) es la lista de porcentaje de xs. Por
-- ejemplo, 
--    porcentajesP [1,2,3,4] == [10.0,20.0,30.0,40.0]
--    porcentajesP [1,7,8,4]  ==  [5.0,35.0,40.0,20.0]
-- ---------------------------------------------------------------------

porcentajesF :: [Float] -> [Float]
porcentajesF xs = foldr (\x y -> (x*100/s):y) [] xs
    where s = sum xs

-- ---------------------------------------------------------------------
-- Ejercicio 3.4. Definir la función
--    equivalentes :: [Float] -> [Float] -> Bool
-- tal que (equivalentes xs ys) se verifica si el valor absoluto
-- de las diferencias de los elementos de xs e ys (tomados
-- posicionalmente) son infereriores a 0.001. Por ejemplo,
--    equivalentes [1,2,3] [1,2,3]     == True
--    equivalentes [1,2,3] [0.999,2,3] == True
--    equivalentes [1,2,3] [0.998,2,3] == False
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
equivalentes :: [Float] -> [Float] -> Bool
equivalentes xs ys =
    and [abs (x-y) <= 0.001 | (x,y) <- zip xs ys]

-- 2ª definición (por recursión)
equivalentes2 :: [Float] -> [Float] -> Bool
equivalentes2 [] []         = True
equivalentes2 _ []          = False
equivalentes2 [] _          = False
equivalentes2 (x:xs) (y:ys) = abs (x-y) <= 0.001 && equivalentes2 xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 3.5. Comprobar con QuickCheck que si xs es una lista de
-- números mayores o iguales que 0 cuya suma es mayor que 0, entonces
-- las listas (porcentajesR xs), (porcentajesC xs), (porcentajesS xs) y 
-- (porcentajesF xs) son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_porcentajes :: [Float] -> Property
prop_porcentajes xs = 
    and [x >= 0 | x <- xs] && sum xs > 0 ==>
    equivalentes (porcentajesC xs) ys &&
    equivalentes (porcentajesS xs) ys &&
    equivalentes (porcentajesF xs) ys 
    where ys = porcentajesR xs

-- La comprobación es
--    ghci> quickCheck prop_porcentajes
--    *** Gave up! Passed only 15 tests.

-- Otra forma de expresar la propiedad es
prop_porcentajes2 :: [Float] -> Property
prop_porcentajes2 xs = 
    sum xs' > 0 ==>
    equivalentes (porcentajesC xs') ys &&
    equivalentes (porcentajesS xs') ys &&
    equivalentes (porcentajesF xs') ys 
    where xs' = map abs xs
          ys = porcentajesR xs'

-- Su comprobación es
--    ghci> quickCheck prop_porcentajes2
--    +++ OK, passed 100 tests.


