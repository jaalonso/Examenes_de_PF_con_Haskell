-- Informática (1º del Grado en Matemáticas)
-- 2º examen de evaluación continua (19 de diciembre de 2013)
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función 
--     bocata :: Eq a => a -> a -> [a] -> [a]
-- tal que (bocata x y zs) es la lista obtenida colocando y delante y
-- detrás de todos los elementos de zs que coinciden con x. Por ejemplo,
--     > bocata "chorizo" "pan" ["jamon", "chorizo", "queso", "chorizo"]
--     ["jamon","pan","chorizo","pan","queso","pan","chorizo","pan"]
--     > bocata "chorizo" "pan" ["jamon", "queso", "atun"]
--     ["jamon","queso","atun"]
-- ---------------------------------------------------------------------

bocata :: Eq a => a -> a -> [a] -> [a]
bocata _ _ []     = []
bocata x y (z:zs) | z == x    = y : z : y : bocata x y zs
                  | otherwise = z : bocata x y zs

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que el número de elementos de
-- (bocata a b xs) es el número de elementos de xs más el doble del
-- número de elementos de xs que coinciden con a.
-- ---------------------------------------------------------------------

-- La propiedad es 
prop_bocata :: String -> String -> [String] -> Bool
prop_bocata a b xs = 
    length (bocata a b xs) == length xs + 2 * length (filter (==a) xs)

-- La comprobación es
--    ghci> quickCheck prop_bocata
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función   
--    mezclaDigitos :: Integer -> Integer -> Integer
-- tal que (mezclaDigitos n m) es el número formado intercalando los
-- dígitos de n y m, empezando por los de n. Por ejemplo,
--    mezclaDigitos 12583 4519       == 142551893
--    mezclaDigitos 12583 4519091256 == 142551893091256
-- ---------------------------------------------------------------------

mezclaDigitos :: Integer -> Integer -> Integer
mezclaDigitos n m = 
    read (intercala (show n) (show m))

-- (intercala xs ys) es la lista obtenida intercalando los elementos de
-- xs e ys. Por ejemplo,
--    intercala [2,5,3] [4,7,9,6,0]  ==  [2,4,5,7,3,9,6,0]
--    intercala [4,7,9,6,0] [2,5,3]   ==  [4,2,7,5,9,3,6,0]
intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x:xs) (y:ys) = x : y : intercala xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 3. (Problema 211 del proyecto Euler) Dado un entero
-- positivo n, consideremos la suma de los cuadrados de sus divisores,
-- Por ejemplo,  
--    f(10) = 1 + 4 + 25 + 100 = 130
--    f(42) = 1 + 4 +  9 +  36 + 49 + 196 + 441 + 1764 = 2500
-- Decimos que n es especial si f(n) es un cuadrado perfecto. En los
-- ejemplos anteriores, 42 es especial y 10 no lo es.
-- 
-- Definir la función 
--    especial:: Int -> Bool
-- tal que (especial x) se verifica si x es un número es especial. Por
-- ejemplo, 
--    especial 42  ==  True
--    especial 10  ==  False
-- Calcular todos los números especiales de tres cifras.
-- ---------------------------------------------------------------------

especial:: Int -> Bool
especial n = esCuadrado (sum (map (^2) (divisores n)))

-- (esCuadrado n) se verifica si n es un cuadrado perfecto. Por ejemplo, 
--    esCuadrado 36  ==  True
--    esCuadrado 40  ==  False
esCuadrado :: Int -> Bool
esCuadrado n = y^2 == n
    where y = floor (sqrt (fromIntegral n))

-- (divisores n) es la lista de los divisores de n. Por ejemplo,
--    divisores 36  ==  [1,2,3,4,6,9,12,18,36]
divisores :: Int -> [Int]        
divisores n = [x | x <- [1..n], rem n x == 0]        

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función 
--    verificaMax :: (a -> Bool) -> [[a]] -> [a]
-- tal que (verificaMax p xss) es la lista de xss con mayor número de
-- elementos que verifican la propiedad p. Por ejemplo, 
--    ghci> verificaMax even [[1..5], [2,4..20], [3,2,1,4,8]]
--    [2,4,6,8,10,12,14,16,18,20]
--    ghci> verificaMax even [[1,2,3], [6,8], [3,2,10], [3]]
--    [6,8]
-- Nota: En caso de que haya más de una lista, obtener la primera. 
-- ---------------------------------------------------------------------

verificaMax :: (a -> Bool) -> [[a]] -> [a]        
verificaMax p xss = head [xs | xs <- xss, test xs]
    where f xs    = length [x | x <- xs, p x]
          m       = maximum [f xs | xs <-xss]
          test xs = f xs == m


