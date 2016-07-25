-- Informática (1º del Grado en Matemáticas)
-- 2º examen de evaluación continua (4 de diciembre de 2014)
-- ---------------------------------------------------------------------

import Data.List
import Data.Char
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    seleccionaDiv :: [Integer] -> [Integer] -> [Integer]
-- tal que (seleccionaDiv xs ys) es la lista de los elementos de xs que
-- son divisores de los correspondientes elementos de ys. Por ejemplo,
--    seleccionaDiv [1..5] [7,8,1,2,3]      == [1,2]
--    seleccionaDiv [2,5,3,7] [1,3,4,5,0,9] == []
--    seleccionaDiv (repeat 1) [3,5,8]      == [1,1,1]
--    seleccionaDiv [1..4] [2,4..]          == [1,2,3,4]
-- ---------------------------------------------------------------------

-- Por comprensión:
seleccionaDiv :: [Integer] -> [Integer] -> [Integer]
seleccionaDiv xs ys = [x | (x,y) <- zip xs ys, rem y x == 0]

-- Por recursión:
seleccionaDivR :: [Integer] -> [Integer] -> [Integer]
seleccionaDivR (x:xs) (y:ys) | rem y x == 0 = x: seleccionaDivR xs ys
                             | otherwise    = seleccionaDivR xs ys
seleccionaDivR _ _ = []

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Un número es especial si se cumple que la suma de cada
-- dos dígitos consecutivos es primo. Por ejemplo, 
--    4116743 es especial pues 4+1, 1+1, 1+6, 6+7, 7+4 y 4+3 son primos.
--    41167435 no es especial porque 3+5 no es primo.
-- 
-- Definir la función 
--    especial :: Integer -> Bool
-- tal que (especial n) se verifica si n es especial. Por ejemplo,
--    especial 4116743  == True
--    especial 41167435 == False
-- ---------------------------------------------------------------------

especial :: Integer -> Bool
especial n = all esPrimo (zipWith (+) xs (tail xs))
    where xs = digitos n

digitos :: Integer -> [Integer]
digitos n = [read [x] | x <- show n]

esPrimo :: Integer -> Bool
esPrimo x = x `elem` takeWhile (<=x) primos

primos:: [Integer]
primos = criba [2..]
    where criba (p:ps) = p: criba [x | x <- ps, rem x p /= 0]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Calcular el menor (y el mayor) número especial con 6
-- cifras. 
-- ---------------------------------------------------------------------

-- El cálculo es 
--    ghci> head [n | n <- [10^5..], especial n]
--    111111
--    ghci> head [n | n <- [10^6,10^6-1..], especial n]
--    989898

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función
--    productoDigitosNN :: Integer -> Integer
-- tal que (productoDigitosNN n) es el producto de los dígitos no nulos
-- de n.
--    productoDigitosNN 2014  ==  8
-- ---------------------------------------------------------------------

productoDigitosNN :: Integer -> Integer
productoDigitosNN = product . filter (/=0) . digitos

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Consideremos la sucesión definida a partir de un
-- número d, de forma que cada elemento es la suma del anterior más el
-- producto de sus dígitos no nulos. Por ejemplo,
--    Si d =  1, la sucesión es 1,2,4,8,16,22,26,38,62,74,102,104, ...
--    Si d = 15, la sucesión es 15,20,22,26,38,62,74,102,104,108,...
-- 
-- Definir, usando iterate, la función 
--    sucesion :: Integer -> [Integer]
-- tal que (sucesion d) es la sucesión anterior, empezando por d. Por
-- ejemplo, 
--    take 10 (sucesion  1)   ==  [1,2,4,8,16,22,26,38,62,74]
--    take 10 (sucesion 15)   ==  [15,20,22,26,38,62,74,102,104,108]
-- ---------------------------------------------------------------------

sucesion :: Integer -> [Integer]
sucesion = iterate f
  where f x = x + productoDigitosNN x 

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Llamamos sucesionBase a la sucesión que empieza en
-- 1. Probar con QuickCheck que cualquier otra sucesión que empiece en
-- d, con d > 0, tiene algún elemento común con la sucesionBase.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_sucesion :: Integer -> Property
prop_sucesion d = 
    d > 0 ==> 
    [n | n <- sucesion d, n `elem` takeWhile (<=n) sucesionBase] /= []
    where sucesionBase = sucesion 1

-- La comprobación es
--    ghci> quickCheck prop_sucesion
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4. Consideremos el tipo de dato árbol, definido por
--   data Arbol a = H a | N a (Arbol a) (Arbol a)
-- y los siguientes ejemplos de árboles
--    ej1 = N 9 (N 3 (H 2) (H 4)) (H 7)
--    ej2 = N 9 (N 3 (H 2) (N 1 (H 4) (H 5))) (H 7)
-- 
-- Definir la función
--    allArbol :: (t -> Bool) -> Arbol t -> Bool
-- tal que (allArbol p a) se verifica si todos los elementos del árbol
-- verifican p. Por ejemplo,
--    allArbol even ej1 == False
--    allArbol (>0) ej1 == True
-- ---------------------------------------------------------------------

data Arbol a = H a | N a (Arbol a) (Arbol a)

ej1 = N 9 (N 3 (H 2) (H 4)) (H 7)
ej2 = N 9 (N 3 (H 2) (N 1 (H 4) (H 5))) (H 7)

allArbol :: (t -> Bool) -> Arbol t -> Bool
allArbol p (H x)     = p x
allArbol p (N r i d) = p r && allArbol p i && allArbol p d

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    listasNoPrimos :: [[Integer]]
-- tal que listasNoPrimos es lista formada por las listas de números
-- no primos consecutivos. Por ejemplo,
--    take 7 listasNoPrimos == [[1],[4],[6],[8,9,10],[12],[14,15,16],[18]]
-- ---------------------------------------------------------------------

listasNoPrimos :: [[Integer]]
listasNoPrimos = aux [1..]
    where aux xs = takeWhile (not . esPrimo) xs : 
                   aux (dropWhile esPrimo (dropWhile (not . esPrimo ) xs))

