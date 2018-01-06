-- Informática (1º del Grado en Matemáticas), Grupo 5
-- 2º examen de evaluación continua (18 de diciembre de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Librerías auxiliares
-- ---------------------------------------------------------------------

import Data.List
import Data.Numbers.Primes 

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Un número entero positivo se dirá 2-pandigital si en 
-- su cuadrado aparecen todos los dígitos del 0 al 9, al menos una vez.
-- Por ejemplo, 100287 es 2-pandigital porque 100287^2=10057482369.

-- Definir la lista infinita
--    dosPandigitales :: [Integer]
-- cuyos elementos son los números 2-pandigitales. Por ejemplo,
-- take 10 dosPandigitales ==
--   [32043,32286,33144,35172,35337,35757,35853,37176,37905,38772]
-- ---------------------------------------------------------------------

dosPandigitales :: [Integer]
dosPandigitales = filter esPandigital [1..]

esPandigital :: Integer -> Bool
esPandigital x = all (`elem` xs) "0123456789"
  where xs = show (x^2)

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Calcular el menor número 2-pandigital que sea primo.
-- ---------------------------------------------------------------------

-- El cálculo es
--    ghci> head (filter isPrime dosPandigitales)
--    101723

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--   coincidencias :: Eq a => Int -> [a] -> [a] -> Bool
-- tal que (coincidencias k xs ys) se verifica si las listas xs e ys
-- coinciden en, exactamente, k de sus posiciones. Por ejemplo,
--    coincidencias 7 "salamanca" "salamandra"  ==  True
-- ---------------------------------------------------------------------

-- 1ª solución
coincidencias1 :: Eq a => Int -> [a] -> [a] -> Bool
coincidencias1 n [] _                      = n == 0
coincidencias1 n _ []                      = n == 0
coincidencias1 n (x:xs) (y:ys) | x == y    = coincidencias1 (n-1) xs ys
                               | otherwise = coincidencias1 n xs ys

-- 2ª solución
coincidencias2 :: Eq a => Int -> [a] -> [a] -> Bool
coincidencias2 n xs ys =
  length (filter (\(a,b) -> a == b) (zip xs ys)) == n

-- 3ª solución
coincidencias3 :: Eq a => Int -> [a] -> [a] -> Bool
coincidencias3 n xs ys =
  length (filter (uncurry (==)) (zip xs ys)) == n

-- ----------------------------------------------------------------------
-- Ejercicio 3. Una lista de listas xss se dirá encadenada si todos sus
-- elementos son no vacíos, y el máximo de cada elemento coincide con el
-- mínimo del siguiente. 
--
-- Definir la función
--    encadenada :: Ord a => [[a]] -> Bool
-- tal que (encadenada xss) se verifica si xss es encadenada. Por
-- ejemplo,
--    encadenada [[2,1],[2,5,3],[6,5]]  ==  True
--    encadenada [[2,1],[2,0,3],[6,5]]  ==  False
--    encadenada [[2,1],[],[6,5]]       ==  False
-- ---------------------------------------------------------------------

-- 1ª solución
encadenada1 :: Ord a => [[a]] -> Bool
encadenada1 xss =
     all (not . null) xss 
  && and [maximum xs == minimum ys | (xs,ys) <-  zip xss (tail xss)]
                  
-- 2ª solución
encadenada2 :: Ord a => [[a]] -> Bool
encadenada2 []   = True
encadenada2 [xs] = not (null xs)
encadenada2 (xs:ys:zss) =
      not (null xs)
   && not (null ys)
   && maximum xs == minimum ys
   && encadenada2 (ys:zss)

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Representamos los árboles binarios mediante el tipo de
-- dato 
--    data Arbol a = H a | N a (Arbol a) (Arbol a)
--      deriving (Show, Eq)
-- Por ejemplo, el árbol 
--         4 
--        / \
--       /   \
--      3     7
--           / \
--          1   6
--         / \
--        0   2
-- se define de la siguiente forma
--    ejArbol :: Arbol Int
--    ejArbol = N 4 (H 3) (N 7 (N 1 (H 0) (H 2)) (H 6))
-- 
-- Definir la función
--    hojasProf :: Arbol a -> [a]
-- tal que (hojasProf t) es la lista de los pares formados por las hojas
-- de t junto con su profundidad. Por ejemplo, 
--    hojasProf (H 7)   == [(7,0)]
--    hojasProf ejArbol == [(3,1),(0,3),(2,3),(6,2)]
-- ---------------------------------------------------------------------

data Arbol a = H a | N a (Arbol a) (Arbol a)
  deriving (Show, Eq)

ejArbol :: Arbol Int
ejArbol = N 4 (H 3) (N 7 (N 1 (H 0) (H 2)) (H 6))

hojasProf :: Arbol a -> [(a,Int)]
hojasProf a = aux a 0
  where aux (H a) n     = [(a,n)]
        aux (N a i d) n = aux i (n+1) ++ aux d (n+1)

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función
--    hojasMasProfundas:: Arbol a -> [a]
-- tal que (hojasMasProfundas t) es la lista de las hojas más profundas
-- del árbol t. Por ejemplo, 
--    hojasMasProfundas (H 7)   == [7]
--    hojasMasProfundas ejArbol == [0,2]
-- ---------------------------------------------------------------------

-- 1ª solución
hojasMasProfundas :: Arbol a -> [a]
hojasMasProfundas a = [i | (i,p) <- hojasProf a
                         , p == profundidad a]

profundidad :: Arbol a -> Int
profundidad (H a)     = 0
profundidad (N a i d) = 1 + max (profundidad i) (profundidad d)

-- 2ª solución
hojasMasProfundas2 :: Arbol a -> [a]
hojasMasProfundas2 a = [h | (h,p) <- hps
                          , p == m]
  where hps = hojasProf a
        m   = maximum [p | (_,p) <- hps]
