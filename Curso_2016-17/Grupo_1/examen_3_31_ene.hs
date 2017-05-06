-- Informática (1º del Grado en Matemáticas)
-- 3º examen de evaluación continua (31 de enero de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List 
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. [2.5 puntos] Definir la lista
--    ternasCoprimas :: [(Integer,Integer,Integer)]
-- cuyos elementos son ternas de primos relativos (a,b,c) tales que
-- a < b y a + b = c. Por ejemplo,
--    take 7 ternasCoprimas
--    [(1,2,3),(1,3,4),(2,3,5),(1,4,5),(3,4,7),(1,5,6),(2,5,7)]
--    ternasCoprimas !! 300000
--    (830,993,1823) 
-- ---------------------------------------------------------------------

-- 1ª solución
ternasCoprimas :: [(Integer,Integer,Integer)]
ternasCoprimas = [(x,y,z) | y <- [1..]
                          , x <-[1..y-1]
                          , gcd x y == 1 
                          , let z = x+y
                          , gcd x z == 1
                          , gcd y z == 1]

-- 2ª solución (Teniendo en cuenta que es suficiente que gcd x y == 1).
ternasCoprimas2 :: [(Integer,Integer,Integer)]
ternasCoprimas2 =
  [(x,y,x+y) | y <- [1..]
             , x <- [1..y-1]
             , gcd x y == 1]

-- ---------------------------------------------------------------------
-- Ejercicio 2. [2.5 puntos] Un entero positivo n es pandigital en base
-- b si su expresión en base b contiene todos los dígitos de 0 a b-1 al
-- menos una vez. Por ejemplo,
--    el 2 es pandigital en base 2 porque 2 en base 2 es 10,
--    el 11 es pandigital en base 3 porque 11 en base 3 es 102 y
--    el 75 es pandigital en base 4 porque 75 en base 4 es 1023.
--
-- Un número n es super pandigital de orden m si es pandigital en todas
-- las bases desde 2 hasta m. Por ejemplo, 978 es super pandigital de
-- orden 5 pues
--    en base 2 es: 1111010010
--    en base 3 es: 1100020
--    en base 4 es: 33102
--    en base 5 es: 12403
--
-- Definir la función
--    superPandigitales :: Integer -> [Integer]
-- tal que (superPandigitales m) es la lista de los números super
-- pandigitales de orden m. Por ejemplo,
--    take 3 (superPandigitales 3) == [11,19,21]
--    take 3 (superPandigitales 4) == [75,99,114]
--    take 3 (superPandigitales 5) == [978,1070,1138]  
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

superPandigitales :: Integer -> [Integer]
superPandigitales m =
  [n | n <- [1..]
     , and [pandigitalBase b n | b <- [2..m]]]

-- (pandigitalBase b n) se verifica si n es pandigital en base la base
-- b. Por ejemplo,
--    pandigitalBase 4 75  ==  True
--    pandigitalBase 4 76  ==  False
pandigitalBase :: Integer -> Integer -> Bool
pandigitalBase b n = [0..b-1] `esSubconjunto` enBase b n

-- (enBase b n) es la lista de los dígitos de n en base b. Por ejemplo,
--    enBase 4 75  ==  [3,2,0,1]
--    enBase 4 76  ==  [0,3,0,1]
enBase :: Integer -> Integer -> [Integer]
enBase b n | n < b     = [n]
           | otherwise = n `mod` b : enBase b (n `div` b)
 
-- (esSubconjunto xs ys) se verifica si xs es un subconjunto de ys. Por
-- ejemplo,
--    esSubconjunto [1,5] [5,2,1]  ==  True
--    esSubconjunto [1,5] [5,2,3]  ==  False
esSubconjunto :: Eq a => [a] -> [a] -> Bool
esSubconjunto xs ys = all (`elem` ys) xs

-- 2ª definición
-- =============

superPandigitales2 :: Integer -> [Integer]
superPandigitales2 a = foldl' f ys [a-1,a-2..2]
  where ys = filter (pandigitalBase a) [1..]
        f xs b = filter (pandigitalBase b) xs
  
pandigitalBase2 :: Integer -> Integer -> Bool
pandigitalBase2 b n = sort (nub xs) == [0..(b-1)]
  where xs = enBase b n

-- ---------------------------------------------------------------------
-- Ejercicio 3. [2.5 puntos] Definir la sucesión
--    sucFinalesFib :: [(Integer,Integer)]
-- cuyos elementos son los pares (n,x), donde x es el n-ésimo término de
-- la sucesión de Fibonacci, tales que la terminación de x es n. Por
-- ejemplo,
--    ghci> take 6 sucFinalesFib
--    [(0,0),(1,1),(5,5),(25,75025),(29,514229),(41,165580141)]
--    ghci> head [(n,x) | (n,x) <- sucFinalesFib, n > 200]
--    (245,712011255569818855923257924200496343807632829750245)
--    ghci> head [n | (n,_) <- sucFinalesFib, n > 10^4]
--    10945
-- ---------------------------------------------------------------------

sucFinalesFib :: [(Integer, Integer)]
sucFinalesFib =
  [(n, fib n) | n <- [0..]
              , show n `isSuffixOf` show (fib n)]
 
-- (fib n) es el n-ésimo término de la sucesión de Fibonacci.
fib :: Integer -> Integer
fib n = sucFib `genericIndex` n

-- sucFib es la sucesión de Fibonacci.
sucFib :: [Integer]
sucFib = 0 : 1 : zipWith (+) sucFib (tail sucFib)

-- 2ª definición de sucFib
sucFib2 :: [Integer]
sucFib2 = 0 : scanl (+) 1 sucFib2

-- ---------------------------------------------------------------------
-- Ejercicio 4. [2.5 puntos] Los árboles se pueden representar mediante
-- el siguiente tipo de datos 
--    data Arbol a = N a [Arbol a]
--      deriving Show
-- Por ejemplo, los árboles
--      1         1             1          
--     / \       / \           / \   
--    8   3     5   3         5   3  
--        |        /|\       /|\  |   
--        4       4 7 6     4 7 6 7
-- se representan por
--    ej1, ej2, ej3 :: Arbol Int
--    ej1 = N 1 [N 8 [],N 3 [N 4 []]]
--    ej2 = N 1 [N 5 [], N 3 [N 4 [], N 7 [], N 6 []]]
--    ej3 = N 1 [N 5 [N 4 [], N 7 [], N 6 []], N 3 [N 7 []]]
--
-- Definir la función
--    minimaSuma :: Arbol Int -> Int
-- tal que (minimaSuma a) es el mínimo de las sumas de las ramas del
-- árbol a. Por ejemplo,
--    minimaSuma ej1  ==  8
--    minimaSuma ej2  ==  6
--    minimaSuma ej3  ==  10
-- ---------------------------------------------------------------------

data Arbol a = N a [Arbol a]
  deriving Show
 
ej1, ej2, ej3 :: Arbol Int
ej1 = N 1 [N 8 [],N 3 [N 4 []]]
ej2 = N 1 [N 5 [], N 3 [N 4 [], N 7 [], N 6 []]]
ej3 = N 1 [N 5 [N 4 [], N 7 [], N 6 []], N 3 [N 7 []]] 
 
-- 1ª definición
-- =============

minimaSuma :: Arbol Int -> Int
minimaSuma a = minimum [sum xs | xs <- ramas a]

-- (ramas a) es la lista de las ramas del árbol a. Por ejemplo,
--    ghci> ramas (N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]])
--    [[3,5,6],[3,4],[3,7,2],[3,7,1]]
ramas :: Arbol b -> [[b]]
ramas (N x []) = [[x]]
ramas (N x as) = [x : xs | a <- as, xs <- ramas a]
 
-- 2ª definición (Como composición de funciones):
-- ==============================================

minimaSuma2 :: Arbol Int -> Int
minimaSuma2 = minimum . map sum . ramas
  
-- 3ª definición
-- =============
 
minimaSuma3 :: Arbol Int -> Int
minimaSuma3 (N x []) = x
minimaSuma3 (N x as) = x + minimum [minimaSuma3 a | a <- as]

-- 4ª definición
-- =============
 
minimaSuma4 :: Arbol Int -> Int
minimaSuma4 (N x []) = x
minimaSuma4 (N x as) = x + minimum (map minimaSuma4 as)
