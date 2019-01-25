-- Informática (1º del Grado en Matemáticas, Grupos 4 y 5)
-- 3º examen de evaluación continua (22 de enero de 2019)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    nParejas :: Ord a => [a] -> Int
-- tal que (nParejas xs) es el número de parejas de elementos iguales en
-- xs. Por rjemplo,
--    nParejas [1,2,2,1,1,3,5,1,2]        ==  3
--    nParejas [1,2,1,2,1,3,2]            ==  2
--    nParejas [1..2*10^6]                ==  0
--    nParejas2 ([1..10^6] ++ [1..10^6])  ==  1000000
-- En el primer ejemplos las parejas son (1,1), (1,1) y (2,2). En el
-- segundo ejemplo, las parejas son (1,1) y (2,2).
-- ---------------------------------------------------------------------

-- 1ª solución
nParejas :: Ord a => [a] -> Int
nParejas []     = 0
nParejas (x:xs) | x `elem` xs = 1 + nParejas (xs \\ [x])
                | otherwise   = nParejas xs

-- 2ª solución
nParejas2 :: Ord a => [a] -> Int
nParejas2 xs =
  sum [length ys `div` 2 | ys <- group (sort xs)]

-- 3ª solución
nParejas3 :: Ord a => [a] -> Int
nParejas3 = sum . map (`div` 2) . map length . group . sort

-- 4ª solución
nParejas4 :: Ord a => [a] -> Int
nParejas4 = sum . map ((`div` 2) . length) . group . sort

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    maximos :: Ord a => [a] -> [a]
-- tal que (maximos xs) es la lista de los elementos de xs que son
-- mayores que todos sus anteriores. Por ejemplo, 
--    maximos [1,-3,5,2,3,4,7,6,7]                         ==  [1,5,7]
--    maximos "bafcdegag"                                  ==  "bfg"
--    maximos (concat (replicate (10^6) "adxbcde")++"yz")  ==  "adxyz"
--    length (maximos [1..10^6])                           ==  1000000
-- ---------------------------------------------------------------------

-- 1ª solución
maximos :: Ord a => [a] -> [a]
maximos xs =
  [x | (ys,x) <- zip (inits xs) xs, all (<x) ys]

-- 2ª solución
maximos2 :: Ord a => [a] -> [a]
maximos2 [] = []
maximos2 (x:xs) = x : maximos2 (filter (>x) xs)

-- 3ª solución
maximos3 :: Ord a => [a] -> [a]
maximos3 [] = []
maximos3 (x:xs) = aux xs [x] x
    where aux [] zs _ = reverse zs
          aux (y:ys) zs m | y > m     = aux ys (y:zs) y
                          | otherwise = aux ys zs m 

-- 4ª solución
maximos4 :: Ord a => [a] -> [a]
maximos4 = nub . scanl1 max 

-- ---------------------------------------------------------------------
-- Ejercicio 3. La sucesión de los primeros factoriales es
--    1, 2, 6, 24, 120, 720, 5040, 40320, 362880, 3628800, ...
-- El factorial más cercano a un número x es el menor elemento y de la
-- sucesión de los factoriales tal que el valor absoluto de la
-- diferencia entre x e y es la menor posible. Por ejemplo,
-- + el factorial más cercano a 3 es 2 porque |3-2| < |3-6|
-- + el factorial más cercano a 4 es 2 porque |4-2| = |4-6| y 2 < 
-- + el factorial más cercano a 5 es 6 porque |5-2| > |5-6|
-- + el factorial más cercano a 5 es 6 porque 6 es un factorial.
--
-- Definir la función
--    factorialMasCercano :: Integer -> Integer
-- tal que (factorialMasCercano n) es el factorial más cercano a n. Por
-- ejemplo, 
--    factorialMasCercano 3  ==  2
--    factorialMasCercano 4  ==  2
--    factorialMasCercano 5  ==  6
--    factorialMasCercano 6  ==  6
--    factorialMasCercano 2019  ==  720
-- ---------------------------------------------------------------------

factorialMasCercano :: Integer -> Integer
factorialMasCercano n
  | b == n                 = n
  | abs (n-a) <= abs (n-b) = a
  | otherwise              = b
  where (xs,b:ys) = span (<n) factoriales
        a = last xs

factoriales :: [Integer]
factoriales = scanl1 (*) [1..]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Las expresiones aritméticas. generales se contruyen con
-- las sumas generales (sumatorios) y productos generales (productorios).
-- Su tipo es 
--    data Expresion = N Int
--                   | S [Expresion]
--                   | P [Expresion]
--      deriving Show
-- Por ejemplo, la expresión (2 * (1 + 2 + 1) * (2 + 3)) + 1 se
-- representa por S [P [N 2, S [N 1, N 2, N 1], S [N 2, N 3]], N 1]
--
-- Definir la función
--    valor :: Expresion -> Int
-- tal que (valor e) es el valor de la expresión e. Por ejemplo,
--    λ> valor (S [P [N 2, S [N 1, N 2, N 1], S [N 2, N 3]], N 1])
--    41
-- ---------------------------------------------------------------------

data Expresion = N Int
               | S [Expresion]
               | P [Expresion]
  deriving Show

-- 1ª solución
valor :: Expresion -> Int
valor (N x)  = x
valor (S es) = sum (map valor es)
valor (P es) = product (map valor es)

-- 2ª solución
valor2 :: Expresion -> Int
valor2 (N x)  = x
valor2 (S es) = sum [valor2 e | e <- es]
valor2 (P es) = product [valor2 e | e <- es]
