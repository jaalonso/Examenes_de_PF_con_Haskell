-- Informática (1º del Grado en Matemáticas, Grupos 1, 2 y 3)
-- 3º examen de evaluación continua (22 de enero de 2019)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    divisoresConFinal :: Integer -> Integer -> [Integer]
-- tal que (divisoresConFinal n m) es la lista de los divisores de n
-- cuyos dígitos finales coincide con m. por ejemplo,
--    divisoresConFinal 84 4    ==  [4,14,84]
--    divisoresConFinal 720 20  ==  [20,120,720]
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

divisoresConFinal :: Integer -> Integer -> [Integer]
divisoresConFinal n m = 
  [x | x <- [1..n], n `rem` x == 0, final x m]

final :: Integer -> Integer -> Bool
final x y = take n xs == ys
    where xs = reverse (show x)
          ys = reverse (show y)
          n  = length ys

-- 2ª solución
-- ===========

divisoresConFinal2 :: Integer -> Integer -> [Integer]
divisoresConFinal2 n m = 
    [x | x <- [1..n], n `rem` x == 0, show m `isSuffixOf` show x]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    alternativa :: (a -> b) -> (a -> b) -> [a] -> [b]
-- tal que (alternativa f g xs) es la lista obtenida aplicando
-- alternativamente las funciones f y g a los elementos de xs. por
-- ejemplo, 
--    alternativa (+1)  (+10) [1,2,3,4]    ==  [2,12,4,14]
--    alternativa (+10) (*10) [1,2,3,4,5]  ==  [11,20,13,40,15]
-- ---------------------------------------------------------------------

-- 1ª solución
alternativa :: (a -> b) -> (a -> b) -> [a] -> [b]
alternativa f g []     = []
alternativa f g (x:xs) = f x : alternativa g f xs

-- 2ª solución
alternativa2 :: (a -> b) -> (a -> b) -> [a] -> [b]
alternativa2 f g xs = 
  [h x | (h,x) <- zip (cycle [f,g]) xs]

-- ---------------------------------------------------------------------
-- Ejercicio 3. La sucesión de los primeros números de Fibonacci es
--    0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, ...
-- el fibonacci más cercano a un número x es el menor elemento y de la
-- sucesión de Fibonacci tal que el valor absoluto de la diferencia
-- entre x e y es la menor posible. por ejemplo, 
-- + el fibonacci más cercano a 16 es 13 porque |16-13| < |16-21|
-- + el fibonacci más cercano a 17 es 13 porque |17-13| = |17-21| y 17 < 21
-- + el fibonacci más cercano a 18 es 21 porque |18-13| > |18-21|
-- + el fibonacci más cercano a 21 es 21 porque 21 es un número de fibonacci.
--
-- Definir la función
--    fibonacciMasCercano :: Integer -> Integer
-- tal que (fibonacciMasCercano n) es el número de fibonacci más cercano
-- a n. por ejemplo, 
--    fibonacciMasCercano 16    ==  13
--    fibonacciMasCercano 17    ==  13
--    fibonacciMasCercano 18    ==  21
--    fibonacciMasCercano 21    ==  21
--    fibonacciMasCercano 2019  ==  1597
-- ---------------------------------------------------------------------

fibonacciMasCercano :: Integer -> Integer
fibonacciMasCercano n
  | b == n                 = n
  | abs (n-a) <= abs (n-b) = a
  | otherwise              = b
  where (xs,b:ys) = span (<n) fibs
        a         = last xs

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Los árboles se pueden representar mediante el siguiente
-- tipo de datos 
--    data Arbol a = N a [Arbol a]
--      deriving Show
-- por ejemplo, los árboles
--      1               3
--     / \             /|\ 
--    2   3           / | \
--        |          5  4  7
--        4          |     /\ 
--                   6    2  1
-- se representan por
--    ej1, ej2 :: Arbol Int
--    ej1 = N 1 [N 2 [],N 3 [N 4 []]]
--    ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]
-- 
-- Definir la función 
--    ramas :: Arbol b -> [[b]]
-- tal que (ramas a) es la lista de las ramas del árbol a. Por ejemplo,
--    ramas ej1  ==  [[1,2],[1,3,4]]
--    ramas ej2  ==  [[3,5,6],[3,4],[3,7,2],[3,7,1]]
-- ---------------------------------------------------------------------

data Arbol a = N a [Arbol a]
  deriving Show

ej1, ej2 :: Arbol Int
ej1 = N 1 [N 2 [],N 3 [N 4 []]]
ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]

-- 1ª solución
ramas :: Arbol b -> [[b]]
ramas (N x []) = [[x]]
ramas (N x as) = [x : xs | a <- as, xs <- ramas a]

-- 2ª solución
ramas2 :: Arbol b -> [[b]]
ramas2 (N x []) = [[x]]
ramas2 (N x as) = concat (map (map (x:)) (map ramas2 as))

-- 3ª solución
ramas3 :: Arbol b -> [[b]]
ramas3 (N x []) = [[x]]
ramas3 (N x as) = concatMap (map (x:)) (map ramas3 as)
