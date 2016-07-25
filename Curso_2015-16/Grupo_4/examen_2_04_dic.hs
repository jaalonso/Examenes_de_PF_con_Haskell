-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 2º examen de evaluación continua (4 de diciembre de 2015)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Numbers.Primes
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    listasDecrecientesDesde :: Int -> [[Int]]
-- tal que (listasDecrecientesDesde n) es la lista de las sucesiones
-- estrictamente decrecientes cuyo primer elemento es n. Por ejemplo,
--    ghci> listasDecrecientesDesde 2
--    [[2],[2,1],[2,1,0],[2,0]]
--    ghci> listasDecrecientesDesde 3
--    [[3],[3,2],[3,2,1],[3,2,1,0],[3,2,0],[3,1],[3,1,0],[3,0]]
-- ---------------------------------------------------------------------

-- 1ª solución
listasDecrecientesDesde :: Int -> [[Int]]
listasDecrecientesDesde 0 = [[0]]
listasDecrecientesDesde n =
    [n] : [n:ys | m <- [n-1,n-2..0], ys <- listasDecrecientesDesde m]

-- 2ª solución
listasDecrecientesDesde2 :: Int -> [[Int]]
listasDecrecientesDesde2 n = 
    [n : xs | xs <- subsequences [n-1,n-2..0]]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Una propiedad del 2015 es que la suma de sus dígitos
-- coincide con el número de sus divisores; en efecto, la suma de sus
-- dígitos es 2+0+1+5=8 y tiene 8 divisores (1, 5, 13, 31, 65, 155, 403
-- y 2015). 
--    
-- Definir la sucesión
--    especiales :: [Int]
-- formada por los números n tales que la suma de los dígitos de n
-- coincide con el número de divisores de n. Por ejemplo,
--    take 12 especiales == [1,2,11,22,36,84,101,152,156,170,202,208]
--
-- Usando la sucesión, calcular cuál será el siguiente al 2015 que
-- cumplirá la propiedad.
-- ---------------------------------------------------------------------

especiales :: [Int]
especiales = [n | n <- [1..], sum (digitos n) == length (divisores n)]

digitos :: Int -> [Int]
digitos n = [read [d] | d <- show n]

divisores :: Int -> [Int]
divisores n = n : [x | x <- [1..n `div` 2], n `mod` x == 0]

-- El cálculo del siguiente al 2015 que cumplirá la propiedad es 
--    ghci> head (dropWhile (<=2015) especiales)
--    2101

-- ---------------------------------------------------------------------
-- Ejercicio 3. Las expresiones aritméticas se pueden representar como
-- árboles con números en las hojas y operaciones en los nodos. Por
-- ejemplo, la expresión "9-2*4" se puede representar por el árbol
--      - 
--     / \
--    9   *
--       / \
--      2   4
-- 
-- Definiendo el tipo de dato Arbol por 
--    data Arbol = H Int | N (Int -> Int -> Int) Arbol Arbol
-- la representación del árbol anterior es
--    N (-) (H 9) (N (*) (H 2) (H 4))
--
-- Definir la función
--    valor :: Arbol -> Int
-- tal que (valor a) es el valor de la expresión aritmética
-- correspondiente al árbol a. Por ejemplo,
--    valor (N (-) (H 9) (N (*) (H 2) (H 4)))    ==  1
--    valor (N (+) (H 9) (N (*) (H 2) (H 4)))    ==  17
--    valor (N (+) (H 9) (N (div) (H 4) (H 2)))  ==  11
--    valor (N (+) (H 9) (N (max) (H 4) (H 2)))  ==  13
-- ---------------------------------------------------------------------

data Arbol = H Int | N (Int -> Int -> Int) Arbol Arbol

valor :: Arbol -> Int
valor (H x)     = x
valor (N f i d) = f (valor i) (valor d)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Un número x cumple la propiedad de Roldán si la suma de
-- x y el primo que sigue a x es un número primo. Por ejemplo, el número
-- 8 es un número de Roldán porque su siguiente primo es 11 y
-- 8+11=19 es primo. El 12 no es un número de Roldán porque su siguiente
-- primo es 13 y 12+13=25 no es primo.
-- 
-- Definir la sucesión
--     roldanes :: [Integer]
-- cuyo elementos son los números de Roldán. Por ejemplo, 
--    ghci> take 20 roldanes
--    [0,1,2,6,8,14,18,20,24,30,34,36,38,48,50,54,64,68,78,80]
--    ghci> roldanes3 !! 2015
--    18942
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

roldanes :: [Integer]
roldanes = 0: 1: [x | x <- [2,4..], primo (x + siguientePrimo x)]

primo :: Integer -> Bool
primo x = [y | y <- [1..x], x `rem` y == 0] == [1,x]

siguientePrimo :: Integer -> Integer
siguientePrimo x = head [y | y <- [x+1..], primo y]

-- 2ª definición (por recursión)
-- =============================

roldanes2 :: [Integer]
roldanes2 = 0 : 1 : 2: aux [2,4..] primos where
    aux (x:xs) (y:ys) 
        | y < x                = aux (x:xs) ys
        | (x+y) `pertenece` ys = x : aux xs (y:ys)
        | otherwise            = aux xs (y:ys)
    pertenece x ys = x == head (dropWhile (<x) ys)

primos :: [Integer]
primos = 2 : [x | x <- [3,5..], primo x]

-- 3ª definición (con la librería de primos)
-- =========================================

roldanes3 :: [Integer]
roldanes3 = 0: 1: [x | x <- [2,4..], isPrime (x + siguientePrimo3 x)]

siguientePrimo3 x = head [y | y <- [x+1..], isPrime y]

-- 4ª definición (por recursión con la librería de primos)
-- =======================================================

roldanes4 :: [Integer]
roldanes4 = 0 : 1 : 2: aux [2,4..] primes where
    aux (x:xs) (y:ys) 
        | y < x                = aux (x:xs) ys
        | (x+y) `pertenece` ys = x : aux xs (y:ys)
        | otherwise            = aux xs (y:ys)
    pertenece x ys = x == head (dropWhile (<x) ys)

-- 5ª definición
-- =============

roldanes5 :: [Integer]
roldanes5 = [a | q <- primes, 
                let p = siguientePrimo3 (q `div` 2),
                let a = q-p,
                siguientePrimo3 a == p]

-- 6ª definición
-- =============

roldanes6 :: [Integer]
roldanes6 = [x | (x,y) <- zip [0..] ps, isPrime (x+y)]
    where ps = 2:2:concat (zipWith f primes (tail primes))
          f p q = genericReplicate (q-p) q

-- 7ª definición
-- =============

roldanes7 :: [Integer]
roldanes7 = 0:1:(aux primes (tail primes) primes)
    where aux (x:xs) (y:ys) zs
              | null rs   = aux xs ys zs2
              | otherwise = [r-y | r <- rs] ++ (aux xs ys zs2)
              where a = x+y
                    b = 2*y-1
                    zs1 = takeWhile (<=b) zs
                    rs = [r | r <- [a..b], r `elem` zs1]
                    zs2 = dropWhile (<=b) zs

-- Comparación de eficiencia                                        --
--    ghci> :set +s
--    
--    ghci> roldanes !! 700
--    5670
--    (12.72 secs, 1245938184 bytes)
--    
--    ghci> roldanes2 !! 700
--    5670
--    (8.01 secs, 764775268 bytes)
-- 
--    ghci> roldanes3 !! 700
--    5670
--    (0.22 secs, 108982640 bytes)
--    
--    ghci> roldanes4 !! 700
--    5670
--    (0.20 secs, 4707384 bytes)
--    
--    ghci> roldanes5 !! 700
--    5670
--    (0.17 secs, 77283064 bytes)
--    
--    ghci> roldanes6 !! 700
--    5670
--    (0.08 secs, 31684408 bytes)
--    
--    ghci> roldanes7 !! 700
--    5670
--    (0.03 secs, 4651576 bytes)
--    
--    ghci> roldanes3 !! 2015
--    18942
--    (1.78 secs, 1,065,913,952 bytes)
--    
--    ghci> roldanes4 !! 2015
--    18942
--    (2.85 secs, 0 bytes)
--    
--    ghci> roldanes5 !! 2015
--    18942
--    (1.17 secs, 694,293,520 bytes)
--    
--    ghci> roldanes6 !! 2015
--    18942
--    (0.48 secs, 248,830,328 bytes)
--    
--    ghci> roldanes7 !! 2015
--    18942
--    (0.11 secs, 0 bytes)


