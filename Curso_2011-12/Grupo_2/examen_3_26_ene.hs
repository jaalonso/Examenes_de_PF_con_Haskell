-- Informática (1º del Grado en Matemáticas, Grupo 2)
-- 3º examen de evaluación continua (26 de enero de 2012)
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función
--    sumatorio :: (Integer -> Integer) -> Integer -> Integer -> Integer
-- tal que (sumatorio f m n) es la suma de f(x) desde x=m hasta x=n. Por
-- ejemplo, 
--    sumatorio (^2) 5 10     == 355
--    sumatorio abs (-5) 10   == 70
--    sumatorio (^2) 3 100000 == 333338333349995
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
sumatorioC :: (Integer -> Integer) -> Integer -> Integer -> Integer
sumatorioC f m n = sum [f x | x <- [m..n]]

-- 2ª definición (por recursión):
sumatorioR :: (Integer -> Integer) -> Integer -> Integer -> Integer
sumatorioR f m n = aux m 0
    where aux k ac | k > n     = ac
                   | otherwise = aux (k+1) (ac + f k)

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función 
--    sumaPred :: Num a => (a -> Bool) -> [a] -> a
-- tal que (sumaPred p xs) es la suma de los elementos de xs que
-- verifican el predicado p. Por ejemplo:
--    sumaPred even [1..1000]   == 250500 
--    sumaPred even [1..100000] == 2500050000          
-- ---------------------------------------------------------------------

-- 1ª definición (por composición, usando funciones de orden superior):          
sumaPred :: Num a => (a -> Bool) -> [a] -> a
sumaPred p = sum . filter p
          
-- 2ª definición (por recursión):
sumaPredR :: Num a => (a -> Bool) -> [a] -> a
sumaPredR _ [] = 0
sumaPredR p (x:xs) | p x       = x + sumaPredR p xs
                   | otherwise = sumaPredR p xs

-- 3ª definición (por plegado por la derecha, usando foldr):                
sumaPredPD :: Num a => (a -> Bool) -> [a] -> a
sumaPredPD p = foldr f 0 
    where f x y | p x       = x + y
                | otherwise = y

-- 4ª definición (por recursión final):
sumaPredRF :: Num a => (a -> Bool) -> [a] -> a
sumaPredRF p xs = aux xs 0
    where aux []     a = a
          aux (x:xs) a | p x       = aux xs (x+a)
                       | otherwise = aux xs a

-- 5ª definición (por plegado por la izquierda, usando foldl):                
sumaPredPI p = foldl f 0 
    where f x y | p y       = x + y
                | otherwise = x

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Representamos una relación binaria sobre un conjunto
-- como un par formado por:
--   * una lista, que representa al conjunto, y
--   * una lista de pares, que forman la relación
-- En los ejemplos usaremos las siguientes relaciones
--    r1, r2, r3,r4 :: ([Int],[(Int, Int)])
--    r1 = ([1..9],[(1,3), (2,6), (8,9), (2,7)])
--    r2 = ([1..9],[(1,3), (2,6), (8,9), (3,7)])
--    r3 = ([1..9],[(1,3), (2,6), (6,2), (3,1), (4,4)])
--    r4 = ([1..3],[(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)])
-- 
-- Definir la función 
--    reflexiva :: Eq a => ([a],[(a,a)]) -> Bool
-- tal que (reflexiva r) se verifica si r es una relación reflexiva. Por
-- ejemplo, 
--    reflexiva r1 == False
--    reflexiva r4 == True
-- ---------------------------------------------------------------------

r1, r2, r3,r4 :: ([Int],[(Int, Int)])
r1 = ([1..9],[(1,3), (2,6), (8,9), (2,7)])
r2 = ([1..9],[(1,3), (2,6), (8,9), (3,7)])
r3 = ([1..9],[(1,3), (2,6), (6,2), (3,1), (4,4)])
r4 = ([1..3],[(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)])

reflexiva :: Eq a => ([a],[(a,a)]) -> Bool
reflexiva (us,ps) = and [(x,x) `elem` ps | x <- us]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función 
--    simetrica :: Eq a => ([a],[(a,a)]) -> Bool
-- tal que (simetrica r) se verifica si r es una relación simétrica. Por
-- ejemplo, 
--    simetrica r1 == False
--    simetrica r3 == True
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
simetricaC :: Eq a => ([a],[(a,a)]) -> Bool
simetricaC (x,r) =
    null [(x,y) | (x,y) <- r, (y,x) `notElem` r]

-- 2ª definición (por recursión):
simetrica :: Eq a => ([a],[(a,a)]) -> Bool
simetrica r = aux (snd r)
    where aux [] = True
          aux ((x,y):s) | x == y    = aux s
                        | otherwise = elem (y,x) s && aux (delete (y,x) s)

-- ---------------------------------------------------------------------
-- Ejercicio 3. (Problema 347 del proyecto Euler) El mayor entero menor
-- o igual que 100 que sólo es divisible por los primos 2 y 3, y sólo
-- por ellos, es 96, pues 96 = 3*32 = 3*2^5.  
-- 
-- Dados dos primos distintos p y q, sea M(p,q,n) el mayor entero menor
-- o igual que n sólo divisible por ambos p y q; o M(p,q,N)=0 si tal
-- entero no existe. Por ejemplo:
--    M(2,3,100)  = 96
--    M(3,5,100)  = 75 y no es 90 porque 90 es divisible por 2, 3 y 5
--                     y tampoco es 81 porque no es divisible por 5.
--    M(2,73,100) = 0 porque no existe un entero menor o igual que 100 que
--                    sea divisible por 2 y por 73.
-- 
-- Definir la función 
--    mayorSoloDiv :: Int -> Int -> Int -> Int
-- tal que (mayorSoloDiv p q n) es M(p,q,n). Por ejemplo,
--    mayorSoloDiv 2 3 100  == 96
--    mayorSoloDiv 3 5 100  == 75
--    mayorSoloDiv 2 73 100 == 0
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

mayorSoloDiv :: Int -> Int -> Int -> Int
mayorSoloDiv p q n 
    | null xs   = 0
    | otherwise = head xs
    where xs = [x | x <- [n,n-1..1], divisoresPrimos x == sort [p,q]]

-- (divisoresPrimos n) es la lista de los divisores primos de x. Por
-- ejemplo, 
--    divisoresPrimos 180  ==  [2,3,5]
divisoresPrimos :: Int -> [Int]
divisoresPrimos n = [x | x <- [1..n], rem n x == 0, esPrimo x]

-- (esPrimo n) se verifica si n es primo. Por ejemplo,
--    esPrimo 7  ==  True
--    esPrimo 9  ==  False
esPrimo :: Int -> Bool
esPrimo n = [x | x <- [1..n], rem n x == 0] == [1,n]

-- 2ª solución:
-- ============

mayorSoloDiv2 :: Int -> Int -> Int -> Int
mayorSoloDiv2 p q n 
    | null xs = 0
    | otherwise = head xs
    where xs = [x | x <- [n,n-1..1], soloDivisible p q x]

-- (soloDivisible p q x) se verifica si x es divisible por los primos p
-- y por q, y sólo por ellos. Por ejemplo,
--    soloDivisible 2 3 96  ==  True
--    soloDivisible 3 5 90  ==  False
--    soloDivisible 3 5 75  ==  True
soloDivisible :: Int -> Int -> Int -> Bool
soloDivisible p q x =
    mod x p == 0 && mod x q == 0 && aux x
    where aux x | x `elem` [p,q] = True
                | mod x p == 0   = aux (div x p)
                | mod x q == 0   = aux (div x q)
                | otherwise      = False

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Dado un número n, calculamos la suma de sus divisores
-- propios reiteradamente hasta que quede un número primo. Por ejemplo,
-- 
--    n  | divisores propios                    | suma de div. propios
-- ------+--------------------------------------+---------------------
--   30  | [1,2,3,5,6,10,15]                    |  42
--   42  | [1,2,3,6,7,14,21]                    |  54
--   54  | [1,2,3,6,9,18,27]                    |  66
--   66  | [1,2,3,6,11,22,33]                   |  78
--   78  | [1,2,3,6,13,26,39]                   |  90
--   90  | [1,2,3,5,6,9,10,15,18,30,45]         | 144
--  144  | [1,2,3,4,6,8,9,12,16,18,24,36,48,72] | 259
--  259  | [1,7,37]                             |  45
--   45  | [1,3,5,9,15]                         |  33
--   33  | [1,3,11]                             |  15
--   15  | [1,3,5]                              |   9
--    9  | [1,3]                                |   4
--    4  | [1,2]                                |   3
--    3 (es primo)
-- 
-- Definir una función 
--    sumaDivReiterada :: Int -> Int
-- tal que (sumaDivReiterada n) calcule reiteradamente la suma de los
-- divisores propios hasta que se llegue a un número primo. Por ejemplo, 
--    sumaDivReiterada 30   == 3
--    sumaDivReiterada 52   == 3
--    sumaDivReiterada 5289 == 43
--    sumaDivReiterada 1024 == 7
-- ---------------------------------------------------------------------

sumaDivReiterada :: Int -> Int
sumaDivReiterada n 
    | esPrimo n   = n
    | otherwise = sumaDivReiterada (sumaDivPropios n)

-- (sumaDivPropios n) es la suma de los divisores propios de n. Por
-- ejemplo, 
--    sumaDivPropios 30  ==  42
sumaDivPropios :: Int -> Int
sumaDivPropios n = sum [k | k <- [1..n-1], rem n k == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. ¿Hay números naturales para los que la función
-- anterior no termina? Si crees que los hay, explica por qué y
-- encuentra los tres primeros  números para los que la función anterior
-- no terminaría. En caso contrario, justifica por qué termina siempre.
-- .....................................................................

-- Basta observar que si n es igual a la suma de sus divisores propios
-- (es decir, si n es un número perfecto), la función no termina porque
-- vuelve a hacer la suma reiterada de sí mismo otra vez. Luego, la
-- función no termina para los números perfectos. 

-- Los números perfectos se definen por
esPerfecto :: Int -> Bool
esPerfecto n = sumaDivPropios n == n

-- Los 3 primeros números perfectos se calcula por
--    ghci> take 3 [n | n <- [1..], esPerfecto n]
--    [6,28,496]

-- Por tanto, los tres primeros números para los que el algoritmo no
-- termina son los 6, 28 y 496.

-- Se puede comprobar con 
--    ghci> sumaDivReiterada 6
--      C-c C-cInterrupted.
