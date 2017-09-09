-- Informática (1º del Grado en Matemáticas)
-- 6º examen de evaluación continua (12 de junio de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Librerías auxiliares
-- ---------------------------------------------------------------------

import Data.List 
import Data.Matrix 
import Data.Numbers.Primes
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir las siguientes funciones
--    potenciasDe2  :: Integer -> [Integer]
--    menorPotenciaDe2 :: Integer -> Integer
-- tales que
-- + (potenciasDe2 a) es la lista de las potencias de 2 que comienzan
--   por a. Por ejemplo,
--      ghci> take 5 (potenciasDe2 3)
--      [32,32768,33554432,34359738368,35184372088832]
--      ghci> take 2 (potenciasDe2 102)
--      [1024,102844034832575377634685573909834406561420991602098741459288064]
--      ghci> take 2 (potenciasDe2 40)
--      [4096,40564819207303340847894502572032]
-- + (menorPotenciaDe2 a) es la menor potencia de 2 que comienza con
--   el número a. Por ejemplo,
--      ghci> menorPotenciaDe2 10
--      1024
--      ghci> menorPotenciaDe2 25
--      256
--      ghci> menorPotenciaDe2 62
--      6277101735386680763835789423207666416102355444464034512896
--      ghci> menorPotenciaDe2 425
--      42535295865117307932921825928971026432
--      ghci> menorPotenciaDe2 967140655691
--      9671406556917033397649408
--      ghci> [menorPotenciaDe2 a | a <- [1..10]]
--      [1,2,32,4,512,64,70368744177664,8,9007199254740992,1024]
--
-- Comprobar con QuickCheck que, para todo entero positivo a, existe una
-- potencia de 2 que empieza por a. 
-- ---------------------------------------------------------------------

-- 1ª definición de potenciasDe2
-- =============================

potenciasDe2A :: Integer -> [Integer]
potenciasDe2A a =
  [x | x <- potenciasA
     , a `esPrefijo` x]

potenciasA :: [Integer]
potenciasA = [2^n | n <- [0..]]

esPrefijo :: Integer -> Integer -> Bool
esPrefijo x y = show x `isPrefixOf` show y

-- 2ª definición de potenciasDe2
-- =============================

potenciasDe2 :: Integer -> [Integer]
potenciasDe2 a = filter (a `esPrefijo`) potenciasA

-- 3ª definición de potenciasDe2
-- =============================

potenciasDe2C :: Integer -> [Integer]
potenciasDe2C a = filter (a `esPrefijo`) potenciasC

potenciasC :: [Integer]
potenciasC = iterate (*2) 1

-- Comparación de eficiencia
-- =========================

-- ghci> length (show (head (potenciasDe2A 123456)))
-- 19054
-- (7.17 secs, 1,591,992,792 bytes)
-- ghci> length (show (head (potenciasDe2 123456)))
-- 19054
-- (5.96 secs, 1,273,295,272 bytes)
-- ghci> length (show (head (potenciasDe2C 123456)))
-- 19054
-- (6.24 secs, 1,542,698,392 bytes)

-- Definición de menorPotenciaDe2 
menorPotenciaDe2 :: Integer -> Integer
menorPotenciaDe2 = head . potenciasDe2

-- Propiedad
prop_potenciasDe2 :: Integer -> Property
prop_potenciasDe2 a =
  a > 0 ==> not (null (potenciasDe2 a))

-- Comprobación
--    ghci> quickCheck prop_potenciasDe2
--    +++ OK, passed 100 tests.
  
-- ---------------------------------------------------------------------
-- Ejrcicio 2. Un número natural n es una potencia perfecta si existen
-- dos números naturales m > 1 y k > 1 tales que n = m^k. Las primeras
-- potencias perfectas son
--    4 = 2², 8 = 2³, 9 = 3², 16 = 2⁴, 25 = 5², 27 = 3³, 32 = 2⁵, 
--    36 = 6², 49 = 7², 64 = 2⁶, ...
-- 
-- Definir la sucesión
--    potenciasPerfectas :: [Integer]
-- cuyos términos son las potencias perfectas. Por ejemplo,
--    take 10 potenciasPerfectas     ==  [4,8,9,16,25,27,32,36,49,64]
--    144 `elem` potenciasPerfectas  ==  True
--    potenciasPerfectas !! 100      ==  6724
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

potenciasPerfectas1 :: [Integer]
potenciasPerfectas1 = filter esPotenciaPerfecta [4..]

-- (esPotenciaPerfecta x) se verifica si x es una potencia perfecta. Por
-- ejemplo, 
--    esPotenciaPerfecta 36  ==  True
--    esPotenciaPerfecta 72  ==  False
esPotenciaPerfecta :: Integer -> Bool
esPotenciaPerfecta = not . null. potenciasPerfectasDe 

-- (potenciasPerfectasDe x) es la lista de pares (a,b) tales que 
-- x = a^b. Por ejemplo,
--    potenciasPerfectasDe 64  ==  [(2,6),(4,3),(8,2)]
--    potenciasPerfectasDe 72  ==  []
potenciasPerfectasDe :: Integer -> [(Integer,Integer)]
potenciasPerfectasDe n = 
    [(m,k) | m <- takeWhile (\x -> x*x <= n) [2..]
           , k <- takeWhile (\x -> m^x <= n) [2..]
           , m^k == n]

-- 2ª solución
-- ===========

potenciasPerfectas2 :: [Integer]
potenciasPerfectas2 = [x | x <- [4..], esPotenciaPerfecta2 x]

-- (esPotenciaPerfecta2 x) se verifica si x es una potencia perfecta. Por
-- ejemplo, 
--    esPotenciaPerfecta2 36  ==  True
--    esPotenciaPerfecta2 72  ==  False
esPotenciaPerfecta2 :: Integer -> Bool
esPotenciaPerfecta2 x = mcd (exponentes x) > 1

-- (exponentes x) es la lista de los exponentes de l factorización prima
-- de x. Por ejemplos,
--    exponentes 36  ==  [2,2]
--    exponentes 72  ==  [3,2]
exponentes :: Integer -> [Int]
exponentes x = [length ys | ys <- group (primeFactors x)] 

-- (mcd xs) es el máximo común divisor de la lista xs. Por ejemplo,
--    mcd [4,6,10]  ==  2
--    mcd [4,5,10]  ==  1
mcd :: [Int] -> Int
mcd = foldl1 gcd

-- 3ª definición
-- =============

potenciasPerfectas :: [Integer]
potenciasPerfectas = mezclaTodas potencias

-- potencias es la lista las listas de potencias de todos los números
-- mayores que 1 con exponentes mayores que 1. Por ejemplo,
--    ghci> map (take 3) (take 4 potencias)
--    [[4,8,16],[9,27,81],[16,64,256],[25,125,625]]
potencias :: [[Integer]]
potencias = [[n^k | k <- [2..]] | n <- [2..]]

-- (mezclaTodas xss) es la mezcla ordenada sin repeticiones de las
-- listas ordenadas xss. Por ejemplo,
--    take 7 (mezclaTodas potencias)  ==  [4,8,9,16,25,27,32]
mezclaTodas :: Ord a => [[a]] -> [a]
mezclaTodas = foldr1 xmezcla
    where xmezcla (x:xs) ys = x : mezcla xs ys

-- (mezcla xs ys) es la mezcla ordenada sin repeticiones de las
-- listas ordenadas xs e ys. Por ejemplo,
--    take 7 (mezcla [2,5..] [4,6..])  ==  [2,4,5,6,8,10,11]
mezcla :: Ord a => [a] -> [a] -> [a]
mezcla (x:xs) (y:ys) | x < y  = x : mezcla xs (y:ys)
                     | x == y = x : mezcla xs ys
                     | x > y  = y : mezcla (x:xs) ys

-- Comparación de eficiencia
-- =========================

--    ghci> potenciasPerfectas1 !! 100
--    6724
--    (3.39 secs, 692758212 bytes)
--    ghci> potenciasPerfectas2 !! 100
--    6724
--    (0.29 secs, 105,459,200 bytes)
--    ghci> potenciasPerfectas3 !! 100
--    6724
--    (0.01 secs, 1582436 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Los árboles binarios se pueden representar con el de
-- tipo de dato algebraico 
--    data Arbol a = H
--                 | N a (Arbol a) (Arbol a)
--      deriving Show
-- Por ejemplo, los árboles
--        3                7     
--       / \              / \    
--      2   4            5   8   
--     / \   \          / \   \  
--    1   3   5        6   4   10
-- se representan por
--    ej1, ej2 :: Arbol Int
--    ej1 = N 3 (N 2 (N 1 H H) (N 3 H H)) (N 4 H (N 5 H H))
--    ej2 = N 7 (N 5 (N 6 H H) (N 4 H H)) (N 8 H (N 10 H H))
--
-- Un árbol binario es continuo si el valor absoluto de la
-- diferencia de los elementos adyacentes es 1. Por ejemplo, el árbol ej1
-- es continuo ya que el valor absoluto de sus pares de elementos
-- adyacentes son
--    |3-2| = |2-1| = |2-3| = |3-4| = |4-5| = 1
-- En cambio, el ej2 no lo es ya que |8-10| =/= 1.
--
-- Definir la función
--    esContinuo :: (Num a, Eq a) => Arbol a -> Bool
-- tal que (esContinuo x) se verifica si el árbol x es continuo. Por
-- ejemplo, 
--    esContinuo ej1  ==  True
--    esContinuo ej2  ==  False
-- ---------------------------------------------------------------------

data Arbol a = H
             | N a (Arbol a) (Arbol a)
  deriving Show

ej1, ej2 :: Arbol Int
ej1 = N 3 (N 2 (N 1 H H) (N 3 H H)) (N 4 H (N 5 H H))
ej2 = N 7 (N 5 (N 6 H H) (N 4 H H)) (N 8 H (N 10 H H))

-- 1ª solución
-- ===========

esContinuo :: (Num a, Eq a) => Arbol a -> Bool
esContinuo H         = True
esContinuo (N _ H H) = True
esContinuo (N x i@(N y _ _) H) =
  abs (x - y) == 1 && esContinuo i
esContinuo (N x H d@(N y _ _)) =
  abs (x - y) == 1 && esContinuo d
esContinuo (N x i@(N y _ _) d@(N z _ _)) =
  abs (x - y) == 1 && esContinuo i && abs (x - z) == 1 && esContinuo d 

-- 2ª solución
-- ===========

esContinuo2 :: (Num a, Eq a) => Arbol a -> Bool
esContinuo2 x =
  all esContinua (ramas x)

-- (ramas x) es la lista de las ramas del árbol x. Por ejemplo,
--    ramas ej1  ==  [[3,2,1],[3,2,3],[3,4,5]]
--    ramas ej2  ==  [[7,5,6],[7,5,4],[7,8,10]]
ramas :: Arbol a -> [[a]]
ramas H         = []
ramas (N x H H) = [[x]]
ramas (N x i d) = [x : xs | xs <- ramas i ++ ramas d]

-- (esContinua xs) se verifica si el valor absoluto de la diferencia de
-- los elementos adyacentes de xs es 1. Por ejemplo, 
--    esContinua [3,2,3]   ==  True
--    esContinua [7,8,10]  ==  False
esContinua :: (Num a, Eq a) => [a] -> Bool
esContinua xs =
  and [abs (x - y) == 1 | (x, y) <- zip xs (tail xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 4. El problema de las N torres consiste en colocar N torres
-- en un tablero con N filas y N columnas de forma que no haya dos
-- torres en la misma fila ni en la misma columna.  
--
-- Cada solución del problema de puede representar mediante una matriz
-- con ceros y unos donde los unos representan las posiciones ocupadas
-- por las torres y los ceros las posiciones libres. Por ejemplo, 
--    ( 0 1 0 )
--    ( 1 0 0 )
--    ( 0 0 1 )
-- representa una solución del problema de las 3 torres.
-- 
-- Definir las funciones
--    torres  :: Int -> [Matrix Int]
--    nTorres :: Int -> Integer
-- tales que
-- + (torres n) es la lista de las soluciones del problema de las n
--   torres. Por ejemplo,  
--       ghci> torres 3
--       [( 1 0 0 )
--        ( 0 1 0 )
--        ( 0 0 1 )
--       ,( 1 0 0 )
--        ( 0 0 1 )
--        ( 0 1 0 )
--       ,( 0 1 0 )
--        ( 1 0 0 )
--        ( 0 0 1 )
--       ,( 0 1 0 )
--        ( 0 0 1 )
--        ( 1 0 0 )
--       ,( 0 0 1 )
--        ( 1 0 0 )
--        ( 0 1 0 )
--       ,( 0 0 1 )
--        ( 0 1 0 )
--        ( 1 0 0 )
--       ]
--   donde se ha indicado con 1 las posiciones ocupadas por las torres. 
-- + (nTorres n) es el número de soluciones del problema de las n
--   torres. Por ejemplo,   
--       ghci> nTorres 3
--       6
--       ghci> length (show (nTorres (10^4)))
--       35660
-- ---------------------------------------------------------------------

-- 1ª definición de torres
-- =======================

torres1 :: Int -> [Matrix Int]
torres1 n = 
    [permutacionAmatriz n p | p <- sort (permutations [1..n])]

permutacionAmatriz :: Int -> [Int] -> Matrix Int
permutacionAmatriz n p =
    matrix n n f
    where f (i,j) | (i,j) `elem` posiciones = 1
                  | otherwise               = 0
          posiciones = zip [1..n] p    

-- 2ª definición de torres
-- =======================

torres :: Int -> [Matrix Int]
torres = map fromLists . permutations . toLists . identity

-- El cálculo con la definición anterior es:
--    ghci> identity 3
--    ( 1 0 0 )
--    ( 0 1 0 )
--    ( 0 0 1 )
--    
--    ghci> toLists it
--    [[1,0,0],[0,1,0],[0,0,1]]
--    ghci> permutations it
--    [[[1,0,0],[0,1,0],[0,0,1]],
--     [[0,1,0],[1,0,0],[0,0,1]],
--     [[0,0,1],[0,1,0],[1,0,0]],
--     [[0,1,0],[0,0,1],[1,0,0]],
--     [[0,0,1],[1,0,0],[0,1,0]],
--     [[1,0,0],[0,0,1],[0,1,0]]]
--    ghci> map fromLists it
--    [( 1 0 0 )
--     ( 0 1 0 )
--     ( 0 0 1 )
--    ,( 0 1 0 )
--     ( 1 0 0 )
--     ( 0 0 1 )
--    ,( 0 0 1 )
--     ( 0 1 0 )
--     ( 1 0 0 )
--    ,( 0 1 0 )
--     ( 0 0 1 )
--     ( 1 0 0 )
--    ,( 0 0 1 )
--     ( 1 0 0 )
--     ( 0 1 0 )
--    ,( 1 0 0 )
--     ( 0 0 1 )
--     ( 0 1 0 )
--    ]

-- 1ª definición de nTorres
-- ========================

nTorres1 :: Int -> Integer
nTorres1 = genericLength . torres1

-- 2ª definición de nTorres
-- ========================

nTorres :: Int -> Integer
nTorres n = product [1..fromIntegral n]

-- Comparación de eficiencia
-- =========================

--    ghci> nTorres1 9
--    362880
--    (4.22 secs, 693,596,128 bytes)
--    ghci> nTorres2 9
--    362880
--    (0.00 secs, 0 bytes)
