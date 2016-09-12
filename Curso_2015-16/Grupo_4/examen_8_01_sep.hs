-- Informática (1º del Grado en Matemáticas)
-- Examen de la 2ª convocatoria (1 de septiembre de 2016)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List 
import Data.Numbers.Primes
import Data.Matrix 
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Un primo cubano es un número primo que se puede escribir
-- como diferencia de dos cubos consecutivos. Por ejemplo, el 61 es un
-- primo cubano porque es primo y 61 = 5^3 - 4^3. 
-- 
-- Definir la sucesión
--    cubanos :: [Integer]
-- tal que sus elementos son los números cubanos. Por ejemplo,
--    ghci> take 15 cubanos
--    [7,19,37,61,127,271,331,397,547,631,919,1657,1801,1951,2269]
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

cubanos1 :: [Integer]
cubanos1 = filter isPrime (zipWith (-) (tail cubos) cubos) 
  where cubos = map (^3) [1..]

-- 2ª solución
-- ===========

cubanos2 :: [Integer]
cubanos2 = filter isPrime [(x+1)^3 - x^3 | x <- [1..]]

-- 3ª solución
-- ===========

cubanos3 :: [Integer]
cubanos3 = filter isPrime [3*x^2 + 3*x + 1 | x <- [1..]]

-- ----------------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    segmentos :: (Enum a, Eq a) => [a] -> [[a]]
-- tal que (segmentos xss) es la lista de los segmentos maximales (es
-- decir, de máxima longitud) de xss formados por elementos
-- consecutivos. Por ejemplo, 
--    segmentos [1,2,5,6,4]     ==  [[1,2],[5,6],[4]]
--    segmentos [1,2,3,4,7,8,9] ==  [[1,2,3,4],[7,8,9]]
--    segmentos "abbccddeeebc"  ==  ["ab","bc","cd","de","e","e","bc"]
-- 
-- Nota: Se puede usar la función succ tal que (succ x) es el sucesor de
-- x. Por ejemplo,
--    succ 3    ==  4
--    succ 'c'  ==  'd'
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión):
segmentos1 :: (Enum a, Eq a) => [a] -> [[a]]
segmentos1 []  = []
segmentos1 [x] = [[x]]
segmentos1 (x:xs) | y == succ x = (x:y:ys):zs
                  | otherwise   = [x] : (y:ys):zs
    where ((y:ys):zs) = segmentos1 xs
 
-- 2ª definición.
segmentos2 :: (Enum a, Eq a) => [a] -> [[a]]
segmentos2 []  = []
segmentos2 xs = ys : segmentos2 zs
  where ys = inicial xs
        n  = length ys
        zs = drop n xs
 
-- (inicial xs) es el segmento inicial de xs formado por elementos
-- consecutivos. Por ejemplo,
--    inicial [1,2,5,6,4]  ==  [1,2]
--    inicial "abccddeeebc"  ==  "abc"
inicial :: (Enum a, Eq a) => [a] -> [a]
inicial [] = []
inicial (x:xs) = 
  [y | (y,z) <- takeWhile (\(u,v) -> u == v) (zip (x:xs) [x..])]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Los árboles se pueden representar mediante el siguiente
-- tipo de datos 
--    data Arbol a = N a [Arbol a]
--                   deriving Show
-- Por ejemplo, los árboles
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
--    ramasLargas :: Arbol b -> [[b]]
-- tal que (ramasLargas a) es la lista de las ramas más largas del árbol
-- a. Por ejemplo, 
--    ramas ej1  ==  [[1,3,4]]
--    ramas ej2  ==  [[3,5,6],[3,7,2],[3,7,1]]
-- ---------------------------------------------------------------------

data Arbol a = N a [Arbol a]
  deriving Show

ej1, ej2 :: Arbol Int
ej1 = N 1 [N 2 [],N 3 [N 4 []]]
ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]

ramasLargas :: Arbol b -> [[b]]
ramasLargas a = [xs | xs <- todasRamas,
                      length xs == m]
  where todasRamas = ramas a
        m = maximum (map length todasRamas)

-- (ramas a) es la lista de todas las ramas del árbol a. Por ejemplo, 
--    ramas ej1  ==  [[1,2],[1,3,4]]
--    ramas ej2  ==  [[3,5,6],[3,4],[3,7,2],[3,7,1]]
ramas :: Arbol b -> [[b]]
ramas (N x []) = [[x]]
ramas (N x as) = [x : xs | a <- as, xs <- ramas a]

-- 2ª solución:
ramas2 :: Arbol b -> [[b]]
ramas2 (N x []) = [[x]]
ramas2 (N x as) = concatMap (map (x:)) (map ramas2 as)

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. El problema de las N torres consiste en colocar N
-- torres en un  tablero con N filas y N columnas de forma que no haya
-- dos torres en la misma fila ni en la misma columna.  
--
-- Cada solución del problema de puede representar mediante una matriz
-- con ceros y unos donde los unos representan las posiciones ocupadas
-- por las torres y los ceros las posiciones libres. Por ejemplo, 
--    ( 0 1 0 )
--    ( 1 0 0 )
--    ( 0 0 1 )
-- representa una solución del problema de las 3 torres.
-- 
-- Definir la función
--    torres  :: Int -> [Matrix Int]
-- tal que (torres n) es la lista de las soluciones del problema de las
-- n torres. Por ejemplo,  
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
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

torres1 :: Int -> [Matrix Int]
torres1 n = 
  [permutacionAmatriz n p | p <- sort (permutations [1..n])]

permutacionAmatriz :: Int -> [Int] -> Matrix Int
permutacionAmatriz n p =
  matrix n n f
  where f (i,j) | (i,j) `elem` posiciones = 1
                | otherwise               = 0
        posiciones = zip [1..n] p    

-- 2ª definición
-- =============

torres2 :: Int -> [Matrix Int]
torres2 = map fromLists . permutations . toLists . identity

-- El cálculo con la definición anterior es:
--    ghci> identity 3
--    ( 1 0 0 )
--    ( 0 1 0 )
--    ( 0 0 1 )
--    
--    ghci> toLists it
--    [[1,0,0],[0,1,0],[0,0,1]]
--
--    ghci> permutations it
--    [[[1,0,0],[0,1,0],[0,0,1]],
--     [[0,1,0],[1,0,0],[0,0,1]],
--     [[0,0,1],[0,1,0],[1,0,0]],
--     [[0,1,0],[0,0,1],[1,0,0]],
--     [[0,0,1],[1,0,0],[0,1,0]],
--     [[1,0,0],[0,0,1],[0,1,0]]]
-- 
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

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función
--    nTorres :: Int -> Integer
-- tal que (nTorres n) es el número de soluciones del problema de las n
-- torres. Por ejemplo,   
--       ghci> nTorres 3
--       6
--       ghci> length (show (nTorres (10^4)))
--       35660
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

nTorres1 :: Int -> Integer
nTorres1 = genericLength . torres1

-- 2ª definición de nTorres
-- ========================

nTorres2 :: Int -> Integer
nTorres2 n = product [1..fromIntegral n]

-- Comparación de eficiencia
-- =========================

--    ghci> nTorres1 9
--    362880
--    (4.22 secs, 693,596,128 bytes)
--    ghci> nTorres2 9
--    362880
--    (0.00 secs, 0 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. El conjunto de todas las palabras se puede ordenar
-- como se muestra a continuación:
--      a,   b, ...,   z,   A,   B, ...,   Z,
--     aa,  ab, ...,  az,  aA,  aB, ...,  aZ,
--     ba,  bb, ...,  bz,  bA,  bB, ...,  bZ,
--    ...
--     za,  zb, ...,  zz,  zA,  zB, ...,  zZ,
--    aaa, aab, ..., aaz, aaA, aaB, ..., aaZ,
--    baa, bab, ..., baz, baA, baB, ..., baZ,
--    ...
--
-- Definir la función
--    palabras :: [String]
-- tal que palabras es la lista ordenada de todas las palabras. Por
-- ejemplo, 
--      ghci> take 10 (drop 100 palabras)
--      ["aW","aX","aY","aZ","ba","bb","bc","bd","be","bf"]
--      ghci> take 10 (drop 2750 palabras)
--      ["ZU","ZV","ZW","ZX","ZY","ZZ","aaa","aab","aac","aad"]
--      ghci> palabras !! (10^6)
--      "gePO"
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============
    
palabras1 :: [String]
palabras1 = concatMap palabrasDeLongitud [1..]

-- (palabrasDeLongitud n) es la lista ordenada de palabras longitud
-- n. Por ejemplo,
--    take 3 (palabrasDeLongitud 2)  ==  ["aa","ab","ac"]
--    take 3 (palabrasDeLongitud 3)  ==  ["aaa","aab","aac"]
palabrasDeLongitud :: Integer -> [String]
palabrasDeLongitud 1 = [[x]  | x <- letras]
palabrasDeLongitud n = [x:ys | x <- letras,
                               ys <- palabrasDeLongitud (n-1)]

-- letras es la lista ordenada de las letras.                       
letras :: [Char]
letras = ['a'..'z'] ++ ['A'..'Z']

-- 2ª definición
-- =============

palabras2 :: [String]
palabras2 = concat (iterate f elementales)
  where f = concatMap (\x -> map (x++) elementales)

-- elementales es la lista de las palabras de una letra.              
elementales :: [String]            
elementales = map (:[]) letras

-- 3ª definición
-- =============

palabras3 :: [String]
palabras3 = tail $ map reverse aux
  where aux = "" : [c:cs | cs <- aux, c <- letras]

-- Comparación
-- ===========

--    ghci> palabras1 !! (10^6)
--    "gePO"
--    (0.87 secs, 480,927,648 bytes)
--    ghci> palabras2 !! (10^6)
--    "gePO"
--    (0.11 secs, 146,879,840 bytes)
--    ghci> palabras3 !! (10^6)
--    "gePO"
--    (0.25 secs, 203,584,608 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir la función
--    posicion :: String -> Integer
-- tal que (posicion n) es la palabra que ocupa la posición n en la lista
-- ordenada de todas las palabras. Por ejemplo,
--    posicion "c"                    ==  2
--    posicion "ab"                   ==  53
--    posicion "ba"                   ==  104
--    posicion "eva"                  ==  14664
--    posicion "adan"                 ==  151489
--    posicion "HoyEsMartes"          ==  4957940944437977046
--    posicion "EnUnLugarDeLaMancha"  ==  241779893912461058861484239910864
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

posicion1 :: String -> Integer
posicion1 cs = genericLength (takeWhile (/=cs) palabras1)

-- 2ª definición
-- =============

posicion2 :: String -> Integer
posicion2 ""     = -1
posicion2 (c:cs) = (1 + posicionLetra c) * 52^(length cs) + posicion2 cs

posicionLetra :: Char -> Integer
posicionLetra c = genericLength (takeWhile (/=c) letras)

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Comprobar con QuickCheck que para todo entero positivo
-- n se verifica que
--    posicion (palabras `genericIndex` n) == n
-- ---------------------------------------------------------------------

-- La propiedad es
prop_palabras :: (Positive Integer) -> Bool
prop_palabras (Positive n) =
  posicion2 (palabras3 `genericIndex` n) == n

-- La comprobación es              
--    ghci> quickCheck prop_palabras
--    +++ OK, passed 100 tests.



