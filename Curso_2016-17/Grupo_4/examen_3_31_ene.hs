-- Informática (1º del Grado en Matemáticas, Grupos 2 y 4)
-- 3º examen de evaluación continua (31 de enero de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List 
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. (2.5 puntos) Se observa que en la cadena "aabbccddeffgg"
-- todos los caracteres están duplicados excepto el 'e'. Al añadirlo
-- obtenemos la cadena "aabbccddeeffgg" y se dice que esta última está
-- duplicada.  
--
-- También se observa que la cadena "aaaabbbccccdd" no está duplicada
-- (porque hay un número impar de 'b' consecutivas). Añadiendo una 'b'
-- se obtiene la cadena "aaaabbbbccccdd" que sí está duplicada.
-- 
-- Definir la función
--    duplica :: Eq a => [a] -> [a]
-- tal que (duplica xs) es la lista obtenida duplicando los elementos de
-- xs que no lo están. Por ejemplo,
--    duplica "b"        ==  "bb"
--    duplica "abba"     ==  "aabbaa"
--    duplica "Betis"    ==  "BBeettiiss"
--    duplica [1,1,1]    ==  [1,1,1,1]
--    duplica [1,1,1,1]  ==  [1,1,1,1]
-- ---------------------------------------------------------------------

-- 1ª definición
duplica :: Eq a => [a] -> [a]
duplica []       = []
duplica [x]      = [x,x]
duplica (x:y:zs) | x == y    = x : x : duplica zs
                 | otherwise = x : x : duplica (y:zs)

-- 2ª definición
duplica2 :: Eq a => [a] -> [a]
duplica2 xs = concatMap dupl (group xs)
  where dupl ys@(y:_) | (even.length) ys = ys
                      | otherwise        = y : ys

-- ---------------------------------------------------------------------
-- Ejercicio 2. (2.5 puntos) Las matrices se pueden representar mediante
-- listas de listas. Por ejemplo, la matriz
--    |1 2 5| 
--    |3 0 7| 
--    |9 1 6| 
--    |6 4 2|
-- se puede representar por
--    ej :: [[Int]]
--    ej = [[1,2,5],
--          [3,0,7],
--          [9,1,6],
--          [6,4,2]]
--
-- Definir la función 
--    ordenaPor :: Ord a => [[a]] -> Int -> [[a]]
-- tal que (ordenaPor xss k) es la matriz obtenida ordenando la matriz
-- xss por los elementos de la columna k. Por ejemplo,
--    ordenaPor ej 0  ==  [[1,2,5],[3,0,7],[6,4,2],[9,1,6]]
--    ordenaPor ej 1  ==  [[3,0,7],[9,1,6],[1,2,5],[6,4,2]]
--    ordenaPor ej 2  ==  [[6,4,2],[1,2,5],[9,1,6],[3,0,7]]
-- ---------------------------------------------------------------------

ej :: [[Int]]
ej = [[1,2,5],
      [3,0,7],
      [9,1,6],
      [6,4,2]]

-- 1ª definición
ordenaPor :: Ord a => [[a]] -> Int -> [[a]]
ordenaPor xss k =
  map snd (sort [(xs!!k,xs) | xs <- xss])

-- 2ª definición
ordenaPor2 :: Ord a => [[a]] -> Int -> [[a]]
ordenaPor2 xss k =
  map snd (sort (map (\xs -> (xs!!k, xs)) xss))

-- 4ª definición
ordenaPor4 :: Ord a => [[a]] -> Int -> [[a]]
ordenaPor4 xss k =
  map snd (sort (zip (map (!! k) xss) xss))

-- 5ª definición
ordenaPor5 :: Ord a => [[a]] -> Int -> [[a]]
ordenaPor5 xss k = sortBy comp xss
  where comp ys zs = compare (ys!!k) (zs!!k)

-- ---------------------------------------------------------------------
-- Ejercicio 3. (2.5 puntos) Definir la función
--    sumas3Capicuas  :: Integer -> [(Integer, Integer, Integer)]
-- tal que (sumas3Capicuas n) es la lista de las descomposiciones del
-- número natural n como suma de tres números capicúas (con los sumandos
-- no decrecientes). Por ejemplo,
--    sumas3Capicuas 0             ==  [(0,0,0)]
--    sumas3Capicuas 1             ==  [(0,0,1)]
--    sumas3Capicuas 2             ==  [(0,0,2),(0,1,1)]
--    sumas3Capicuas 3             ==  [(0,0,3),(0,1,2),(1,1,1)]
--    sumas3Capicuas 4             ==  [(0,0,4),(0,1,3),(0,2,2),(1,1,2)]
--    length (sumas3Capicuas 17)   ==  17
--    length (sumas3Capicuas 2017) ==  47
-- 
-- Comprobar con QuickCheck que todo número natural se puede escribir
-- como suma de tres capicúas.
-- ---------------------------------------------------------------------

sumas3Capicuas :: Integer -> [(Integer, Integer, Integer)]
sumas3Capicuas x =
  [(a,b,c) | a <- as
           , b <- dropWhile (< a) as
           , let c = x - a - b
           , b <= c 
           , esCapicua c]
  where as = takeWhile (<= x) capicuas

-- capicuas es la sucesión de los números capicúas. Por ejemplo,
--    ghci> take 45 capicuas
--    [0,1,2,3,4,5,6,7,8,9,11,22,33,44,55,66,77,88,99,101,111,121,131,
--     141,151,161,171,181,191,202,212,222,232,242,252,262,272,282,292,
--     303,313,323,333,343,353]
capicuas :: [Integer]
capicuas = capicuas1

-- 1ª definición de capicuas
-- =========================

capicuas1 :: [Integer]
capicuas1 = [n | n <- [0..]
               , esCapicua n]

-- (esCapicua x) se verifica si x es capicúa. Por ejemplo,
--    esCapicua 353   ==  True
--    esCapicua 3553  ==  True
--    esCapicua 3535  ==  False
esCapicua :: Integer -> Bool
esCapicua x =
  xs == reverse xs
  where xs = show x


-- 2ª definición de capicuas
-- =========================

capicuas2 :: [Integer]
capicuas2 = capicuasImpares `mezcla` capicuasPares

-- capicuasPares es la sucesión del cero y las capicúas con un número
-- par de dígitos. Por ejemplo,  
--    ghci> take 17 capicuasPares
--    [0,11,22,33,44,55,66,77,88,99,1001,1111,1221,1331,1441,1551,1661]
capicuasPares :: [Integer]
capicuasPares =
  [read (ns ++ reverse ns) | n <- [0..]
                           , let ns = show n]   

-- capicuasImpares es la sucesión de las capicúas con un número
-- impar de dígitos a partir de 1. Por ejemplo,  
--    ghci> take 20 capicuasImpares
--    [1,2,3,4,5,6,7,8,9,101,111,121,131,141,151,161,171,181,191,202]
capicuasImpares :: [Integer]
capicuasImpares =
  [1..9] ++ [read (ns ++ [z] ++ reverse ns)
            | n <- [1..]
            , let ns = show n
            , z <- "0123456789"]   

-- (mezcla xs ys) es la lista ordenada obtenida mezclando las dos listas
-- ordenadas xs e ys, suponiendo que ambas son infinitas y con elementos
-- distintos. Por ejemplo,
--    take 10 (mezcla [2,12..] [5,15..])  ==  [2,5,12,15,22,25,32,35,42,45]
--    take 10 (mezcla [2,22..] [5,15..])  ==  [2,5,15,22,25,35,42,45,55,62]
mezcla :: Ord a => [a] -> [a] -> [a]
mezcla us@(x:xs) vs@(y:ys)
  | x < y     = x : mezcla xs vs
  | otherwise = y : mezcla us ys

-- 3ª definición de capicuas
-- =========================

capicuas3 :: [Integer]
capicuas3 = iterate sigCapicua 0

-- (sigCapicua x) es el capicúa siguiente del número x. Por ejemplo,
--    sigCapicua 12321           == 12421
--    sigCapicua 1298921         == 1299921
--    sigCapicua 999             == 1001
--    sigCapicua 9999            == 10001
--    sigCapicua 898             == 909
--    sigCapicua 123456777654321 == 123456787654321
sigCapicua :: Integer -> Integer
sigCapicua n = read cs
    where l  = length (show (n+1))
          k  = l `div` 2
          xs = show ((n `div` (10^k)) + 1) 
          cs = xs ++ drop (l `rem` 2) (reverse xs)

-- 4ª definición de capicuas
-- =========================

capicuas4 :: [Integer]
capicuas4 =
  concatMap generaCapicuas4 [1..]

generaCapicuas4 :: Integer -> [Integer]
generaCapicuas4 1 = [0..9]
generaCapicuas4 n
  | even n    = [read (xs ++ reverse xs)
                | xs <- map show [10^(m-1)..10^m-1]]
  | otherwise = [read (xs ++ (y : reverse xs))
                | xs <- map show [10^(m-1)..10^m-1]
                , y <- "0123456789"]
  where m = n `div` 2

-- 5ª definición de capicuas
-- =========================

capicuas5 :: [Integer]
capicuas5 = 0 : aux 1
  where aux n =    [read (show x ++ tail (reverse (show x)))
                   | x <- [10^(n-1)..10^n-1]]
                ++ [read (show x ++ reverse (show x))
                   | x <- [10^(n-1)..10^n-1]]
                ++ aux (n+1)

-- 6ª definición de capicuas
-- =========================

capicuas6 :: [Integer]
capicuas6 = 0 : map read (capicuas6Aux [1..9])
 
capicuas6Aux :: [Integer] -> [String]
capicuas6Aux xs =  map duplica1 xs'
                ++ map duplica2 xs'
                ++ capicuas6Aux [head xs * 10 .. last xs * 10 + 9]
  where
    xs'         = map show xs
    duplica1 cs = cs ++ tail (reverse cs)
    duplica2 cs = cs ++ reverse cs

-- 7ª definición de capicuas
-- =========================

capicuas7 :: [Integer]
capicuas7 = 0 : map read (capicuas7Aux [1..9])
 
capicuas7Aux :: [Integer] -> [String]
capicuas7Aux xs =  map duplica1 xs'
                ++ map duplica2 xs'
                ++ capicuas7Aux [head xs * 10 .. last xs * 10 + 9]
  where
    xs'      = map show xs
    duplica1 = (++) <$> id <*> tail . reverse
    duplica2 = (++) <$> id <*> reverse

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_capicuas :: (Positive Int) -> Bool
prop_capicuas (Positive k) =
  all (== capicuas1 !! k) [f !! k | f <- [ capicuas2
                                         , capicuas3
                                         , capicuas4
                                         , capicuas5
                                         , capicuas6
                                         , capicuas7]]

-- La comprobación es
--    ghci> quickCheck prop_capicuas
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

--    ghci> capicuas1 !! 2000
--    1001001
--    (2.25 secs, 598,879,552 bytes)
--    ghci> capicuas2 !! 2000
--    1001001
--    (0.05 secs, 28,630,552 bytes)
--    ghci> capicuas3 !! 2000
--    1001001
--    (0.06 secs, 14,721,360 bytes)
--    ghci> capicuas4 !! 2000
--    1001001
--    (0.01 secs, 0 bytes)
--    ghci> capicuas5 !! 2000
--    1001001
--    (0.01 secs, 0 bytes)
--    ghci> capicuas6 !! 2000
--    1001001
--    (0.01 secs, 0 bytes)
--    ghci> capicuas7 !! 2000
--    1001001
--    (0.01 secs, 0 bytes)
--    
--    ghci> capicuas2 !! (10^5)
--    900010009
--    (2.03 secs, 1,190,503,952 bytes)
--    ghci> capicuas3 !! (10^5)
--    900010009
--    (5.12 secs, 1,408,876,328 bytes)
--    ghci> capicuas4 !! (10^5)
--    900010009
--    (0.21 secs, 8,249,296 bytes)
--    ghci> capicuas5 !! (10^5)
--    900010009
--    (0.10 secs, 31,134,176 bytes)
--    ghci> capicuas6 !! (10^5)
--    900010009
--    (0.14 secs, 55,211,272 bytes)
--    ghci> capicuas7 !! (10^5)
--    900010009
--    (0.03 secs, 0 bytes)

-- La propiedad es
prop_sumas3Capicuas :: Integer -> Property
prop_sumas3Capicuas x =
  x >= 0 ==> not (null (sumas3Capicuas x))

-- La comprobación es
--    ghci> quickCheck prop_sumas3Capicuas
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4. (2.5 puntos) Los árboles se pueden representar mediante
-- el siguiente tipo de datos
--    data Arbol a = N a [Arbol a]
--      deriving Show
-- Por ejemplo, los árboles
--      1         1             1          
--     / \       / \           / \   
--    8   3     8   3         8   3  
--        |        /|\       /|\  |   
--        4       4 5 6     4 5 6 7
-- se pueden representar por
--    ej1, ej2, ej3 :: Arbol Int
--    ej1 = N 1 [N 8 [],N 3 [N 4 []]]
--    ej2 = N 1 [N 8 [], N 3 [N 4 [], N 5 [], N 6 []]]
--    ej3 = N 1 [N 8 [N 4 [], N 5 [], N 6 []], N 3 [N 7 []]]
--
-- Definir la función
--    nodos :: Int -> Arbol a -> [a]
-- tal que (nodos k t) es la lista de los nodos del árbol t que tienen k
-- sucesores. Por ejemplo, 
--    nodos 0 ej1  ==  [8,4]
--    nodos 1 ej1  ==  [3]
--    nodos 2 ej1  ==  [1]
--    nodos 3 ej1  ==  []
--    nodos 3 ej2  ==  [3]
-- ---------------------------------------------------------------------

data Arbol a = N a [Arbol a]
  deriving Show
           
ej1, ej2, ej3 :: Arbol Int
ej1 = N 1 [N 8 [],N 3 [N 4 []]]
ej2 = N 1 [N 8 [], N 3 [N 4 [], N 5 [], N 6 []]]
ej3 = N 1 [N 8 [N 4 [], N 5 [], N 6 []], N 3 [N 7 []]]

-- 1ª definición
nodos :: Int -> Arbol a -> [a]
nodos k (N x ys)
  | k == length ys = x : concatMap (nodos k) ys
  | otherwise      = concatMap (nodos k) ys

-- 2ª definición
nodos2 :: Int -> Arbol a -> [a]
nodos2 k (N x ys)
  | k == length ys = x : zs
  | otherwise      = zs
  where zs = concat [nodos2 k y | y <- ys]

-- 3ª definición
nodos3 :: Int -> Arbol a -> [a]
nodos3 k (N x ys)
  = [x | k == length ys]
    ++ concat [nodos3 k y | y <- ys]

