-- Informática (1º del Grado en Matemáticas, Grupo 5)
-- 1º examen de evaluación continua (25 de octubre de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías                                                        --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. La carga de una lista es el número de elementos
-- estrictamente positivos menos el número de elementos estrictamente
-- negativos.
-- 
-- Definir la función  
--    carga :: [Int] -> Int
-- tal que (carga xs) es la carga de la lista xs. Por ejemplo,
--    carga [1,0,2,0,3]    ==  3
--    carga [1,0,-2,0,3]   ==  1
--    carga [1,0,-2,0,-3]  ==  -1
--    carga [1,0,-2,2,-3]  ==  0
-- ---------------------------------------------------------------------

-- 1ª definición
carga :: [Int] -> Int
carga xs = length [x | x <- xs, x > 0] - length [x | x <- xs, x < 0]

-- 2ª definición
carga2 :: [Int] -> Int
carga2 xs = sum [signum x | x <- xs]

-- 3ª definición
carga3 :: [Int] -> Int
carga3 [] = 0
carga3 (x:xs) = signum x + carga xs 

-- 4ª definición
carga4 :: [Int] -> Int
carga4 = sum . map signum

-- Propiedad de equivalencia
prop_carga :: [Int] -> Bool
prop_carga xs =
  carga xs == carga2 xs &&
  carga xs == carga3 xs &&
  carga xs == carga4 xs 

-- La comprobación es
--   ghci> quickCheck prop_carga
--   +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Una lista es equilibrada si el número de elementos
-- estrictamente positivos difiere en, a lo más, una unidad del número
-- de elementos estrictamente negativos.
-- 
-- Definir la función
--    equilibrada :: [Int] -> Bool
-- tal que (equilibrada xs) se verifica si xs es una lista
-- equilibrada. Por ejemplo, 
--    equilibrada [1,0,2,0,3]    ==  False
--    equilibrada [1,0,-2,0,3]   ==  True
--    equilibrada [1,0,-2,0,-3]  ==  True
--    equilibrada [1,0,-2,2,-3]  ==  True
-- ---------------------------------------------------------------------

equilibrada :: [Int] -> Bool
equilibrada xs = abs (carga xs) <= 1

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    triples :: Int -> [(Int,Int,Int)]
-- tal que (triples n) es la lista de todos los triples (x,y,z) con
-- 1 <= x, y, z <= n que están formados por coordenas distintas. Por
-- ejemplo, 
--    ghci> triples 3
--    [(1,2,3),(1,3,2),(2,1,3),(2,3,1),(3,1,2),(3,2,1)]
--    ghci> triples 4
--    [(1,2,3),(1,2,4),(1,3,2),(1,3,4),(1,4,2),(1,4,3),(2,1,3),(2,1,4),
--     (2,3,1),(2,3,4),(2,4,1),(2,4,3),(3,1,2),(3,1,4),(3,2,1),(3,2,4),
--     (3,4,1),(3,4,2),(4,1,2),(4,1,3),(4,2,1),(4,2,3),(4,3,1),(4,3,2)]
-- ------------------------------------------------------------------------

triples :: Int -> [(Int,Int,Int)]
triples n = [(x,y,z) | x <- [1..n]
                     , y <- [1..n]
                     , z <- [1..n]
                     , x /= y
                     , y /= z
                     , x /= z]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Los resultados de las votaciones a delegado en un
-- grupo de clase se recogen mediante listas de asociación. Por ejemplo,  
--    votos :: [(String,Int)]
--    votos =  [("Ana Perez",10),("Juan Lopez",7), ("Julia Rus", 27),
--              ("Pedro Val",1), ("Pedro Ruiz",27),("Berta Gomez",11)]
--
-- Definir la función
--    mayorV :: [(String,Int)] -> Int
-- tal que (mayorV xs) es el número de votos obtenido por los ganadores
-- de la votación xs. Por ejemplo, 
--    mayorV votos == 27
-- ---------------------------------------------------------------------

votos :: [(String,Int)]
votos =  [("Ana Perez",10),("Juan Lopez",7), ("Julia Rus", 27),
          ("Pedro Val",1), ("Pedro Ruiz",27),("Berta Gomez",11)]

-- 1ª definición
mayorV :: [(String,Int)] -> Int
mayorV xs = maximum [j | (_,j) <- xs]

-- 2ª definición
mayorV2 :: [(String,Int)] -> Int
mayorV2 = maximum . map snd

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función 
--    ganadores :: [(String,Int)] -> [String]
-- tal que (ganadores xs) es la lista de los estudiantes con mayor
-- número de votos en xs. Por ejemplo,
--    ganadores votos == ["Julia Rus","Pedro Ruiz"]
-- ---------------------------------------------------------------------

ganadores :: [(String,Int)] -> [String]
ganadores xs = [c | (c,x) <- xs, x == maxVotos]
  where maxVotos = mayorV xs

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Una lista es muy creciente si cada elemento es mayor
-- estricto que el triple del siguiente.
-- 
-- Definir la función
--    muyCreciente :: [Integer] -> Bool
-- tal que (muyCreciente xs) se verifica si xs es muy creciente. Por
-- ejemplo,
--    muyCreciente [1,5,23,115]  == True
--    muyCreciente [1,2,7,14]    == False
--    muyCreciente [7]           == True
--    muyCreciente []            == True
-- ---------------------------------------------------------------------

muyCreciente :: [Integer] -> Bool
muyCreciente xs = and [3*x < y | (x,y) <- zip xs (tail xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función
--    busca :: Integer -> Integer
-- tal que (busca n) devuelve el menor número natural de n o más cifras
-- no primo cuya lista de divisores es una lista muy creciente. Por
-- ejemplo, 
--    busca 2 == 25
--    busca 3 == 115
--    busca 6 == 100001
-- ---------------------------------------------------------------------

busca :: Integer -> Integer
busca n = head [i | i <- [10^(n-1)..10^n-1]
                  , muyCreciente (divisores i)
                  , not (primo i)]

divisores :: Integer -> [Integer]
divisores x = [i | i <- [1..x], rem x i == 0]

primo :: Integer -> Bool
primo x = divisores x == [1,x]
