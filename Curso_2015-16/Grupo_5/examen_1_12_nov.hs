-- Informática (1º del Grado en Matemáticas)
-- 1º examen de evaluación continua (3 de noviembre de 2015)
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por comprensión, la función
--    selecC :: Int -> Int -> [Int] -> [Int]
-- tal que (selecC a b xs) es la lista de los elementos de que son
-- múltiplos de a pero no de b. Por ejemplo,
--   selecC 3 2 [8,9,2,3,4,5,6,10] == [9,3]
--   selecC 2 3 [8,9,2,3,4,5,6,10] == [8,2,4,10]
-- ---------------------------------------------------------------------

selecC :: Int -> Int -> [Int] -> [Int]
selecC a b xs = [x | x <- xs, rem x a == 0, rem x b /= 0]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, por recursión, la función
--    selecR :: Int -> Int -> [Int] -> [Int]
-- tal que (selecR a b xs) es la lista de los elementos de que son
-- múltiplos de a pero no de b. Por ejemplo,
--   selecR 3 2 [8,9,2,3,4,5,6,10] == [9,3]
--   selecR 2 3 [8,9,2,3,4,5,6,10] == [8,2,4,10]
-- ---------------------------------------------------------------------

selecR :: Int -> Int -> [Int] -> [Int]
selecR _ _ [] = []
selecR a b (x:xs) 
    | rem x a == 0 && rem x b /= 0 = x : selecR a b xs
    | otherwise                    = selecR a b xs

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    mismaLongitud :: [[a]] -> Bool
-- tal que (mismaLongitud xss) se verifica si todas las listas de la
-- lista de listas xss tienen la misma longitud. Por ejemplo,
--    mismaLongitud [[1,2],[6,4],[0,0],[7,4]] == True
--    mismaLongitud [[1,2],[6,4,5],[0,0]]     == False
-- ---------------------------------------------------------------------

-- 1ª solución:
mismaLongitud1 :: [[a]] -> Bool
mismaLongitud1 []       = True
mismaLongitud1 (xs:xss) = and [length ys == n | ys <- xss]
    where n = length xs

-- 2ª solución:
mismaLongitud2 :: [[a]] -> Bool
mismaLongitud2 xss = 
    and [length xs == length ys | (xs,ys) <- zip xss (tail xss)]

-- 3ª solución:
mismaLongitud3 :: [[a]] -> Bool
mismaLongitud3 (xs:ys:xss) = 
    length xs == length ys && mismaLongitud3 (ys:xss)
mismaLongitud3 _ = True           

-- 4ª solución:
mismaLongitud4 :: [[a]] -> Bool
mismaLongitud4 [] = True
mismaLongitud4 (xs:xss) =
    all (\ys -> length ys == n) xss
    where n = length xs 

-- Comparación de eficiencia
--    ghci> mismaLongitud1 (replicate 20000 (replicate 20000 5))
--    True
--    (5.05 secs, 0 bytes)
--    ghci> mismaLongitud2 (replicate 20000 (replicate 20000 5))
--    True
--    (9.98 secs, 0 bytes)
--    ghci> mismaLongitud3 (replicate 20000 (replicate 20000 5))
--    True
--    (10.17 secs, 0 bytes)
--    ghci> mismaLongitud4 (replicate 20000 (replicate 20000 5))
--    True
--    (5.18 secs, 0 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Los resultados de las votaciones a delegado en un grupo
-- de clase se recogen mediante listas de asociación. Por ejemplo, 
--    votos :: [(String,Int)]
--    votos = [("Ana Perez",10),("Juan Lopez",7), ("Julia Rus", 27),
--             ("Pedro Val",1), ("Pedro Ruiz",27),("Berta Gomez",11)]
-- 
-- Definir la función 
--    ganadores :: [(String,Int)] -> [String]
-- tal que (ganadores xs) es la lista de los estudiantes con mayor
-- número de votos en xs. Por ejemplo,
--     ganadores votos == ["Julia Rus","Pedro Ruiz"]
-- ---------------------------------------------------------------------

votos :: [(String,Int)]
votos = [("Ana Perez",10),("Juan Lopez",7), ("Julia Rus", 27),
         ("Pedro Val",1), ("Pedro Ruiz",27),("Berta Gomez",11)]

ganadores :: [(String,Int)] -> [String]
ganadores xs = [c | (c,x) <- xs, x == maxVotos]
    where maxVotos = maximum [j | (i,j) <- xs] 

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Un entero positivo x se dirá muy par si tanto x como
-- x^2 sólo contienen cifras pares. Por ejemplo, 200 es muy par porque
-- todas las cifras de 200 y 200² = 40000 son pares; pero 26 no lo es
-- porque 26² = 767 tiene cifras impares. 
-- 
-- Definir  la función
--    muyPar :: Integer -> Bool
-- tal que (muyPar x) se verifica si x es muy par. por ejemplo,
--    muyPar 200           == True
--    muyPar 26            == False
--    muyPar 828628040080  == True
-- ---------------------------------------------------------------------

muyPar :: Integer -> Bool
muyPar n = all even (digitos n) && all even (digitos (n*n))

digitos :: Integer -> [Int]
digitos n = [read [d] | d <- show n]

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función
--    siguienteMuyPar :: Integer -> Integer
-- tal que (siguienteMuyPar x) es el primer número mayor que x que es
-- muy par. Por ejemplo, 
--    siguienteMuyPar 300           ==  668
--    siguienteMuyPar 668           ==  680
--    siguienteMuyPar 828268400000  ==  828268460602
-- ---------------------------------------------------------------------

siguienteMuyPar :: Integer -> Integer
siguienteMuyPar x = 
    head [n | n <- [y,y+2..], muyPar n]
    where y = siguientePar x

-- (siguientePar x) es el primer número mayor que x que es par. Por
-- ejemplo, 
--    siguientePar 3  ==  4
--    siguientePar 4  ==  6
siguientePar :: Integer -> Integer
siguientePar x | odd x     = x+1
               | otherwise = x+2

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir la función
--    transformada :: [a] -> [a]
-- tal que (transformada xs) es la lista obtenida repitiendo cada
-- elemento tantas veces como indica su posición en la lista. Por
-- ejemplo, 
--    transformada [7,2,5] == [7,2,2,5,5,5]
--    transformada "eco"   == "eccooo"
-- ---------------------------------------------------------------------

transformada :: [a] -> [a]
transformada xs = concat [replicate n x | (n,x) <- zip [1..] xs]

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Comprobar con QuickCheck si la transformada de una
-- lista de n números enteros, con n >= 2, tiene menos de n^3 elementos.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_transformada :: [Int] -> Property
prop_transformada xs = n >= 2 ==> length (transformada xs) < n^3
    where n = length xs

-- La comprobación es
--    ghci> quickCheck prop_transformada
--    +++ OK, passed 100 tests.
