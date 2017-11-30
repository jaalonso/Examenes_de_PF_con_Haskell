-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 2º examen de evaluación continua (29 de noviembre de 2017)
-- ---------------------------------------------------------------------

-- Nota: La puntuación de cada ejercicio es 2.5 puntos.

-- ---------------------------------------------------------------------
-- § Librerías                                                        --
-- ---------------------------------------------------------------------

import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    biparticiones :: Integer -> [(Integer,Integer)]
-- tal que (biparticiones n) es la lista de pares de números formados
-- por las primeras cifras de n y las restantes. Por ejemplo, 
--    biparticiones  2025  ==  [(202,5),(20,25),(2,25)]
--    biparticiones 10000  ==  [(1000,0),(100,0),(10,0),(1,0)]
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

biparticiones1 :: Integer -> [(Integer,Integer)]
biparticiones1 x = [(read y, read z) | (y,z) <- biparticionesL1 xs]
  where xs = show x

-- (biparticionesL1 xs) es la lista de los pares formados por los
-- prefijos no vacío de xs y su resto. Por ejemplo,
--    biparticionesL1 "2025" == [("2","025"),("20","25"),("202","5")]
biparticionesL1 :: [a] -> [([a],[a])]
biparticionesL1 xs = [splitAt k xs | k <- [1..length xs - 1]]

-- 2ª solución
-- ===========

biparticiones2 :: Integer -> [(Integer,Integer)]
biparticiones2 x = [(read y, read z) | (y,z) <- biparticionesL2 xs]
  where xs = show x

-- (biparticionesL2 xs) es la lista de los pares formados por los
-- prefijos no vacío de xs y su resto. Por ejemplo,
--    biparticionesL2 "2025" == [("2","025"),("20","25"),("202","5")]
biparticionesL2 :: [a] -> [([a],[a])]
biparticionesL2 xs =
  takeWhile (not . null . snd) [splitAt n xs | n <- [1..]]

-- 3ª solución
-- ===========

biparticiones3 :: Integer -> [(Integer,Integer)]
biparticiones3 a =
  takeWhile ((>0) . fst) [divMod a (10^n) | n <- [1..]] 

-- 4ª solución
-- ===========

biparticiones4 :: Integer -> [(Integer,Integer)]
biparticiones4 n =
  [quotRem n (10^x) | x <- [1..length (show n) -1]]

-- 5ª solución
-- ===========

biparticiones5 :: Integer -> [(Integer,Integer)]
biparticiones5 n =
  takeWhile (/= (0,n)) [divMod n (10^x) | x <- [1..]]

-- Comparación de eficiencia
-- =========================

--    λ> numero n = (read (replicate n '2')) :: Integer
--    (0.00 secs, 0 bytes)
--    λ> length (biparticiones1 (numero 10000))
--    9999
--    (0.03 secs, 10,753,192 bytes)
--    λ> length (biparticiones2 (numero 10000))
--    9999
--    (1.89 secs, 6,410,513,136 bytes)
--    λ> length (biparticiones3 (numero 10000))
--    9999
--    (0.54 secs, 152,777,680 bytes)
--    λ> length (biparticiones4 (numero 10000))
--    9999
--    (0.01 secs, 7,382,816 bytes)
--    λ> length (biparticiones5 (numero 10000))
--    9999
--    (2.11 secs, 152,131,136 bytes)
--    
--    λ> length (biparticiones1 (numero (10^7)))
--    9999999
--    (14.23 secs, 10,401,100,848 bytes)
--    λ> length (biparticiones4 (numero (10^7)))
--    9999999
--    (11.43 secs, 7,361,097,856 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    producto :: [[a]] -> [[a]]
-- tal que (producto xss) es el producto cartesiano de los conjuntos
-- xss. Por ejemplo,
--    ghci> producto [[1,3],[2,5]]
--    [[1,2],[1,5],[3,2],[3,5]]
--    ghci> producto [[1,3],[2,5],[6,4]]
--    [[1,2,6],[1,2,4],[1,5,6],[1,5,4],[3,2,6],[3,2,4],[3,5,6],[3,5,4]]
--    ghci> producto [[1,3,5],[2,4]]
--    [[1,2],[1,4],[3,2],[3,4],[5,2],[5,4]]
--    ghci> producto []
--    [[]]
-- ---------------------------------------------------------------------

-- 1ª solución
producto :: [[a]] -> [[a]]
producto []       = [[]]
producto (xs:xss) = [x:ys | x <- xs, ys <- producto xss]

-- 2ª solución
producto2 :: [[a]] -> [[a]]
producto2 = foldr f [[]]
  where f xs xss = [x:ys | x <- xs, ys <- xss]

-- 3ª solución
producto3 :: [[a]] -> [[a]]
producto3 = foldr aux [[]] 
  where aux [] _      = []
        aux (x:xs) ys = map (x:) ys ++ aux xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 3. Los árboles binarios con valores enteros se pueden
-- representar con el tipo de dato algebraico 
--    data Arbol = H
--               | N a Arbol Arbol
-- Por ejemplo, los árboles
--        3                7     
--       / \              / \    
--      2   4            5   8   
--     / \   \          / \   \  
--    1   3   5        6   4   10
--                        /   /
--                       9   1
-- se representan por
--    ej1, ej2 :: Arbol 
--    ej1 = N 3 (N 2 (N 1 H H) (N 3 H H)) (N 4 H (N 5 H H))
--    ej2 = N 7 (N 5 (N 6 H H) (N 4 (N 9 H H) H)) (N 8 H (N 10 (N 1 H H) H))
--
-- Definir la función
--    suma :: Arbol -> Int
-- tal que (suma a) es la suma de todos los nodos a una distancia par
-- de la raíz del árbol a menos la suma de todos los nodos a una
-- distancia impar de la raíz. Por ejemplo,
--    suma ej1  ==  6
--    suma ej2  ==  4
-- ya que
--     (3 + 1+3+5) - (2+4)        = 6
--     (7 + 6+4+10) - (5+8 + 9+1) = 4 
-- ---------------------------------------------------------------------

data Arbol = H
           | N Int Arbol Arbol

ej1, ej2 :: Arbol 
ej1 = N 3 (N 2 (N 1 H H) (N 3 H H)) (N 4 H (N 5 H H))
ej2 = N 7 (N 5 (N 6 H H) (N 4 (N 9 H H) H)) (N 8 H (N 10 (N 1 H H) H))

suma :: Arbol -> Int
suma H         = 0
suma (N x i d) = x - suma i - suma d

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    cercano :: (a -> Bool) -> Int -> [a] -> Maybe a
-- tal que (cercano p n xs) es el elemento de xs más cercano a n que
-- verifica la propiedad p. La búsqueda comienza en n y los elementos se
-- analizan en el siguiente orden: n, n+1, n-1, n+2, n-2,... Por ejemplo, 
--    cercano (`elem` "aeiou") 6 "Sevilla"     ==  Just 'a'
--    cercano (`elem` "aeiou") 1 "Sevilla"     ==  Just 'e'
--    cercano (`elem` "aeiou") 2 "Sevilla"     ==  Just 'i'
--    cercano (`elem` "aeiou") 5 "Sevilla"     ==  Just 'a'
--    cercano (`elem` "aeiou") 9 "Sevilla"     ==  Just 'a'
--    cercano (`elem` "aeiou") (-3) "Sevilla"  ==  Just 'e'
--    cercano (`elem` "obcd")  1 "Sevilla"     ==  Nothing
--    cercano (>100) 4 [200,1,150,2,4]         ==  Just 150
--    cercano even 5 [1,3..99]                 ==  Nothing
--    cercano even 2 [1,4,6,8,0]               ==  Just 6
--    cercano even 2 [1,4,7,8,0]               ==  Just 8
--    cercano even 2 [1,4,7,5,0]               ==  Just 4
--    cercano even 2 [1,3,7,5,0]               ==  Just 0
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

cercano :: (a -> Bool) -> Int -> [a] -> Maybe a
cercano p n xs | null ys   = Nothing
               | otherwise = Just (head ys)
    where ys = filter p (ordenaPorCercanos xs n)

-- (ordenaPorCercanos xs n) es la lista de los elementos de xs que
-- ocupan las posiciones n, n+1, n-1, n+2, n-2... Por ejemplo, 
--    ordenaPorCercanos [0..9] 4     ==  [4,5,3,6,2,7,1,8,0,9]
--    ordenaPorCercanos [0..9] 7     ==  [7,8,6,9,5,4,3,2,1,0]
--    ordenaPorCercanos [0..9] 2     ==  [2,3,1,4,0,5,6,7,8,9]
--    ordenaPorCercanos [0..9] (-3)  ==  [0,1,2,3,4,5,6,7,8,9]
--    ordenaPorCercanos [0..9] 20    ==  [9,8,7,6,5,4,3,2,1,0]
ordenaPorCercanos :: [a] -> Int -> [a]
ordenaPorCercanos xs n 
    | n < 0          = xs
    | n >= length xs = reverse xs
    | otherwise      = z : intercala zs (reverse ys)
    where (ys,(z:zs)) = splitAt n xs

-- (intercala xs ys) es la lista obtenida intercalando los elementos de
-- las lista xs e ys. Por ejemplo,
--    intercala [1..4] [5..10]   ==  [1,5,2,6,3,7,4,8,9,10]
--    intercala [5..10] [1..4]   ==  [5,1,6,2,7,3,8,4,9,10]
intercala :: [a] -> [a] -> [a]
intercala [] ys         = ys
intercala xs []         = xs
intercala (x:xs) (y:ys) = x : y : intercala xs ys

-- 2ª solución (usando find)
-- =========================

cercano2 :: (a -> Bool) -> Int -> [a] -> Maybe a
cercano2 p n xs = find p (ordenaPorCercanos xs n)

