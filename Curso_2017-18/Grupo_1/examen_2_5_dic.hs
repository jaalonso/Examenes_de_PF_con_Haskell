-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 2º examen de evaluación continua (5 de diciembre de 2017)
-- ---------------------------------------------------------------------

-- Nota: La puntuación de cada ejercicio es 2.5 puntos.

-- ---------------------------------------------------------------------
-- § Librerías                                                        --
-- ---------------------------------------------------------------------

import Data.List
import Data.Maybe
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercico 1. Definir la función 
--    sublistasSuma :: (Num a, Eq a) => [a] -> a -> [[a]]
-- tal que (sublistasSuma xs m) es la lista de sublistas de xs cuya suma
-- es m. Por ejemplo,
--    sublistasSuma [1..5] 10 == [[2,3,5],[1,4,5],[1,2,3,4]]
--    sublistasSuma [1..5] 23 == []
--    length (sublistasSuma [1..20] 145) == 5337
-- ---------------------------------------------------------------------

-- 1ª solución (por orden superior):
sublistasSuma1 :: (Num a, Eq a) => [a] -> a -> [[a]]
sublistasSuma1 xs m = filter ((==m) . sum) (subsequences xs)

-- 2ª solución (por recursión:
sublistasSuma2 :: [Int] -> Int -> [[Int]]
sublistasSuma2 xs = aux (sort xs) 
  where aux [] m = []
        aux (y:ys) m | y == m    = [[m]]
                     | y > m     = []
                     | otherwise = aux ys m ++ map (y:) (aux ys (m-y))

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. La sucesión de Padovan es la secuencia de números
-- enteros P(n) definida por los valores iniciales
--    P(0) = P(1) = P( 2) = 1,
-- y por la relación
--    P (n) = P(n−2) + P(n−3) . 
-- 
-- Los primeros valores de P(n) son
--    1, 1, 1, 2, 2, 3, 4, 5, 7, 9, 12, 16, 21, 28, 37,... 
-- 
-- Definir la funciones
--    padovan :: Int -> Integer
--    sucPadovan :: [Integer]
-- tales que
--    sucPadovan es la sucesión así construida, y 
--    (padovan n) es el término n-simo de la sucesión
-- Por ejemplo,
--    padovan 10                   == 12
--    padovan 100                  == 1177482265857
--    length $ show $ padovan 1000 == 122
--    take 14 sucPadovan           == [1,1,1,2,2,3,4,5,7,9,12,16,21,28]
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

padovan1 :: Int -> Integer
padovan1 n | n <= 2    = 1
           | otherwise = padovan1 (n-2) + padovan1 (n-3)

sucPadovan1 :: [Integer]
sucPadovan1 = map padovan1 [0..]

-- 2ª solución
-- ===========

sucPadovan2 :: [Integer]
sucPadovan2 = 1:1:1: zipWith (+) sucPadovan2 (tail sucPadovan2) 

padovan2 :: Int -> Integer
padovan2 n = sucPadovan2 `genericIndex` n

-- Comparación de eficiencia
-- =========================

--    ghci> sucPadovan1 !! 60
--    15346786
--    (12.53 secs, 6,752,729,800 bytes)
--    ghci> sucPadovan2 !! 60
--    15346786
--    (0.00 secs, 152,648 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Comprobar con QuickCheck la siguiente propiedad: 
--                n
--               ---
--               \
--               /    P(j)   = P(n+4) - 2
--               ---
--               j=0
-- ---------------------------------------------------------------------

-- La propiedad es
propPadovan :: Int -> Bool
propPadovan n = sum (take m sucPadovan2) == padovan2 (m+4) - 2
  where m = abs n

-- La comprobación es
--    ghci> quickCheck propPadovan
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3. Los árboles con un número variable de sucesores se
-- pueden representar mediante el siguiente tipo de dato
--    data Arbol a = N a [Arbol a]
--      deriving Show
-- Por ejemplo, los árboles
--      1         1             1          
--     / \       / \           / \   
--    8   3     8   3         8   3  
--        |        /|\       /|\  |   
--        4       4 5 6     4 5 6 7
-- se representan por
--    ej1, ej2, ej3 :: Arbol Int
--    ej1 = N 1 [N 8 [],N 3 [N 4 []]]
--    ej2 = N 1 [N 8 [], N 3 [N 4 [], N 5 [], N 6 []]]
--    ej3 = N 1 [N 8 [N 4 [], N 5 [], N 6 []], N 3 [N 7 []]]
-- 
-- Definir la función
--     caminosDesdeRaiz :: Arbol a -> [[a]]
-- tal que (caminosDesdeRaiz x) es la lista de todos los caminos desde
-- la raiz. Por ejemplo,
--     caminosDesdeRaiz ej1 == [[1,8],[1,3,4]]
--     caminosDesdeRaiz ej2 == [[1,8],[1,3,4],[1,3,5],[1,3,6]]
--     caminosDesdeRaiz ej3 == [[1,8,4],[1,8,5],[1,8,6],[1,3,7]]
-- ---------------------------------------------------------------------

data Arbol a = N a [Arbol a]
  deriving Show

ej1, ej2, ej3 :: Arbol Int
ej1 = N 1 [N 8 [],N 3 [N 4 []]]
ej2 = N 1 [N 8 [], N 3 [N 4 [], N 5 [], N 6 []]]
ej3 = N 1 [N 8 [N 4 [], N 5 [], N 6 []], N 3 [N 7 []]]

caminosDesdeRaiz :: Arbol a -> [[a]]
caminosDesdeRaiz (N r []) = [[r]]
caminosDesdeRaiz (N r as) = map (r:) (concatMap caminosDesdeRaiz as)

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. La lista [7, 10, 12, 1, 2, 4] no está ordenada, pero si
-- consideramos las listas que se pueden formar cíclicamente a partir de
-- cada elemento, obtenemos:
--    [7, 10, 12, 1, 2, 4]
--    [10, 12, 1, 2, 4, 7]
--    [12, 1, 2, 4, 7, 10]
--    [1, 2, 4, 7, 10, 12]  ** ordenada **
--    [2, 4, 7, 10, 12, 1]
--    [4, 7, 10, 12, 1, 2]
-- Se observa que una de ellas está ordenada.
-- 
-- Se dice que una lista [x(0), ..., x(n)] es cíclicamente ordenable
-- si existe un índice i tal que la lista
--    [x(i), x(i+1), ..., x(n), x(0), ..., x(i-1)]
-- está ordenada. 
-- 
-- Definir la función
--    ciclicamenteOrdenable :: Ord a => [a] -> Bool
-- tal que (ciclicamenteOrdenable xs) se verifica si xs es una lista
-- cíclicamente ordenable. Por ejemplo,
--    ciclicamenteOrdenable [7,10,12,1,2,4]  ==  True
--    ciclicamenteOrdenable [7,20,12,1,2,4]  ==  False
-- ---------------------------------------------------------------------

ciclicamenteOrdenable :: Ord a => [a] -> Bool
ciclicamenteOrdenable xs =
  any ordenada (zipWith (++) (tails xs) (inits xs))

ordenada :: Ord a => [a] -> Bool
ordenada xs = and (zipWith (<=) xs (tail xs))

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función
--    posicionOC :: Ord a => [a] -> Maybe Int
-- tal que (posicionOC xs) es (Just i) si i es el índice a partir del
-- cual la lista xs está ordenada cíclicamente; o bien Nothing, en caso
-- contrario. Por ejemplo,
--    posicionOC [7,10,12,1,2,4]            == Just 3
--    posicionOC [4,5,6,1,2,3]              == Just 3
--    posicionOC [4,5,6,1,2,3,7]            == Nothing
--    posicionOC ([10^3..10^4] ++ [1..999]) == Just 9001
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

posicionOC :: Ord a => [a] -> Maybe Int
posicionOC xs | null is   = Nothing
              | otherwise = Just (head is)
  where is = ciclosOrdenados xs

-- (cicloOrdenado xs i) se verifica si el ciclo i-ésimo de xs está
-- ordenado; es decir, si
--    [x(i), x(i+1), ..., x(n), x(0), ..., x(i-1)]
-- está ordenado. Por ejemplo,
--    cicloOrdenado [4,5,6,1,2,3] 3  ==  True
--    cicloOrdenado [4,5,6,1,2,3] 2  ==  False
--    cicloOrdenado [4,5,6,1,2,3] 4  ==  False
cicloOrdenado :: Ord a => [a] -> Int -> Bool
cicloOrdenado xs i = ordenada (drop i xs ++ take i xs)

-- (ciclosOrdenados xs) es el conjunto de índices i tales que el ciclo
-- i-ésimo de xs está ordenado. Por ejemplo,
--    ciclosOrdenados [7,10,12,1,2,4]  ==  [3]
--    ciclosOrdenados [7,20,12,1,2,4]  ==  []
ciclosOrdenados :: Ord a => [a] -> [Int]  
ciclosOrdenados xs =
  [i | i <- [0..length xs - 1]
     , cicloOrdenado xs i]

-- 2ª solución
-- ===========

posicionOC2 :: Ord a => [a] -> Maybe Int
posicionOC2 = listToMaybe . ciclosOrdenados


