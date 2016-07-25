-- Informática (1º del Grado en Matemáticas)
-- 5º examen de evaluación continua (19 de mayo de 2014)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1. De una lista se pueden extraer los elementos
-- consecutivos repetidos indicando el número de veces que se repite
-- cada uno. Por ejemplo, la lista [1,1,7,7,7,5,5,7,7,7,7] comienza con
-- dos 1, seguido de tres 7, dos 5 y cuatro 7; por tanto, la extracción
-- de consecutivos repetidos devolverá [(2,1),(3,7),(2,5),(4,7)]. En
-- [1,1,7,5,7,7,7,7], la extracción será [(2,1),(4,7)] ya que el primer
-- 7 y el 5 no se repiten. 
-- 
-- Definir la funcion
--    extraer :: Eq a => [a] -> [(Int,a)]
-- tal que (extraer xs) es la lista que resulta de la extracción de
-- consecutivos repetidos en la lista xs. Por ejemplo, 
--    extraer [1,1,7,7,7,5,5,7,7,7,7]  ==  [(2,1),(3,7),(2,5),(4,7)]
--    extraer "HHoolllla"              ==  [(2,'H'),(2,'o'),(4,'l')]
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión)
-- ===============================

extraer :: Eq a => [a] -> [(Int,a)]
extraer xs = [(length (y:ys),y) | (y:ys) <- group xs, not (null ys)]

-- 2ª definición (por recursión)
-- =============================

extraer2 :: Eq a => [a] -> [(Int,a)]
extraer2 [] = []
extraer2 (x:xs) | n == 0    = extraer2 (drop n xs)
                | otherwise = (1+n,x) : extraer2 (drop n xs)
    where n = length (takeWhile (==x) xs)

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Partiendo de un número a, se construye la sucesión 
-- de listas [xs(1), xs(2) ,xs(3), ...] tal que 
--    * xs(1) = [x(1,1), x(1,2), x(1,3), ....], donde 
--      x(1,1) = a + el primer  primo mayor que a,
--      x(1,2) = a + el segundo primo mayor que a, ...
--    * xs(2) = [x(2,1), x(2,2), x(2,3), ....], donde 
--      x(2,i) = x(1,i) + el primer primo mayor que x(1,1), 
--    * xs(3) = [x(2,1), x(2,2), x(3,3), ....], donde 
--      x(3,i) = x(2,i) + el primer primo mayor que x(2,1), 
-- Por ejemplo, si empieza con a = 15, la sucesión es 
--    [[15+17, 15+19, 15+23, ...],       
--     [(15+17)+37, (15+19)+37,(15+23)+41, ...],
--     [((15+17)+37)+71, ...]]                  
--    = [[32,34,38,...],[69,71,79,...],[140,144,162,...],...]
-- 
-- Definir la función
--    sucesionN :: Integer -> Int -> [Integer]
-- tal que (sucesionN x n) es elemento n-ésimo de la sucesión que
-- empieza por a. Por ejemplo, 
--    take 10 (sucesionN 15 2) ==  [69,71,79,91,93,105,115,117,129,139]
-- ---------------------------------------------------------------------

sucesionN :: Integer -> Int -> [Integer]
sucesionN x 1 = [x+y | y <- primos, y > x]
sucesionN x n = zipWith (+) (map menorPrimoMayor (sucesionN x (n-1)))
                            (sucesionN x (n-1))

menorPrimoMayor :: Integer -> Integer
menorPrimoMayor x = head [y | y <- primos, y > x]

primos :: [Integer]
primos = [x | x <- [1 .. ], factores x == [1,x]]

factores :: Integer -> [Integer]
factores x = [y | y <- [1 ..x], mod x y ==0]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función
--    sucesion :: Integer -> [[Integer]]
-- tal que (sucesion a) es la sucesión construida a partir de a. Por
-- ejemplo, 
--    ghci> take 5 (map (take 4)(sucesion 15))
--    [[32,34,38,44],[69,71,79,91],[140,144,162,188],[289,293,325,379], 
--     [582,600,656,762]]
-- ---------------------------------------------------------------------

sucesion :: Integer -> [[Integer]]
sucesion a = [sucesionN a n | n <- [1..]]

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Definir la función
--    cuenta :: Integer -> Integer -> Int -> [Int]
-- tal que (cuenta a b n) es la lista del número de elementos de las
-- primeras n listas de la (sucesion a) que son menores que b. Por
-- ejemplo, 
--    ghci> cuenta 15 80 5
--    [12,3,0,0,0]
-- ---------------------------------------------------------------------

cuenta :: Integer -> Integer -> Int -> [Int]
cuenta a b n = 
    map (length . takeWhile (< b)) [sucesionN a m | m <- [1 .. n]]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función          
--    simetricos:: Eq a => [a] -> [a]
-- tal que (simetricos xs) es la lista de los elementos de xs que
-- coinciden con su simétricos. Por ejemplo,
--    simetricos [1,2,3,4,3,2,1]     == [1,2,3]
--    simetricos [1,2,5,4,3,4,3,2,1] == [1,2,4]
--    simetricos "amiima"            == "ami"
--    simetricos "ala"               == "a"
--    simetricos [1..20]             == []
-- ---------------------------------------------------------------------

simetricos:: Eq a => [a] -> [a]
simetricos xs = 
    [x | (x,y) <- zip (take m xs) (take m (reverse xs)), x==y]
    where m = div (length xs) 2

-- ---------------------------------------------------------------------
-- Ejercicio 4. La matrices piramidales son las formadas por unos y ceros
-- de forma que los unos forman una pirámide. Por ejemplo, 
--   |1|   |0 1 0|   |0 0 1 0 0|   |0 0 0 1 0 0 0|
--         |1 1 1|   |0 1 1 1 0|   |0 0 1 1 1 0 0|
--                   |1 1 1 1 1|   |0 1 1 1 1 1 0|
--                                 |1 1 1 1 1 1 1|
-- 
-- El tipo de las matrices se define por
--    type Matriz a = Array (Int,Int) a
-- Por ejemplo, las matrices anteriores se definen por
--    p1, p2, p3 :: Matriz Int
--    p1 = listArray ((1,1),(1,1)) [1]
--    p2 = listArray ((1,1),(2,3)) [0,1,0,
--                                  1,1,1]
--    p3 = listArray ((1,1),(3,5)) [0,0,1,0,0,
--                                  0,1,1,1,0,
--                                  1,1,1,1,1]
--
-- Definir la función
--    esPiramidal :: (Eq a, Num a) => Matriz a -> Bool
-- tal que (esPiramidal p) se verifica si la matriz p es piramidal. Por
-- ejemplo, 
--    esPiramidal p3                                        ==  True
--    esPiramidal (listArray ((1,1),(2,3)) [0,1,0, 1,5,1])  ==  False
--    esPiramidal (listArray ((1,1),(2,3)) [0,1,1, 1,1,1])  ==  False
--    esPiramidal (listArray ((1,1),(2,3)) [0,1,0, 1,0,1])  ==  False
-- ---------------------------------------------------------------------

type Matriz a = Array (Int,Int) a

p1, p2, p3 :: Matriz Int
p1 = listArray ((1,1),(1,1)) [1]
p2 = listArray ((1,1),(2,3)) [0,1,0,
                              1,1,1]
p3 = listArray ((1,1),(3,5)) [0,0,1,0,0,
                              0,1,1,1,0,
                              1,1,1,1,1]

esPiramidal :: (Eq a, Num a) => Matriz a -> Bool
esPiramidal p =
    p == listArray ((1,1),(n,m)) (concat (filasPiramidal n))
    where (_,(n,m)) = bounds p 

-- (filasPiramidal n) es la lista dela filas de la matriz piramidal de n
-- filas. Por ejemplo,
--    filasPiramidal 1  ==  [[1]]
--    filasPiramidal 2  ==  [[0,1,0],[1,1,1]]
--    filasPiramidal 3  ==  [[0,0,1,0,0],[0,1,1,1,0],[1,1,1,1,1]]
filasPiramidal 1 = [[1]]
filasPiramidal n = [0:xs++[0] | xs <- filasPiramidal (n-1)] ++ 
                   [replicate (2*n-1) 1]

-- 2ª definición
-- =============

esPiramidal2 :: (Eq a, Num a) => Matriz a -> Bool
esPiramidal2 p =
    p == piramidal n
    where (_,(n,_)) = bounds p 

-- (piramidal n) es la matriz piramidal con n filas. Por ejemplo,
--    ghci> piramidal 3
--    array ((1,1),(3,5)) [((1,1),0),((1,2),0),((1,3),1),((1,4),0),((1,5),0),
--                         ((2,1),0),((2,2),1),((2,3),1),((2,4),1),((2,5),0),
--                         ((3,1),1),((3,2),1),((3,3),1),((3,4),1),((3,5),1)]
piramidal :: (Eq a, Num a) => Int -> Matriz a
piramidal n =
    array ((1,1),(n,2*n-1)) [((i,j),f i j) | i <- [1..n], j <- [1..2*n-1]]
    where f i j | j <= n-i  = 0
                | j <  n+i  = 1
                | otherwise = 0

-- ---------------------------------------------------------------------
-- Ejercicio 5. Los árboles se pueden representar mediante el siguiente
-- tipo de dato 
--    data Arbol a = N a [Arbol a]
--                       deriving Show
-- Por ejemplo, los árboles
--      1               3               3
--     / \             /|\            / | \
--    2   3           / | \          /  |  \
--        |          5  4  7        5   4   7
--        4          |     /\       |   |  / \
--                   6    2  1      6   1 2   1
--                                     / \
--                                    2   3
--                                        |
--                                        4

-- se representan por
--    ej1, ej2,ej3 :: Arbol Int
--    ej1 = N 1 [N 2 [],N 3 [N 4 []]]
--    ej2 = N 3 [N 5 [N 6 []], 
--               N 4 [], 
--               N 7 [N 2 [], N 1 []]]
--    ej3 = N 3 [N 5 [N 6 []], 
--               N 4 [N 1 [N 2 [],N 3 [N 4 []]]], 
--               N 7 [N 2 [], N 1 []]]
-- 
-- Definir la función
--     ramifica :: Arbol a -> Arbol a -> (a -> Bool) -> Arbol a
-- tal que (ramifica a1 a2 p) el árbol que resulta de añadir una copia
-- del árbol a2 a los nodos de a1 que cumplen un predicado p. Por
-- ejemplo, 
--    ghci> ramifica (N 3 [N 5 [N 6 []],N 4 [],N 7 [N 2 [],N 1 []]]) (N 8 []) (>5)
--    N 3 [N 5 [N 6 [N 8 []]],N 4 [],N 7 [N 2 [],N 1 [],N 8 []]]
-- ---------------------------------------------------------------------

data Arbol a = N a [Arbol a]
                   deriving Show

ej1, ej2,ej3 :: Arbol Int
ej1 = N 1 [N 2 [],N 3 [N 4 []]]
ej2 = N 3 [N 5 [N 6 []], 
           N 4 [], 
           N 7 [N 2 [], N 1 []]]
ej3 = N 3 [N 5 [N 6 []], 
           N 4 [N 1 [N 2 [],N 3 [N 4 []]]], 
           N 7 [N 2 [], N 1 []]]

ramifica (N x xs) a2 p  
         | p x       = N x ([ramifica a a2 p | a <- xs] ++ [a2])
         | otherwise = N x  [ramifica a a2 p | a <- xs]
