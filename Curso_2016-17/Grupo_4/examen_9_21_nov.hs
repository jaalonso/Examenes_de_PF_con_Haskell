-- Informática (1º del Grado en Matemáticas)
-- Convocatoria de diciembre (21 de noviembre de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías                                                        --
-- ---------------------------------------------------------------------

import Data.List
import Data.Matrix

-- ---------------------------------------------------------------------
-- Ejercicio 1. Dado un conjunto A de enteros positivos, una partición
-- de A son dos conjuntos disjuntos y no vacíos A1 y A2 cuya unión es
-- A. Decimos que una partición es "buena" si el mínimo común múltiplo
-- de los elementos de A1 coincide con el máximo común divisor de los
-- elementos de A2. 
-- 
-- Definir la función
--    particionesBuenas :: [Int] -> [([Int],[Int])]
-- tal que (particionesBuenas xs) es la lista de las particiones buenas
-- de xs. Por ejemplo,
--    particionesBuenas [1,2,3,6]  == [([1],[2,3,6]),([1,2,3],[6])]
--    particionesBuenas [1..10]    == [([1],[2,3,4,5,6,7,8,9,10])]
--    particionesBuenas [2..10]    == []
-- ---------------------------------------------------------------------

-- 1ª definición de particionesBuenas
particionesBuenas :: [Int] -> [([Int],[Int])]
particionesBuenas xs =
  [p | p <- particiones xs
     , esBuena p]

-- 2ª definición de particionesBuenas
particionesBuenas2 :: [Int] -> [([Int],[Int])]
particionesBuenas2 xs =
  filter esBuena (particiones xs)

-- 3ª definición de particionesBuenas
particionesBuenas3 :: [Int] -> [([Int],[Int])]
particionesBuenas3 =
  filter esBuena . particiones 

-- (particiones xs) es la lista de las particiones de xs. Por ejemplo,
--    ghci> particiones [3,2,5]
--    [([],[3,2,5]),
--     ([3],[2,5]),
--     ([2],[3,5]),
--     ([3,2],[5]),
--     ([5],[3,2]),
--     ([3,5],[2]),
--     ([2,5],[3]),
--     ([3,2,5],[])]

-- 1ª definición de particiones
particiones1 :: [Int] -> [([Int],[Int])]
particiones1 xs = [(ys,xs \\ ys) | ys <- subsequences xs]

-- 2ª definición de particiones
particiones2 :: [Int] -> [([Int],[Int])]
particiones2 xs = map f (subsequences xs)
  where f ys = (ys, xs \\ ys)

-- 3ª definición de particiones
particiones3 :: [Int] -> [([Int],[Int])]
particiones3 xs = map (\ys -> (ys, xs \\ ys)) (subsequences xs)

-- Comparación de eficiencia de particiones
--    ghci> length (particiones1 [1..24])
--    16777216
--    (3.04 secs, 4,160,894,648 bytes)
--    ghci> length (particiones2 [1..24])
--    16777216
--    (0.72 secs, 3,221,368,088 bytes)
--    ghci> length (particiones3 [1..24])
--    16777216
--    (0.72 secs, 3,221,367,168 bytes)

-- Usaremos la 3ª definición de particiones
particiones :: [Int] -> [([Int],[Int])]
particiones = particiones3

-- (esBuena (xs,ys)) se verifica si las listas xs e ys son no vacía y el
-- mínimo comúm múltiplo de xs es igual al máximo común divisor de ys.
esBuena :: ([Int],[Int]) -> Bool
esBuena (xs,ys) =
     not (null xs)
  && not (null ys)
  && mcmL xs == mcdL ys

-- (mcdL xs) es el máximo común divisor de xs. 

-- 1ª definición de mcdL
mcdL1 :: [Int] -> Int
mcdL1 [x] = x
mcdL1 (x:y:xs) = gcd x (mcdL (y:xs))

-- 2ª definición de mcdL
mcdL2 :: [Int] -> Int
mcdL2 = foldl1' gcd

-- Comparación de eficiencia de mcdL
--    ghci> mcdL1 [1..3*10^6]
--    1
--    (2.02 secs, 1,770,213,520 bytes)
--    ghci> mcdL2 [1..3*10^6]
--    1
--    (0.50 secs, 1,032,135,400 bytes)
--
--    ghci> mcdL1 [1..5*10^6]
--    *** Exception: stack overflow
--    ghci> mcdL2 [1..5*10^6]
--    1
--    (1.42 secs, 1,720,137,128 bytes)

-- Usaremos la 2ª definición de mcdL
mcdL :: [Int] -> Int
mcdL = mcdL2

-- (mcmL xs) es el mínimo común múltiplo de xs. 

-- 1ª definición de mcmL
mcmL1 :: [Int] -> Int
mcmL1 [x] = x
mcmL1 (x:y:xs) = lcm x (mcmL1 (y:xs))

-- 2ª definición de mcmL
mcmL2 :: [Int] -> Int
mcmL2 = foldl1' lcm

-- Comparación de eficiencia de mcmL
--    ghci> mcmL1 [1..5*10^6]
--    505479828665794560
--    (6.18 secs, 7,635,567,360 bytes)
--    ghci> mcmL2 [1..5*10^6]
--    26232203725766656
--    (2.71 secs, 5,971,248,720 bytes)
--    ghci> mcmL1 [1..10^7]
--    *** Exception: stack overflow
--    ghci> mcmL2 [1..10^7]
--    1106056739033186304
--    (5.99 secs, 12,269,073,648 bytes)

-- Usaremos la 2ª definición de mcmL
mcmL :: [Int] -> Int
mcmL = mcdL2

-- ---------------------------------------------------------------------
-- Ejercicio 2. Un número natural n se puede descomponer de varias
-- formas como suma de potencias k-ésimas de números naturales. Por
-- ejemplo, 100 se puede descomponer en potencias cuadradas
--    100 = 1 + 9 + 16 + 25 + 49
--        = 36 + 64
--        = 100
-- O bien como potencias cúbicas,
--    100 = 1 + 8 + 27 + 64
-- No hay ninguna descomposición de 100 como potencias cuartas.
-- 
-- Definir la función 
--    descomPotencia :: Int -> Int -> [[Int]]
-- tal que (descomPotencia n k) es la lista de las descomposiciones de n
-- como potencias k-ésimas de númenos naturales. Por ejemplo,
--    descomPotencia 100 2 == [[1,9,16,25,49],[36,64],[100]]
--    descomPotencia 100 3 == [[1,8,27,64]]
--    descomPotencia 100 4 == []
-- ---------------------------------------------------------------------

descomPotencia :: Int -> Int -> [[Int]]
descomPotencia x n =
  descomposiciones x (takeWhile (<=x) (map (^n) [1..]))

descomposiciones :: Int -> [Int] -> [[Int]]
descomposiciones n [] = []
descomposiciones n (x:xs)
  | x > n     = []
  | x == n    = [[n]]
  | otherwise = map (x:) (descomposiciones (n-x) xs)
                ++ descomposiciones n xs

-- ---------------------------------------------------------------------
-- Ejercicio 3. El triángulo de Pascal es un triángulo de números 
--          1
--         1 1
--        1 2 1
--      1  3 3  1
--     1 4  6  4 1
--    1 5 10 10 5 1
--   ...............
-- construido de la siguiente forma
-- + la primera fila está formada por el número 1;
-- + las filas siguientes se construyen sumando los números adyacentes
--   de la fila superior y añadiendo un 1 al principio y al final de la
--   fila. 
--
-- La matriz de Pascal es la matriz cuyas filas son los elementos de la
-- correspondiente fila del triángulo de Pascal completadas con
-- ceros. Por ejemplo, la matriz de Pascal de orden 6 es
--    |1 0  0  0 0 0|
--    |1 1  0  0 0 0|
--    |1 2  1  0 0 0|
--    |1 3  3  1 0 0|
--    |1 4  6  4 1 0|
--    |1 5 10 10 5 1|
-- 
-- Definir la función
--    matrizPascal :: Int -> Matriz Int 
-- tal que (matrizPascal n) es la matriz de Pascal de orden n. Por
-- ejemplo, 
--    ghci> matrizPascal 5
--    ( 1 0 0 0 0 )
--    ( 1 1 0 0 0 )
--    ( 1 2 1 0 0 )
--    ( 1 3 3 1 0 )
--    ( 1 4 6 4 1 )
-- ---------------------------------------------------------------------

matrizPascal :: Int -> Matrix Int
matrizPascal n = fromLists xss
    where yss = take n pascal
          xss = map (take n) (map (++ (repeat 0)) yss)

pascal :: [[Int]]
pascal = [1] : map f pascal
    where f xs = zipWith (+) (0:xs) (xs++[0])

-- ---------------------------------------------------------------------
-- Ejercicio 4. Los árboles se pueden representar mediante el siguiente
-- tipo de datos 
--    data Arbol a = N a [Arbol a]
--      deriving Show
-- Por ejemplo, los árboles

--      1         1             1          
--     / \       / \           / \   
--    8   3     5   3         5   3  
--        |        /|\       /|\  |   
--        4       4 7 6     4 7 6 7
-- se representan por
--    ej1, ej2, ej3 :: Arbol Int
--    ej1 = N 1 [N 8 [],N 3 [N 4 []]]
--    ej2 = N 1 [N 5 [], N 3 [N 4 [], N 7 [], N 6 []]]
--    ej3 = N 1 [N 5 [N 4 [], N 7 [], N 6 []], N 3 [N 7 []]] 
--
-- El peso de una rama de un árbol es la suma de los valores en sus
-- nodos y en sus hojas. Por ejemplo, en el primer árbol su rama
-- izquierda pesa 9 (1+8) y su rama derecha pesa 8 (1+3+4).
--
-- Definir la función
--    minimoPeso :: Arbol Int -> Int
-- tal que (minimoPeso x) es el mínimo de los pesos de la rama del árbol
-- x. Por ejemplo,
--    minimoPeso ej1  ==  8
--    minimoPeso ej2  ==  6
--    minimoPeso ej3  ==  10
-- ---------------------------------------------------------------------

data Arbol a = N a [Arbol a]
  deriving Show

ej1, ej2, ej3 :: Arbol Int
ej1 = N 1 [N 8 [],N 3 [N 4 []]]
ej2 = N 1 [N 5 [], N 3 [N 4 [], N 7 [], N 6 []]]
ej3 = N 1 [N 5 [N 4 [], N 7 [], N 6 []], N 3 [N 7 []]] 

-- 1ª definición
-- =============

minimoPeso :: Arbol Int -> Int
minimoPeso x = minimum (map sum (ramas x))

-- (ramas x) es la lista de las ramas del árbol x. Por ejemplo,
--    ramas ej1  ==  [[1,8],[1,3,4]]
--    ramas ej2  ==  [[1,5],[1,3,4],[1,3,7],[1,3,6]]
--    ramas ej3  ==  [[1,5,4],[1,5,7],[1,5,6],[1,3,7]]
ramas :: Arbol a -> [[a]]
ramas (N r []) = [[r]]
ramas (N r as) = [(r:rs) |  a<-as, rs <-ramas a]

-- 2ª definición
-- =============

minimoPeso2 :: Arbol Int -> Int
minimoPeso2 = minimum . map sum . ramas

-- 3ª definición
-- =============
  
minimoPeso3 :: Arbol Int -> Int
minimoPeso3 (N r []) = r
minimoPeso3 (N r as) = r + minimum (map minimoPeso3 as)
