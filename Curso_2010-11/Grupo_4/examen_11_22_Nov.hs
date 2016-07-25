-- Informática (1º del Grado en Matemáticas)
-- Examen de la 3ª convocatoria (22 de noviembre de 2011)
-- ---------------------------------------------------------------------

import Data.Array
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por comprensión, la función
--    barajaC :: [a] -> [a] -> [a]
-- tal que (barajaC xs ys) es la lista obtenida intercalando los
-- elementos de las listas xs e ys. Por ejemplo, 
--    barajaC [1,6,2] [3,7]        ==  [1,3,6,7]
--    barajaC [1,6,2] [3,7,4,9,0]  ==  [1,3,6,7,2,4]
-- ---------------------------------------------------------------------

barajaC :: [a] -> [a] -> [a]
barajaC xs ys = concat [[x,y] | (x,y) <- zip xs ys]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, por recursión, la función
--    barajaR :: [a] -> [a] -> [a]
-- tal que (barajaR xs ys) es la lista obtenida intercalando los
-- elementos de las listas xs e ys. Por ejemplo, 
--    barajaR [1,6,2] [3,7]        ==  [1,3,6,7]
--    barajaR [1,6,2] [3,7,4,9,0]  ==  [1,3,6,7,2,4]
-- ---------------------------------------------------------------------

barajaR :: [a] -> [a] -> [a]
barajaR (x:xs) (y:ys) = x : y : barajaR xs ys
barajaR _       _     = []

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comprobar con QuickCheck que la longitud de
-- (barajaR xs y1) es el doble del mínimo de las longitudes de xs es ys.
-- ---------------------------------------------------------------------

prop_baraja :: [Int] -> [Int] -> Bool
prop_baraja xs ys = 
    length (barajaC xs ys) == 2 * min (length xs) (length ys)

-- La comprobación es
--    ghci> quickCheck prop_baraja
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Un número n es refactorizable si el número de los
-- divisores de n es un divisor de n. 
-- 
-- Definir la constante  
--    refactorizables :: [Int]
-- tal que refactorizables es la lista de los números
-- refactorizables. Por ejemplo,
--    take 10 refactorizables  ==  1,2,8,9,12,18,24,36,40,56]
-- ---------------------------------------------------------------------

refactorizables :: [Int]
refactorizables = 
    [n | n <- [1..], length (divisores n) `divide` n]

-- (divide x y) se verifica si x divide a y. Por ejemplo,
--    divide 2 6  ==  True
--    divide 2 7  ==  False
--    divide 0 7  ==  False
--    divide 0 0  ==  True
divide :: Int -> Int -> Bool
divide 0 y = y == 0
divide x y = y `rem`x == 0

-- (divisores n) es la lista de los divisores de n. Por ejemplo,
--    divisores 36  ==  [1,2,3,4,6,9,12,18,36]
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `rem` x == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Un número n es redescompible si el número de
-- descomposiciones de n como producto de dos factores distintos divide
-- a n. 
-- 
-- Definir la función 
--    redescompible :: Int -> Bool
-- tal que (redescompible x) se verifica si x es
-- redescompible. Por ejemplo,
--    redescompible 56  ==  True
--    redescompible 57  ==  False
-- ---------------------------------------------------------------------

redescompible :: Int -> Bool
redescompible n = nDescomposiciones n `divide` n

nDescomposiciones :: Int -> Int
nDescomposiciones n = 
    2 * length [(x,y) | x <- [1..n], y <- [x+1..n], x*y == n]

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Definir la función
--    prop_refactorizable :: Int -> Bool
-- tal que (prop_refactorizable n) se verifica si para todo x
-- entre los n primeros números refactorizables se tiene que x es
-- redescompible syss x no es un cuadrado. Por ejemplo,
--    prop_refactorizable 10  ==  True
-- ---------------------------------------------------------------------

prop_refactorizable :: Int -> Bool
prop_refactorizable n = 
    and [(nDescomposiciones x `divide` x) == not (esCuadrado x)  
         | x <- take n refactorizables] 

-- (esCuadrado x) se verifica si  x es un cuadrado perfecto; es decir,
-- si existe un y tal que y^2 es igual a x. Por ejemplo,  
--    esCuadrado 16  ==  True
--    esCuadrado 17  ==  False
esCuadrado :: Int -> Bool
esCuadrado x = y^2 == x
    where y = round (sqrt (fromIntegral x))
 
-- Otra solución, menos eficiente, es
esCuadrado' :: Int -> Bool
esCuadrado' x = 
    [y | y <- [1..x], y^2 == x] /= []

-- ---------------------------------------------------------------------
-- Ejercicio 3. Un árbol binario de búsqueda (ABB) es un árbol binario
-- tal que el de cada nodo es mayor que los valores de su subárbol
-- izquierdo y es menor que los valores de su subárbol derecho y,
-- además, ambos subárboles son árboles binarios de búsqueda. Por
-- ejemplo, al almacenar los valores de [8,4,2,6,3] en un ABB se puede
-- obtener el siguiente ABB: 
--    
--       5                
--      / \               
--     /   \              
--    2     6             
--         / \            
--        4   8           
-- 
-- Los ABB se pueden representar como tipo de dato algebraico:
--    data ABB = V
--             | N Int ABB ABB
--             deriving (Eq, Show)
-- Por ejemplo, la definición del ABB anteriore es
--    ej :: ABB
--    ej = N 3 (N 2 V V) (N 6 (N 4 V V) (N 8 V V))
-- Definir la función 
--    inserta :: Int -> ABB -> ABB
-- tal que (inserta v a) es el árbol obtenido añadiendo el valor v al
-- ABB a, si no es uno de sus valores. Por ejemplo, 
--    ghci>  inserta 5 ej
--    N 3 (N 2 V V) (N 6 (N 4 V (N 5 V V)) (N 8 V V))
--    ghci>  inserta 1 ej
--    N 3 (N 2 (N 1 V V) V) (N 6 (N 4 V V) (N 8 V V))
--    ghci>  inserta 2 ej
--    N 3 (N 2 V V) (N 6 (N 4 V V) (N 8 V V))
-- ---------------------------------------------------------------------

data ABB = V
         | N Int ABB ABB
         deriving (Eq, Show)

ej :: ABB 
ej = N 3 (N 2 V V) (N 6 (N 4 V V) (N 8 V V))

inserta :: Int -> ABB -> ABB
inserta v' V = N v' V V
inserta v' (N v i d) 
    | v' == v   = N v i d
    | v' < v    = N v (inserta v' i) d
    | otherwise = N v i (inserta v' d)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Se consideran los tipos de los vectores y de las
-- matrices definidos por 
--    type Vector a = Array Int a
--    type Matriz a = Array (Int,Int) a
-- 
-- Definir la función
--    antidiagonal :: (Num a, Eq a) => Matriz a -> Bool
-- tal que (antidiagonal m) se verifica si es cuadrada y todos los
-- elementos de m que no están en su diagonal secundaria son nulos. Por
-- ejemplo,  si m1 y m2 son las matrices definidas por
--    m1, m2 :: Matriz Int
--    m1 = array ((1,1),(3,3)) [((1,1),7),((1,2),0),((1,3),4),
--                              ((2,1),0),((2,2),6),((2,3),0),
--                              ((3,1),0),((3,2),0),((3,3),5)]
--    m2 = array ((1,1),(3,3)) [((1,1),0),((1,2),0),((1,3),4),
--                              ((2,1),0),((2,2),6),((2,3),0),
--                              ((3,1),0),((3,2),0),((3,3),0)]
-- entonces   
--    antidiagonal m1  ==  False
--    antidiagonal m2  ==  True
-- ---------------------------------------------------------------------

type Vector a = Array Int a
type Matriz a = Array (Int,Int) a

m1, m2 :: Matriz Int
m1 = array ((1,1),(3,3)) [((1,1),7),((1,2),0),((1,3),4),
                          ((2,1),0),((2,2),6),((2,3),0),
                          ((3,1),0),((3,2),0),((3,3),5)]
m2 = array ((1,1),(3,3)) [((1,1),0),((1,2),0),((1,3),4),
                          ((2,1),0),((2,2),6),((2,3),0),
                          ((3,1),0),((3,2),0),((3,3),0)]

antidiagonal :: (Num a, Eq a) => Matriz a -> Bool 
antidiagonal p = 
    m == n && nula [p!(i,j) | i <- [1..n], j <- [1..n], j /= n+1-i]
    where (m,n) = snd (bounds p)

nula :: (Num a, Eq a) => [a] -> Bool
nula xs = xs == [0 | x <- xs]
