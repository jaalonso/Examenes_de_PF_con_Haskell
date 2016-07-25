-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- Examen de la 1ª convocatoria (8 de julio de 2011)
-- ---------------------------------------------------------------------

import Data.Array
import Data.Char
import Test.QuickCheck
import GrafoConVectorDeAdyacencia 

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. El enunciado de un problema de las olimpiadas rusas de
-- matemáticas es el siguiente:
--    Si escribimos todos los números enteros empezando por el uno, uno
--    al lado del otro (o sea, 1234567891011121314...), ¿qué dígito
--    ocupa la posición 206788? 
-- En los distintos apartados de este ejercicios resolveremos el
-- problema. 
-- 
-- Definir la constante
--    cadenaDeNaturales :: String
-- tal que cadenaDeNaturales es la cadena obtenida escribiendo todos los
-- números enteros empezando por el uno. Por ejemplo,
--    take 19 cadenaDeNaturales  ==  "1234567891011121314"
-- ---------------------------------------------------------------------

cadenaDeNaturales :: String
cadenaDeNaturales = concat [show n | n <- [1..]]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función
--    digito :: Int -> Int
-- tal que (digito n) es el dígito que ocupa la posición n en la cadena
-- de los naturales (el número de las posiciones empieza por 1). Por
-- ejemplo, 
--    digito 10  ==  1
--    digito 11  ==  0
-- ---------------------------------------------------------------------

digito :: Int -> Int
digito n = digitToInt (cadenaDeNaturales !! (n-1))

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Calcular el dígito que ocupa la posición 206788 en la
-- cadena de los naturales.
-- ---------------------------------------------------------------------

-- El cálculo es 
--   ghci> digito 206788
--   7

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. El problema número 15 de los desafíos matemáticos
-- de El Pais parte de la observación de que todos los números naturales
-- tienen al menos un múltiplo no nulo que está formado solamente por
-- ceros y unos. Por ejemplo, 1x10=10, 2x5=10, 3x37=111, 4x25=100,
-- 5x2=10, 6x185=1110; 7x143=1001; 8X125=1000; 9x12345679=111111111, ...
-- y así para cualquier número natural.  
-- 
-- Definir la constante 
--    numerosCon1y0 :: [Integer]
-- tal que numerosCon1y0 es la lista de los números cuyos dígitos son 1
-- ó 0. Por ejemplo, 
--    ghci> take 15 numerosCon1y0
--    [1,10,11,100,101,110,111,1000,1001,1010,1011,1100,1101,1110,1111]
-- ---------------------------------------------------------------------

numerosCon1y0 :: [Integer]
numerosCon1y0 = 1 : concat [[10*x,10*x+1] | x <- numerosCon1y0]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función
--    multiplosCon1y0 :: Integer -> [Integer] 
-- tal que (multiplosCon1y0 n) es la lista de los múltiplos de n cuyos
-- dígitos son 1 ó 0. Por ejemplo,
--    take 4 (multiplosCon1y0 3)  ==  [111,1011,1101,1110]
-- ---------------------------------------------------------------------

multiplosCon1y0 :: Integer -> [Integer] 
multiplosCon1y0 n = 
    [x | x <- numerosCon1y0, x `rem` n == 0] 

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Comprobar con QuickCheck que todo número natural,
-- mayor que 0, tiene múltiplos cuyos dígitos son 1 ó 0.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_existe_multiplosCon1y0 :: Integer -> Property
prop_existe_multiplosCon1y0 n = 
    n > 0 ==> multiplosCon1y0 n /= []

-- La comprobación es
--    ghci> quickCheck prop_existe_multiplosCon1y0
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3. Una matriz permutación es una matriz cuadrada con
-- todos sus elementos iguales a 0, excepto uno cualquiera por cada fila
-- y columna, el cual debe ser igual a 1. 
-- 
-- En este ejercicio se usará el tipo de las matrices definido por
--    type Matriz a = Array (Int,Int) a
-- y los siguientes ejemplos de matrices
--    q1, q2, q3 :: Matriz Int
--    q1 = array ((1,1),(2,2)) [((1,1),1),((1,2),0),((2,1),0),((2,2),1)]
--    q2 = array ((1,1),(2,2)) [((1,1),0),((1,2),1),((2,1),0),((2,2),1)]
--    q3 = array ((1,1),(2,2)) [((1,1),3),((1,2),0),((2,1),0),((2,2),1)]
--
-- Definir la función
--    esMatrizPermutacion :: Num a => Matriz a -> Bool
-- tal que (esMatrizPermutacion p) se verifica si p es una matriz
-- permutación. Por ejemplo.
--    esMatrizPermutacion q1  ==  True
--    esMatrizPermutacion q2  ==  False
--    esMatrizPermutacion q3  ==  False
-- ---------------------------------------------------------------------

type Matriz a = Array (Int,Int) a

q1, q2, q3 :: Matriz Int
q1 = array ((1,1),(2,2)) [((1,1),1),((1,2),0),((2,1),0),((2,2),1)]
q2 = array ((1,1),(2,2)) [((1,1),0),((1,2),1),((2,1),0),((2,2),1)]
q3 = array ((1,1),(2,2)) [((1,1),3),((1,2),0),((2,1),0),((2,2),1)]

esMatrizPermutacion :: (Num a, Eq a) => Matriz a -> Bool
esMatrizPermutacion p = 
    and [esListaUnitaria [p!(i,j) | i <- [1..n]] | j <- [1..n]] &&
    and [esListaUnitaria [p!(i,j) | j <- [1..n]] | i <- [1..n]] 
    where ((1,1),(n,_)) = bounds p

-- (esListaUnitaria xs) se verifica si xs tiene un 1 y los restantes
-- elementos son 0. Por ejemplo,
--    esListaUnitaria [0,1,0,0]  ==  True
--    esListaUnitaria [0,1,0,1]  ==  False
--    esListaUnitaria [0,2,0,0]  ==  False
esListaUnitaria :: (Num a, Eq a) => [a] -> Bool
esListaUnitaria xs = 
    [x | x <- xs, x /= 0] == [1]

-- ---------------------------------------------------------------------
--  Ejercicio 4. Un mapa se puede representar mediante un grafo donde
--  los vértices son las regiones del mapa y hay una arista entre dos
--  vértices si las correspondientes regiones son vecinas. Por ejemplo,
--  el mapa siguiente 
--        +----------+----------+       
--        |    1     |     2    |       
--        +----+-----+-----+----+       
--        |    |           |    |       
--        | 3  |     4     | 5  |       
--        |    |           |    |       
--        +----+-----+-----+----+       
--        |    6     |     7    |       
--        +----------+----------+
-- se pueden representar por
--    mapa :: Grafo Int Int
--    mapa = creaGrafo False (1,7)
--                     [(1,2,0),(1,3,0),(1,4,0),(2,4,0),(2,5,0),(3,4,0),
--                      (3,6,0),(4,5,0),(4,6,0),(4,7,0),(5,7,0),(6,7,0)]
-- Para colorear el mapa se dispone de 4 colores definidos por   
--    data Color = A | B | C | D deriving (Eq, Show)
-- 
-- Definir la función
--    correcta :: [(Int,Color)] -> Grafo Int Int -> Bool
-- tal que (correcta ncs m) se verifica si ncs es una coloración del
-- mapa m tal que todos las regiones vecinas tienen colores distintos. 
-- Por ejemplo, 
--    correcta [(1,A),(2,B),(3,B),(4,C),(5,A),(6,A),(7,B)] mapa == True
--    correcta [(1,A),(2,B),(3,A),(4,C),(5,A),(6,A),(7,B)] mapa == False
-- ---------------------------------------------------------------------

mapa :: Grafo Int Int
mapa = creaGrafo ND (1,7)
                 [(1,2,0),(1,3,0),(1,4,0),(2,4,0),(2,5,0),(3,4,0),
                  (3,6,0),(4,5,0),(4,6,0),(4,7,0),(5,7,0),(6,7,0)]

data Color = A | B | C | D deriving (Eq, Show)

correcta :: [(Int,Color)] -> Grafo Int Int -> Bool
correcta ncs g = 
    and [and [color x /= color y | y <- adyacentes g x] | x <- nodos g]
    where color x = head [c | (y,c) <- ncs, y == x] 

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. La expansión decimal de un número racional puede
-- representarse mediante una lista cuyo primer elemento es la parte
-- entera y el resto está formado por los dígitos de su parte decimal.
-- 
-- Definir la función
--    expansionDec :: Integer -> Integer -> [Integer]
-- tal que (expansionDec x y) es la expansión decimal de x/y. Por
-- ejemplo, 
--    take 10 (expansionDec 1 4)    ==  [0,2,5]
--    take 10 (expansionDec 1 7)    ==  [0,1,4,2,8,5,7,1,4,2]
--    take 12 (expansionDec 90 7)   ==  [12,8,5,7,1,4,2,8,5,7,1,4]
--    take 12 (expansionDec 23 14)  ==  [1,6,4,2,8,5,7,1,4,2,8,5]
-- ---------------------------------------------------------------------

expansionDec :: Integer -> Integer -> [Integer]
expansionDec x y 
    | r == 0    = [q]
    | otherwise = q : expansionDec (r*10) y
    where (q,r) = quotRem x y

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. La parte decimal de las expansiones decimales se puede
-- dividir en la parte pura y la parte periódica (que es la que se
-- repite). Por ejemplo, puesto que la expansión de 23/14 es  
--    [1,6,4,2,8,5,7,1,4,2,8,5,...
-- su parte entera es 1, su parte decimal pura es [6] y su parte decimal
-- periódica es [4,2,8,5,7,1].
-- 
-- Definir la función
--    formaDecExpDec :: [Integer] -> (Integer,[Integer],[Integer])
-- tal que (formaDecExpDec xs) es la forma decimal de la expresión
-- decimal xs; es decir, la terna formada por la parte entera, la parte
-- decimal pura y la parte decimal periódica. Por ejemplo,
--    formaDecExpDec [3,1,4]               ==  (3,[1,4],[])
--    formaDecExpDec [3,1,4,6,7,5,6,7,5]   ==  (3,[1,4],[6,7,5])
--    formaDecExpDec (expansionDec 23 14)  ==  (1,[6],[4,2,8,5,7,1])
-- ---------------------------------------------------------------------

formaDecExpDec :: [Integer] -> (Integer,[Integer],[Integer])
formaDecExpDec (x:xs) = (x,ys,zs)
    where (ys,zs) = decimales xs

-- (decimales xs) es el par formado por la parte decimal pura y la parte
-- decimal periódica de la lista de decimales xs. Por ejemplo, 
--    decimales [3,1,4]            ==  ([3,1,4],[])
--    decimales [3,1,6,7,5,6,7,5]  ==  ([3,1],[6,7,5])
decimales :: [Integer] -> ([Integer],[Integer])
decimales xs = decimales' xs []
    where decimales' [] ys = (reverse ys, [])
          decimales' (x:xs) ys 
              | x `elem` ys = splitAt k ys'
              | otherwise   = decimales' xs (x:ys)
              where ys' = reverse ys
                    k   = posicion x ys'

-- (posicion x ys) es la primera posición de x en la lista ys. Por
-- ejemplo, 
--    posicion 2 [0,2,3,2,5]  ==  1
posicion :: Eq a => a -> [a] -> Int
posicion x ys = head [n | (n,y) <- zip [0..] ys, x == y]

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Definir la función
--    formaDec :: Integer -> Integer -> (Integer,[Integer],[Integer])
-- tal que (formaDec x y) es la forma decimal de x/y; es decir, la terna
-- formada por la parte entera, la parte decimal pura y la parte decimal
-- periódica. Por ejemplo, 
--    formaDec 1 4    ==  (0,[2,5],[])
--    formaDec 23 14  ==  (1,[6],[4,2,8,5,7,1])
-- ---------------------------------------------------------------------

formaDec :: Integer -> Integer -> (Integer,[Integer],[Integer])
formaDec x y =
    formaDecExpDec (expansionDec x y)
