-- Informática (1º del Grado en Matemáticas)
-- 7º examen de evaluación continua (29 de junio de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Librerías auxiliares
-- ---------------------------------------------------------------------

import Data.Array
import Data.List 
import Data.Numbers.Primes 
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    segmentos :: (Enum a, Eq a) => [a] -> [[a]]
-- tal que (segmentos xss) es la lista de los segmentos maximales de xss
-- formados por elementos consecutivos. Por ejemplo,
--    segmentos [1,2,5,6,4]     ==  [[1,2],[5,6],[4]]
--    segmentos [1,2,3,4,7,8,9] ==  [[1,2,3,4],[7,8,9]]
--    segmentos "abbccddeeebc"  ==  ["ab","bc","cd","de","e","e","bc"]
-- 
-- Nota: Se puede usar la función succ tal que (succ x) es el sucesor de
-- x. Por ejemplo,
--    succ 3    ==  4
--    succ 'c'  ==  'd'
-- 
-- Comprobar con QuickCheck que para todo segmento maximal ys de una
-- lista se verifica que la diferencia entre el último y el primer
-- elemento de ys es igual a la longitud de ys menos 1. 
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión)
-- =============================

segmentos1 :: (Enum a, Eq a) => [a] -> [[a]]
segmentos1 []  = []
segmentos1 [x] = [[x]]
segmentos1 (x:xs)
  | y == succ x = (x:y:ys):zs
  | otherwise   = [x] : (y:ys):zs
  where ((y:ys):zs) = segmentos1 xs
 
-- 2ª definición
segmentos2 :: (Enum a, Eq a) => [a] -> [[a]]
segmentos2 []  = []
segmentos2 xs = ys : segmentos2 zs
  where ys = inicial xs
        n  = length ys
        zs = drop n xs
 
-- (inicial xs) es el segmento inicial de xs formado por elementos
-- consecutivos. Por ejemplo,
--    inicial [1,2,5,6,4]    ==  [1,2]
--    inicial "abccddeeebc"  ==  "abc"
inicial :: (Enum a, Eq a) => [a] -> [a]
inicial [] = []
inicial (x:xs) = 
  [y | (y,z) <- takeWhile (\(u,v) -> u == v) (zip (x:xs) [x..])]

-- Comparación de eficiencia
-- =========================

--    ghci> length (segmentos1 (show (5^(10^6))))
--    636114
--    (2.05 secs, 842,837,680 bytes)
--    ghci> length (segmentos2 (show (5^(10^6))))
--    636114
--    (1.21 secs, 833,432,080 bytes)

-- La propiedad es
prop_segmentos :: [Int] ->  Bool
prop_segmentos xs =
  all (\ys -> last ys - head ys == length ys - 1) (segmentos2 xs)

-- La comprobación es 
--    ghci> quickCheck prop_segmentos
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2. Un número de la suerte es un número natural que se
-- genera por una criba, similar a la criba de Eratóstenes, como se
-- indica a continuación: 
-- 
-- Se comienza con la lista de los números enteros a partir de 1:
--    1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25...
-- Se eliminan los números de dos en dos
--    1,  3,  5,  7,  9,   11,   13,   15,   17,   19,   21,   23,   25...
-- Como el segundo número que ha quedado es 3, se eliminan los números
-- restantes de tres en tres:  
--    1,  3,      7,  9,         13,   15,         19,   21,         25...
-- Como el tercer número que ha quedado es 7, se eliminan los números
-- restantes de siete en siete:   
--    1,  3,      7,  9,         13,   15,               21,         25...
-- 
-- Este procedimiento se repite indefinidamente y los supervivientes son
-- los números de la suerte:  
--    1,3,7,9,13,15,21,25,31,33,37,43,49,51,63,67,69,73,75,79
--
-- Definir la sucesión
--    numerosDeLaSuerte :: [Int]
-- cuyos elementos son los números de la suerte. Por ejemplo,
--    ghci> take 20 numerosDeLaSuerte
--    [1,3,7,9,13,15,21,25,31,33,37,43,49,51,63,67,69,73,75,79]
--    ghci> numerosDeLaSuerte !! 1500
--    13995
-- ---------------------------------------------------------------------

-- 1ª definición
numerosDeLaSuerte :: [Int]
numerosDeLaSuerte = criba 3 [1,3..]
  where
    criba i (n:s:xs) =
      n : criba (i + 1) (s : [x | (n, x) <- zip [i..] xs
                                , rem n s /= 0])

-- 2ª definición
numerosDeLaSuerte2 :: [Int]
numerosDeLaSuerte2 =  1 : criba 2 [1, 3..]
  where criba k xs = z : criba (k + 1) (aux xs)
          where z = xs !! (k - 1 )
                aux ws = us ++ aux vs
                  where (us, _:vs) = splitAt (z - 1) ws 

-- Comparación de eficiencia
--    ghci> numerosDeLaSuerte2 !! 300
--    2217
--    (3.45 secs, 2,873,137,632 bytes)
--    ghci> numerosDeLaSuerte !! 300
--    2217
--    (0.04 secs, 22,416,784 bytes)

-- ---------------------------------------------------------------------
-- Los árboles se pueden representar mediante el siguiente tipo de datos
--    data Arbol a = N a [Arbol a]
--                   deriving Show
-- Por ejemplo, los árboles
-- 
--      1         1             1          
--     / \       / \           / \   
--    2   3     2   3         2   3  
--        |        /|\       /|\  |   
--        4       4 5 6     4 5 6 7    
--                   
-- se representan por
--    ej1, ej2, ej3 :: Arbol Int
--    ej1 = N 1 [N 2 [],N 3 [N 4 []]]
--    ej2 = N 1 [N 2 [], N 3 [N 4 [], N 5 [], N 6 []]
--    ej3 = N 1 [N 2 [N 4 [], N 5 [], N 6 []], N 3 [N 7 []]] 
--
-- En el primer ejemplo la máxima ramificación es 2 (en el nodo 1 que
-- tiene 2 hijos), la del segundo es 3 (en el nodo 3 que tiene 3
-- hijos) y la del tercero es 3 (en el nodo 3 que tiene 3 hijos).
-- 
-- Definir la función
--    maximaRamificacion :: Arbol a -> Int
-- tal que (maximaRamificacion a) es la máxima ramificación del árbol
-- a. Por ejemplo,
--    maximaRamificacion ej1  ==  2
--    maximaRamificacion ej2  ==  3
--    maximaRamificacion ej3  ==  3
-- ---------------------------------------------------------------------

data Arbol a = N a [Arbol a]
  deriving Show

ej1, ej2 :: Arbol Int
ej1 = N 1 [N 2 [],N 3 [N 4 []]]
ej2 = N 1 [N 2 [], N 3 [N 4 [], N 5 [], N 6 []]]
ej3 = N 1 [N 2 [N 4 [], N 5 [], N 6 []], N 3 [N 7 []]] 

maximaRamificacion :: Arbol a -> Int
maximaRamificacion (N _ []) = 0
maximaRamificacion (N x xs) =
  max (length xs) (maximum (map maximaRamificacion xs))

-- ---------------------------------------------------------------------
-- Ejercicio 4. Un mapa con dos tipos de regiones (por ejemplo, tierra y
-- mar) se puede representar mediante una matriz de ceros y unos.
--
-- Para los ejemplos usaremos los mapas definidos por
--    type Punto = (Int,Int) 
--    type Mapa  = Array Punto Int
--    
--    mapa1, mapa2 :: Mapa
--    mapa1 = listArray ((1,1),(3,4))
--                      [1,1,0,0,
--                       0,1,0,0,
--                       1,0,0,0]
--    mapa2 = listArray ((1,1),(10,20))
--                      [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
--                       1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,
--                       1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
--                       1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
--                       1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,
--                       0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
--                       0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
--                       1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,
--                       1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
--                       1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
--
-- Definir las funciones
--    alcanzables :: Mapa -> Punto -> [Punto]
--    esAlcanzable :: Mapa -> Punto -> Punto -> Bool
-- tales que
-- + (alcanzables p) es la lista de los puntos de mapa m que se pueden
--   alcanzar a partir del punto p moviéndose en la misma región que p
--   (es decir, a través de ceros si el elemento de m en p es un cero o a
--   través de unos, en caso contrario) y los movimientos permitidos son
--   ir hacia el norte, sur este u oeste (pero no en diagonal). Por
--   ejemplo, 
--      alcanzables mapa1 (1,1)  ==  [(2,2),(1,2),(1,1)]
--      alcanzables mapa1 (1,2)  ==  [(2,2),(1,1),(1,2)]
--      alcanzables mapa1 (1,3)  ==  [(3,2),(3,4),(3,3),(2,3),(2,4),(1,4),(1,3)]
--      alcanzables mapa1 (3,1)  ==  [(3,1)]
-- + (esAlcanzable m p1 p2) se verifica si el punto p1 es alcanzable
--   desde el p1 en el mapa m. Por ejemplo,  
--      esAlcanzable mapa1 (1,4) (3,2)    ==  True
--      esAlcanzable mapa1 (1,4) (3,1)    ==  False
--      esAlcanzable mapa2 (2,3) (8,16)   ==  True
--      esAlcanzable mapa2 (8,1) (7,3)    ==  True
--      esAlcanzable mapa2 (1,1) (10,20)  ==  False
-- ---------------------------------------------------------------------

type Punto = (Int,Int) 
type Mapa  = Array Punto Int

mapa1, mapa2 :: Mapa
mapa1 = listArray ((1,1),(3,4))
                  [1,1,0,0,
                   0,1,0,0,
                   1,0,0,0]
mapa2 = listArray ((1,1),(10,20))
                  [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                   1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,
                   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
                   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,
                   1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,
                   0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                   0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                   1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,
                   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]

alcanzables :: Mapa -> Punto -> [Punto]
alcanzables mapa p = aux [p] []
  where region = mapa ! p
        (_,(m,n)) = bounds mapa
        vecinos (i,j) = [(a,b) | (a,b) <- [(i,j+1),(i,j-1),(i+1,j),(i-1,j)]
                               , 1 <= a && a <= m
                               , 1 <= b && b <= n  
                               , mapa ! (a,b) == region]
        aux [] ys = ys
        aux (x:xs) ys
          | x `elem` ys = aux xs ys
          | otherwise   = aux (vecinos x ++ xs) (x:ys)    

esAlcanzable :: Mapa -> Punto -> Punto -> Bool
esAlcanzable m p1 p2 =
  p2 `elem` alcanzables m p1
