-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 5º examen de evaluación continua (3 de mayo de 2019)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Numbers.Primes (primeFactors)
import Data.List
import Data.Array 
import I1M.Cola
import I1M.Grafo
import qualified Data.Matrix as M
import Data.Maybe
import System.Timeout

-- ---------------------------------------------------------------------
-- Ejercicio 1. Una partición buena de una lista de enteros positivos xs
-- es un par de listas (ys,zs) tal que son ys y zs disjuntas, la
-- unión de ys y zs es xs, y el máximo común divisor de los elementos de
-- ys coincide con el máximo común divisor de los elementos de zs.

-- Definir la función
--    particionesBuenas :: [Int] -> [([Int],[Int])]
-- tal que (particionesBuenas xs) es la lista de las particiones buenas
-- de xs. Por ejemplo, 
--    particionesBuenas [1..10]              == [([1],[2,3,4,5,6,7,8,9,10])]
--    particionesBuenas [3,5..20]            == []
--    particionesBuenas [12,34,10,1020,2040] == [([12,34,10],[1020,2040])]
-- ---------------------------------------------------------------------

particionesBuenas :: [Int] -> [([Int],[Int])]
particionesBuenas xs = filter buena (particiones xs)
  where buena (as,bs) = foldr1 lcm as == foldr1 gcd bs

-- (particiones xs) es la lista de las particiones de xs en dos listas no
-- vacías. Por ejemplo, 
--    particiones "bcd"   ==  [("b","cd"),("bc","d")]
--    particiones "abcd"  ==  [("a","bcd"),("ab","cd"),("abc","d")]
particiones :: [a] -> [([a],[a])]
particiones []     = []
particiones [_]    = []
particiones (x:xs) = ([x],xs) : [(x:is,ds) | (is,ds) <- particiones xs]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Sea S un conjunto finito de números naturales y m un
-- número natural. El problema consiste en determinar si existe un
-- subconjunto de S cuya suma es m. Por ejemplo, si S = [3,34,4,12,5,2]
-- y m = 9, existe un subconjunto de S, [4,5], cuya suma es 9. En
-- cambio, no hay ningún subconjunto de S que sume 13. 

-- Definir una función
--    existeSubSuma :: [Int] -> Int -> Bool
-- tal que (existeSubSuma xs m) compruebe se existe algún subconjunto de
-- xs que sume m. Por ejemplo,
--    existeSubSuma [3,34,4,12,5,2] 9                == True
--    existeSubSuma [3,34,4,12,5,2] 13               == False
--    existeSubSuma ([3,34,4,12,5,2]++[20..400]) 13  == False
--    existeSubSuma ([3,34,4,12,5,2]++[20..400]) 654 == True
-- ---------------------------------------------------------------------

-- 1ª definición (Calculando todos los subconjuntos)
-- =================================================
 
existeSubSuma1 :: [Int] -> Int -> Bool
existeSubSuma1 xs n =
  any (\ys -> sum ys == n) (subsequences xs)
 
-- 2ª definición (por recursión)
-- =============================
 
existeSubSuma2 :: [Int] -> Int -> Bool
existeSubSuma2 _  0 = True
existeSubSuma2 [] _ = False
existeSubSuma2 (x:xs) n
  | n < x     = existeSubSuma2 xs n
  | otherwise = existeSubSuma2 xs (n-x) || existeSubSuma2 xs n 
  
-- 3ª definición (por programación dinámica)
-- =========================================
 
existeSubSuma3 :: [Int] -> Int -> Bool
existeSubSuma3 xs n =
  matrizExisteSubSuma3 xs n ! (length xs,n) 
 
-- (matrizExisteSubSuma3 xs m) es la matriz q tal que q(i,j) se verifica
-- si existe algún subconjunto de (take i xs) que sume j. Por ejemplo,
--    λ> elems (matrizExisteSubSuma3 [1,3,5] 9)
--    [True,False,False,False,False,False,False,False,False,False,
--     True,True, False,False,False,False,False,False,False,False,
--     True,True, False,True, True, False,False,False,False,False,
--     True,True, False,True, True, True, True, False,True, True]
-- Con las cabeceras,
--            0     1     2     3     4     5     6     7     8     9
--    []     [True,False,False,False,False,False,False,False,False,False,
--    [1]     True,True, False,False,False,False,False,False,False,False,
--    [1,3]   True,True, False,True, True, False,False,False,False,False,
--    [1,3,5] True,True, False,True, True, True, True, False,True, True]
matrizExisteSubSuma3 :: [Int] -> Int -> Array (Int,Int) Bool
matrizExisteSubSuma3 xs n = q
  where m = length xs
        v = listArray (1,m) xs
        q = array ((0,0),(m,n)) [((i,j), f i j) | i <- [0..m],
                                                  j <- [0..n]]
        f _ 0 = True
        f 0 _ = False
        f i j | j < v ! i = q ! (i-1,j)
              | otherwise = q ! (i-1,j-v!i) || q ! (i-1,j)
 
-- 4ª definición (ordenando y por recursión)
-- =========================================
 
existeSubSuma4 :: [Int] -> Int -> Bool
existeSubSuma4 xs = aux (sort xs)
  where aux _  0 = True
        aux [] _ = False
        aux (y:ys) n
          | n < y     = False
          | otherwise = aux ys (n-y) || aux ys n
 
-- 5ª definición (ordenando y dinámica)
-- ====================================
 
existeSubSuma5 :: [Int] -> Int -> Bool
existeSubSuma5 xs n =
  matrizExisteSubSuma5 (reverse (sort xs)) n ! (length xs,n) 
 
matrizExisteSubSuma5 :: [Int] -> Int -> Array (Int,Int) Bool
matrizExisteSubSuma5 xs n = q
  where m = length xs
        v = listArray (1,m) xs
        q = array ((0,0),(m,n)) [((i,j), f i j) | i <- [0..m],
                                                  j <- [0..n]]
        f _ 0 = True
        f 0 _ = False
        f i j | v ! i <= j = q ! (i-1,j-v!i) || q ! (i-1,j) 
              | otherwise  = False
 
-- Comparación de eficiencia:
-- ==========================

--    λ> let xs = [1..22] in existeSubSuma1 xs (sum xs)
--    True
--    (7.76 secs, 3,892,403,928 bytes)
--    λ> let xs = [1..22] in existeSubSuma2 xs (sum xs)
--    True
--    (0.02 secs, 95,968 bytes)
--    λ> let xs = [1..22] in existeSubSuma3 xs (sum xs)
--    True
--    (0.03 secs, 6,055,200 bytes)
--    λ> let xs = [1..22] in existeSubSuma4 xs (sum xs)
--    True
--    (0.01 secs, 98,880 bytes)
--    λ> let xs = [1..22] in existeSubSuma5 xs (sum xs)
--    True
--    (0.02 secs, 2,827,560 bytes)
  
--    λ> let xs = [1..200] in existeSubSuma2 xs (sum xs)
--    True
--    (0.01 secs, 182,280 bytes)
--    λ> let xs = [1..200] in existeSubSuma3 xs (sum xs)
--    True
--    (8.89 secs, 1,875,071,968 bytes)
--    λ> let xs = [1..200] in existeSubSuma4 xs (sum xs)
--    True
--    (0.02 secs, 217,128 bytes)
--    λ> let xs = [1..200] in existeSubSuma5 xs (sum xs)
--    True
--    (8.66 secs, 1,875,087,976 bytes)
--
--    λ> and [existeSubSuma2 [1..20] n | n <- [1..sum [1..20]]]
--    True
--    (2.82 secs, 323,372,512 bytes)
--    λ> and [existeSubSuma3 [1..20] n | n <- [1..sum [1..20]]]
--    True
--    (0.65 secs, 221,806,376 bytes)
--    λ> and [existeSubSuma4 [1..20] n | n <- [1..sum [1..20]]]
--    True
--    (4.12 secs, 535,153,152 bytes)
--    λ> and [existeSubSuma5 [1..20] n | n <- [1..sum [1..20]]]
--    True
--    (0.73 secs, 238,579,696 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Ulises, en sus ratos libres, juega a un pasatiempo que
-- consiste en, dada una serie de números naturales positivos en una
-- cola, sacar un elemento y, si es distinto de 1, volver a meter el
-- mayor de sus divisores propios. Si el número que saca es el 1,
-- entonces lo deja fuera y no mete ningún otro. El pasatiempo continúa
-- hasta que la cola queda vacía.   
-- 
-- Por ejemplo, a partir de una cola con los números 10, 20 y 30, el
-- pasatiempo se desarrollaría como sigue:
--   C [30,20,10]
--   C [20,10,15]
--   C [10,15,10]
--   C [15,10,5]
--   C [10,5,5]
--   C [5,5,5]
--   C [5,5,1]
--   C [5,1,1]
--   C [1,1,1]
--   C [1,1]
--   C [1]
--   C []
--
-- Definir la función
--    numeroPasos :: Cola Int -> Int
-- tal que (numeroPasos c) es el número de veces que Ulises saca algún
-- número de la cola c al utilizarla en su pasatiempo. Por ejemplo,
--    numeroPasos (foldr inserta vacia [30])        ==  4
--    numeroPasos (foldr inserta vacia [20])        ==  4
--    numeroPasos (foldr inserta vacia [10])        ==  3
--    numeroPasos (foldr inserta vacia [10,20,30])  ==  11
-- ---------------------------------------------------------------------

numeroPasos :: Cola Int -> Int
numeroPasos c
  | esVacia c = 0
  | pc == 1   = 1 + numeroPasos rc
  | otherwise = 1 + numeroPasos (inserta (mayorDivisorPropio pc) rc)
  where pc = primero c
        rc = resto c

-- (mayorDivisorPropio n) es el mayor divisor propio de n. Por ejemplo,
--    mayorDivisorPropio 30  ==  15

-- 1ª definición de mayorDivisorPropio
mayorDivisorPropio1 :: Int -> Int
mayorDivisorPropio1 n = 
  head [x | x <- [n-1,n-2..], n `mod` x == 0]

-- 2ª definición de mayorDivisorPropio
mayorDivisorPropio :: Int -> Int
mayorDivisorPropio n =
  n `div` head (primeFactors n)

-- Comparación de eficiencia:
--    ghci> sum (map mayorDivisorPropio1 [2..3000])
--    1485659
--    (3.91 secs, 618,271,360 bytes)
--    ghci> sum (map mayorDivisorPropio [2..3000])
--    1485659
--    (0.04 secs, 22,726,600 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 4. El complementario del grafo G es un grafo G' del mismo
-- tipo que G (dirigido o no dirigido), con el mismo conjunto de nodos y
-- tal que dos nodos de G' son adyacentes si y sólo si no son adyacentes
-- en G. Los pesos de todas las aristas del complementario es igual a 0.
--
-- Definir la función
--    complementario :: Grafo Int Int -> Grafo Int Int
-- tal que (complementario g) es el complementario de g. Por ejemplo,
--    λ> complementario (creaGrafo D (1,3) [(1,3,0),(3,2,0),(2,2,0),(2,1,0)])
--    G D (array (1,3) [(1,[(1,0),(2,0)]),(2,[(3,0)]),(3,[(1,0),(3,0)])])
--    λ> complementario (creaGrafo D (1,3) [(3,2,0),(2,2,0),(2,1,0)])
--    G D (array (1,3) [(1,[(1,0),(2,0),(3,0)]),(2,[(3,0)]),(3,[(1,0),(3,0)])])
-- ---------------------------------------------------------------------

complementario :: Grafo Int Int -> Grafo Int Int
complementario g = 
  creaGrafo d (1,n) [(x,y,0) | x <- xs, y <- xs, not (aristaEn g (x,y))]
  where d  = if dirigido g then D else ND
        xs = nodos g
        n  = length xs
