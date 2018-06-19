-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 5º examen de evaluación continua (3 de mayo de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import I1M.Cola
import qualified Data.Matrix as M
import Data.Array
import Test.QuickCheck
import Data.Numbers.Primes

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Un número entero n es muy primo si es n primo y todos
-- los números que resultan de ir suprimimiendo la última cifra también
-- son primos. Por ejemplo, 7193 es muy primo pues los números 7193,
-- 719, 71 y 7 son todos primos. 
-- 
-- Definir la función 
--    muyPrimo :: Int -> Bool
-- que (muyPrimo n) se verifica si n es muy primo. Por ejemplo,
--    muyPrimo 7193  == True
--    muyPrimo 71932 == False
-- ---------------------------------------------------------------------

muyPrimo :: Int -> Bool
muyPrimo n | n < 10    = isPrime n
           | otherwise = isPrime n && muyPrimo (n `div` 10)

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. ¿Cuántos números de cinco cifras son muy primos?
-- ---------------------------------------------------------------------

-- El cálculo es
--    λ> length (filter muyPrimo [10^4..99999])
--    15

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la función
--    muyPrimosF :: FilePath -> FilePath -> IO ()
-- tal que al evaluar (muyPrimosF f1 f2) lee el contenido del fichero
-- f1 (que estará compuesto por números, cada uno en una línea) y
-- escribe en el fichero f2 los números de f1 que son muy primos, cada
-- uno en una línea. Por ejemplo, si el contenido de ej.txt es 
--     7193
--     1870
--     271891
--     23993
--     1013
-- y evaluamos (muyPrimosF "ej.txt" "sol.txt"), entonces el contenido
-- del fichero "sol.txt" será
--     7193
--     23993
-- -------------------------------------------------------------------

muyPrimosF :: FilePath -> FilePath -> IO ()
muyPrimosF f1 f2 = do
  cs <- readFile f1
  writeFile f2 (( unlines
                . map show
                . filter muyPrimo
                . map read
                . lines )
                cs)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Se define la relación de orden entre las colas como el
-- orden lexicográfico. Es decir, la cola c1 es "menor" que c2 si el
-- primer elemento de c1 es menor que el primero de c2, o si son
-- iguales, el resto de la cola c1 es "menor" que el resto de la cola
-- c2. 
-- 
-- Definir la función
--    menorCola :: Ord a => Cola a -> Cola a -> Bool
-- tal que (menorCola c1 c2) se verifica si c1 es "menor" que c2. Por
-- ejemplo, para las colas 
--    c1 = foldr inserta vacia [1..20]
--    c2 = foldr inserta vacia [1..5]
--    c3 = foldr inserta vacia [3..10]
--    c4 = foldr inserta vacia [4,-1,7,3,8,10,0,3,3,4]
-- se verifica que
--    menorCola c1 c2    == False
--    menorCola c2 c1    == True
--    menorCola c4 c3    == True
--    menorCola vacia c1 == True
--    menorCola c1 vacia == False
-- ---------------------------------------------------------------------

menorCola :: Ord a => Cola a -> Cola a -> Bool
menorCola c1 c2 | esVacia c1 = True
                | esVacia c2 = False
                | a1 < a2    = True
                | a1 > a2    = False
                | otherwise  = menorCola r1 r2
  where a1 = primero c1
        a2 = primero c2
        r1 = resto c1
        r2 = resto c2

c1, c2, c3, c4 :: Cola Int 
c1 = foldr inserta vacia [1..20]
c2 = foldr inserta vacia [1..5]
c3 = foldr inserta vacia [3..10]
c4 = foldr inserta vacia [4,-1,7,3,8,10,0,3,3,4]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Una triangulación de un polígono es una división del
-- área en un conjunto de triángulos, de forma que la unión de todos
-- ellos es igual al polígono original, y cualquier par de triángulos es
-- disjunto o comparte únicamente un vértice o un lado. En el caso de
-- polígonos convexos, la cantidad de triangulaciones posibles depende
-- únicamente del número de vértices del polígono.
-- 
-- Si llamamos T(n) al número de triangulaciones de un polígono de n
-- vértices, se verifica la siguiente relación de recurrencia:
--     T(2) = 1
--     T(n) = T(2)*T(n-1) + T(3)*T(n-2) + ... + T(n-1)*T(2)
-- 
-- Definir la función 
--    numeroTriangulaciones :: Integer -> Integer
-- tal que (numeroTriangulaciones n) es el número de triangulaciones de
-- un polígono convexo de n vértices. Por ejemplo,
--    numeroTriangulaciones 3  == 1
--    numeroTriangulaciones 5  == 5
--    numeroTriangulaciones 6  == 14
--    numeroTriangulaciones 7  == 42
--    numeroTriangulaciones 50 == 131327898242169365477991900
--    numeroTriangulaciones 100
--      ==  57743358069601357782187700608042856334020731624756611000
-- ---------------------------------------------------------------------

-- 1ª solución (por recursión)
-- ===========================

numeroTriangulaciones :: Integer -> Integer
numeroTriangulaciones 2 = 1
numeroTriangulaciones n = sum (zipWith (*) ts (reverse ts))
  where ts = [numeroTriangulaciones k | k <- [2..n-1]]

-- 2ª solución
-- ===========

numeroTriangulaciones2 :: Integer -> Integer
numeroTriangulaciones2 n = 
  head (sucNumeroTriangulacionesInversas `genericIndex` (n-2))

--    λ> mapM_ print (take 10 sucNumeroTriangulacionesInversas)
--    [1]
--    [1,1]
--    [2,1,1]
--    [5,2,1,1]
--    [14,5,2,1,1]
--    [42,14,5,2,1,1]
--    [132,42,14,5,2,1,1]
--    [429,132,42,14,5,2,1,1]
--    [1430,429,132,42,14,5,2,1,1]
--    [4862,1430,429,132,42,14,5,2,1,1]
sucNumeroTriangulacionesInversas :: [[Integer]]        
sucNumeroTriangulacionesInversas = iterate f [1]
  where f ts = sum (zipWith (*) ts (reverse ts)) : ts

-- 3ª solución (con programación dinámica)
-- =======================================

numeroTriangulaciones3 :: Integer -> Integer
numeroTriangulaciones3 n = vectorTriang n ! n

--    λ> vectorTriang 9
--    array (2,9) [(2,1),(3,1),(4,2),(5,5),(6,14),(7,42),(8,132),(9,429)]
vectorTriang :: Integer -> Array Integer Integer
vectorTriang n = v
  where v = array (2,n) [(i, f i) | i <-[2..n]]
        f 2 = 1
        f i = sum [v!j*v!(i-j+1) | j <-[2..i-1]]

-- Comparación de eficiencia
-- =========================

--    λ> numeroTriangulaciones 22
--    6564120420
--    (3.97 secs, 668,070,936 bytes)
--    λ> numeroTriangulaciones2 22
--    6564120420
--    (0.01 secs, 180,064 bytes)
--    λ> numeroTriangulaciones3 22
--    6564120420
--    (0.01 secs, 285,792 bytes)
--    
--    λ> length (show (numeroTriangulaciones2 800))
--    476
--    (0.59 secs, 125,026,824 bytes)
--    λ> length (show (numeroTriangulaciones3 800))
--    476
--    (1.95 secs, 334,652,936 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con QuickCheck que se verifica la siguiente
-- propiedad: el número de triangulaciones posibles de un polígono
-- convexo de n vértices es igual al (n-2)-simo número de Catalan, donde
--    
--              (2n)!
--    C(n)  =  ----------
--            (n+1)! n!
-- ---------------------------------------------------------------------

propNumeroTriangulaciones :: Integer -> Property
propNumeroTriangulaciones n = 
  n > 1 ==> numeroTriangulaciones2 n == numeroCatalan (n-2) 

numeroCatalan :: Integer -> Integer
numeroCatalan n = factorial (2*n) `div` (factorial (n+1) * factorial n)

factorial :: Integer -> Integer
factorial n = product [1..n]

-- La comprobación es
--    λ> quickCheck propNumeroTriangulaciones
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4. En el siguiente gráfico se representa en una cuadrícula
-- el plano de Manhattan. Cada línea es una opción a seguir; el número
-- representa las atracciones que se pueden visitar si se elige esa
-- opción.
-- 
--          3         2         4         0
--     * ------- * ------- * ------- * ------- *
--     |         |         |         |         |
--     |1        |0        |2        |4        |3
--     |    3    |    2    |    4    |    2    |
--     * ------- * ------- * ------- * ------- *
--     |         |         |         |         |
--     |4        |6        |5        |2        |1
--     |    0    |    7    |    3    |    4    |
--     * ------- * ------- * ------- * ------- *
--     |         |         |         |         |
--     |4        |4        |5        |2        |1
--     |    3    |    3    |    0    |    2    |
--     * ------- * ------- * ------- * ------- *
--     |         |         |         |         |
--     |5        |6        |8        |5        |3
--     |    1    |    3    |    2    |    2    |
--     * ------- * ------- * ------- * ------- *
-- 
-- El turista entra por el extremo superior izquierda y sale por el
-- extremo inferior derecha. Sólo puede moverse en las direcciones Sur y
-- Este (es decir, hacia abajo o hacia la derecha).
-- 
-- Representamos el mapa mediante una matriz p tal que p(i,j) = (a,b),
-- donde a = nº de atracciones si se va hacia el sur y b = nº de
-- atracciones si se va al este. Además, ponemos un 0 en el valor del
-- número de atracciones por un camino que no se puede elegir. De esta
-- forma, el mapa anterior se representa por la matriz siguiente:
--
--    ( (1,3)   (0,2)   (2,4)   (4,0)  (3,0) )
--    ( (4,3)   (6,2)   (5,4)   (2,2)  (1,0) )
--    ( (4,0)   (4,7)   (5,3)   (2,4)  (1,0) )
--    ( (5,3)   (6,3)   (8,0)   (5,2)  (3,0) )
--    ( (0,1)   (0,3)   (0,2)   (0,2)  (0,0) )
-- 
-- En este caso, si se hace el recorrido 
--    [S, E, S, E, S, S, E, E],
-- el número de atracciones es
--     1  3  6  7  5  8  2  2
-- cuya suma es 34.
-- 
-- Definir la función
--    mayorNumeroV:: M.Matrix (Int,Int) -> Int
-- tal que (mayorNumeroV p) es el máximo número de atracciones que se
-- pueden visitar en el plano representado por la matriz p. Por ejemplo,
-- si se define la matriz anterior por
--    ej1b :: M.Matrix (Int,Int)
--    ej1b = M.fromLists  [[(1,3),(0,2),(2,4),(4,0),(3,0)], 
--                         [(4,3),(6,2),(5,4),(2,2),(1,0)],
--                         [(4,0),(4,7),(5,3),(2,4),(1,0)],
--                         [(5,3),(6,3),(8,0),(5,2),(3,0)],
--                         [(0,1),(0,3),(0,2),(0,2),(0,0)]]
-- entonces
--    mayorNumeroV ej1b == 34
--    mayorNumeroV (M.fromLists [[(1,3),(0,0)],
--                               [(0,3),(0,0)]]) == 4
--    mayorNumeroV (M.fromLists [[(1,3),(0,2),(2,0)],
--                               [(4,3),(6,2),(5,0)],
--                               [(0,0),(0,7),(0,0)]]) == 17
-- ---------------------------------------------------------------------

ej1b :: M.Matrix (Int,Int)
ej1b = M.fromLists  [[(1,3),(0,2),(2,4),(4,0),(3,0)], 
                     [(4,3),(6,2),(5,4),(2,2),(1,0)],
                     [(4,0),(4,7),(5,3),(2,4),(1,0)],
                     [(5,3),(6,3),(8,0),(5,2),(3,0)],
                     [(0,1),(0,3),(0,2),(0,2),(0,0)]]


-- 1ª definición (por recursión)
-- =============================

mayorNumeroV1 :: M.Matrix (Int,Int) -> Int
mayorNumeroV1 p = aux m n
  where m = M.nrows p
        n = M.ncols p
        aux 1 1 = 0
        aux 1 j = sum [snd (p M.!(1,k)) | k <-[1..j-1]]
        aux i 1 = sum [fst (p M.!(k,1)) | k <-[1..i-1]]
        aux i j = max (aux (i-1) j + fst (p M.!(i-1,j)))
                      (aux i (j-1) + snd (p M.!(i,j-1)))

-- 2ª solución (con programación dinámica)
-- =======================================

mayorNumeroV :: M.Matrix (Int,Int) -> Int
mayorNumeroV p = matrizNumeroV p M.! (m,n)
  where m = M.nrows p
        n = M.ncols p

matrizNumeroV :: M.Matrix (Int,Int) -> M.Matrix Int
matrizNumeroV p = q
  where m = M.nrows p
        n = M.ncols p
        q = M.matrix m n f
          where f (1,1) = 0
                f (1,j) = sum [snd (p M.!(1,k)) | k <-[1..j-1]]
                f (i,1) = sum [fst (p M.!(k,1)) | k <-[1..i-1]]
                f (i,j) = max (q M.!(i-1,j) + fst (p M.!(i-1,j)))
                              (q M.!(i,j-1) + snd (p M.!(i,j-1)))

-- Comparación de eficiencia
-- =========================

--    λ> mayorNumeroVR (M.fromList 12 12 [(n,n+1) | n <- [1..]])
--    2200
--    (7.94 secs, 1,348,007,672 bytes)
--    λ> mayorNumeroV (M.fromList 12 12 [(n,n+1) | n <- [1..]])
--    2200
--    (0.01 secs, 348,336 bytes)
