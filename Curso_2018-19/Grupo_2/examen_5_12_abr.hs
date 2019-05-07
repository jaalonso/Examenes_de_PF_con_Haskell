-- Informática (1º del Grado en Matemáticas, Grupo 2)
-- 5º examen de evaluación continua (12 de abril de 2019)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Librerías                                                          --
-- ---------------------------------------------------------------------

import Data.List
import qualified Data.Vector as V
import qualified Data.Matrix as M
import qualified Data.Array  as A
import System.Timeout
import I1M.Cola

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Una matriz equis es una matriz en la que hay una
-- posición (i,j) tal que todos los elementos que están fuera de las
-- diagonales que pasan por dicha posición son nulos. Por ejemplo,
--   ( 0 2 0 2 )      ( 2 0 0 0 1 )      ( 3 0 0 0 5 )
--   ( 0 0 4 0 )      ( 0 0 0 3 0 )      ( 0 4 0 2 0 )
--   ( 0 3 0 7 )      ( 0 0 1 0 0 )      ( 0 0 1 0 0 )
--   ( 5 0 0 0 )      ( 0 7 0 6 0 )
--
-- Definir la función
--    esMatrizEquis :: M.Matrix Int -> Bool
-- tal que (esMatrizEquis a) se verifica si la matriz a es una matriz
-- equis. Por ejemplo, dadas las matrices
--    m1 = M.matrix 3 3 (\(i,j) -> if (all odd [i,j]) then 1 else 0)
--    m2 = M.matrix 3 4 (\(i,j) -> i+j)
-- entonces
--    esMatrizEquis m1  ==  True
--    esMatrizEquis m2  ==  False
-- ---------------------------------------------------------------------

m1, m2 :: M.Matrix Int
m1 = M.matrix 3 3 (\(i,j) -> if (all odd [i,j]) then 1 else 0)
m2 = M.matrix 3 4 (\(i,j) -> i+j)

esMatrizEquis :: M.Matrix Int -> Bool
esMatrizEquis a =
  or [esMatrizEquisAux a (i,j) | i <- [1..m], j <- [1..n]]
  where m = M.nrows a
        n = M.ncols a

esMatrizEquisAux :: M.Matrix Int -> (Int,Int) -> Bool
esMatrizEquisAux a (f,c) =
  all (== 0) [a M.! (i,j) | i <- [1..M.nrows a]
                          , j <- [1..M.ncols a]
                          , abs (f-i) /= abs (c-j)]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función
--    matrizEquis :: Int -> Int -> (Int,Int) -> M.Matrix Int
-- tal que (matrizEquis m n f c) es la matriz equis de dimensión (m,n)
-- con respecto a la posición (f,c), en la que el valor de cada elemento
-- no nulo es la distancia en línea recta a la posición (f,c), contando
-- también esta última. Por ejemplo,
--    λ> matrizEquis 3 3 (2,2)
--    ( 2 0 2 )
--    ( 0 1 0 )
--    ( 2 0 2 )
--    
--    λ> matrizEquis 4 5 (2,3)
--    ( 0 2 0 2 0 )
--    ( 0 0 1 0 0 )
--    ( 0 2 0 2 0 )
--    ( 3 0 0 0 3 )
--    
--    λ> matrizEquis 5 3 (2,3)
--    ( 0 2 0 )
--    ( 0 0 1 )
--    ( 0 2 0 )
--    ( 3 0 0 )
--    ( 0 0 0 )
-- ---------------------------------------------------------------------

matrizEquis :: Int -> Int -> (Int,Int) -> M.Matrix Int
matrizEquis m n p =
  M.matrix m n (\(i,j) -> generadorMatrizEquis p i j)

generadorMatrizEquis :: (Int,Int) -> Int -> Int -> Int
generadorMatrizEquis (f,c) i j
  | abs (f-i) == abs (c-j) = 1 + abs (f-i)
  | otherwise              = 0

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Sheldon Cooper tiene que pagar 10# por el aparcamiento,
-- pero sólo tiene monedas de 1#, de 2# y de 5#. Entonces dice: "podría
-- hacer esto de 125 formas distintas y necesitaría un total de 831 
-- monedas para hacerlo de todas las formas posibles". Está contando las
-- formas de disponer las monedas en la máquina para pagar los 10# y el
-- número de monedas que necesita para hacerlas todas. Por ejemplo, si
-- tuviese que pagar 4# entonces habría 5 formas de pagar:
--    [2,2], [2,1,1], [1,2,1], [1,1,2] y [1,1,1,1]
-- y el total de monedas que se necesitan para hacerlas todas es
--    2 + 3 + 3 + 3 + 4 = 15.
--
-- Definir la función
--   distribuciones :: Integer -> Integer
-- tal que (distribuciones n) es el número de formas distintas de
-- distribuir la cantidad n como una secuencia de monedas de 1#, 2# y
-- 5#, con un valor total igual a n. Por ejemplo,
--   map distribuciones [0..10] ==  [1,1,2,3,5,9,15,26,44,75,128]
--   distribuciones 5           ==  9
--   distribuciones 10          ==  128
--   distribuciones 100         ==  91197869007632925819218
--   length (show (distribuciones 1000))  ==  232
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

distribuciones1 :: Integer -> Integer
distribuciones1 = genericLength . formas

-- (formas n) es la lista de las formas distintas de distribuir la
-- cantidad n como una secuencia de monedas de 1#, 2# y 5#, con un valor
-- total igual a n. Por ejemplo,
--    λ> formas 4
--    [[1,1,1,1],[1,1,2],[1,2,1],[2,1,1],[2,2]]
formas :: Integer -> [[Integer]]
formas n | n < 0     = []
         | n == 0    = [[]]
         | otherwise = [k:xs | k <- [1,2,5], xs <- formas (n-k)]

-- 2ª solución
-- ===========

distribuciones2 :: Integer -> Integer
distribuciones2 0 = 1
distribuciones2 1 = 1
distribuciones2 2 = 2
distribuciones2 3 = 3
distribuciones2 4 = 5
distribuciones2 n = sum [distribuciones2 (n-k) | k <- [1,2,5]]

-- 3ª solución
-- ===========

distribuciones3 :: Integer -> Integer
distribuciones3 n = v A.! n
  where v = A.array (0,n) [(i, f i) | i <- [0..n]]
        f 0 = 1
        f 1 = 1
        f 2 = 2
        f 3 = 3
        f 4 = 5
        f m = sum [v A.! (m-k) | k <- [1,2,5]]

-- Comparación de eficiencia
-- =========================

--    λ> distribuciones1 26
--    651948
--    (2.72 secs, 2,650,412,856 bytes)
--    λ> distribuciones2 26
--    651948
--    (0.30 secs, 153,096,224 bytes)
--    λ> distribuciones3 26
--    651948
--    (0.00 secs, 163,744 bytes)
--    
--    λ> distribuciones2 30
--    5508222
--    (2.46 secs, 1,292,545,008 bytes)
--    λ> distribuciones3 30
--    5508222
--    (0.00 secs, 172,048 bytes)

-- En lo que sigue se usará la 3ª definición
distribuciones :: Integer -> Integer
distribuciones = distribuciones3

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Con la definición del apartado anterior, evaluar (en
-- menos de 2 segundos), la siguiente expresión para que de un valor
-- distinto de Nothing 
--    timeout (2*10^6) (return $! (length (show (distribuciones 100000))))
-- ---------------------------------------------------------------------

-- El cálculo es
--    λ> timeout (2*10^6) (return $! (length (show (distribuciones 100000))))
--    Just 23170
--    (0.90 secs, 1,143,930,488 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    cuentaMonedas :: Integer -> Integer
-- tal que (cuentaMonedas n) es el número de monedas que le hacen falta
-- a Sheldon Cooper para construir todas las distribuciones de la
-- cantidad n como una secuencia de monedas de 1#, 2# y 5#, con un valor
-- total igual a n. Por ejemplo,
--    map cuentaMonedas1 [0..10] == [0,1,3,7,15,31,62,122,235,447,841]
--    cuentaMonedas 5            ==  31
--    cuentaMonedas 10           ==  841
--    cuentaMonedas 100          ==  5660554507743281845750870
--    length (show (cuentaMonedas 1000))  ==  235
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

cuentaMonedas1 :: Integer -> Integer
cuentaMonedas1 n = sum (map genericLength (formas n))


-- 2ª definición
-- =============

cuentaMonedas2 :: Integer -> Integer
cuentaMonedas2 0 = 0
cuentaMonedas2 1 = 1
cuentaMonedas2 2 = 3
cuentaMonedas2 3 = 7
cuentaMonedas2 4 = 15
cuentaMonedas2 n =
  cuentaMonedas2 (n-1) + cuentaMonedas2 (n-2) + cuentaMonedas2 (n-5) +
  distribuciones n

-- 3ª definición
-- =============

cuentaMonedas3 :: Integer -> Integer
cuentaMonedas3 n = v A.! n
  where v = A.array (0,n) [(i, f i) | i <- [0..n]]
        f 0 = 0
        f 1 = 1
        f 2 = 3
        f 3 = 7
        f 4 = 15
        f m = sum [v A.! (m-k) | k <- [1,2,5]] + distribuciones m
        
-- 4ª definición
-- =============

cuentaMonedas4 :: Integer -> Integer
cuentaMonedas4 n =  c A.! n
  where 
    d = A.array (0,n) [(i, f i) | i <- [0..n]]
    f 0 = 1
    f 1 = 1
    f 2 = 2
    f 3 = 3
    f 4 = 5
    f m = sum [d A.! (m-k) | k <- [1,2,5]]
    c = A.array (0,n) [(i, g i) | i <- [0..n]]
    g 0 = 0
    g 1 = 1
    g 2 = 3
    g 3 = 7
    g 4 = 15
    g m = sum [c A.! (m-k) | k <- [1,2,5]] + 
          sum [d A.! (m-k) | k <- [1,2,5]]

-- Comparación de eficiencia
-- =========================

--    λ> cuentaMonedas1 25
--    6050220
--    (3.81 secs, 1,980,897,632 bytes)
--    λ> cuentaMonedas2 25
--    6050220
--    (0.79 secs, 452,597,096 bytes)
--    λ> cuentaMonedas3 25
--    6050220
--    (0.00 secs, 596,400 bytes)
--    λ> cuentaMonedas4 25
--    6050220
--    (0.00 secs, 228,800 bytes)
--    
--    λ> cuentaMonedas2 30
--    104132981
--    (10.94 secs, 6,518,022,552 bytes)
--    λ> cuentaMonedas3 30
--    104132981
--    (0.00 secs, 821,976 bytes)
--    λ> cuentaMonedas4 30
--    104132981
--    (0.00 secs, 256,160 bytes)
--    
--    λ> length (show (cuentaMonedas3 2000))
--    467
--    (4.84 secs, 3,715,286,088 bytes)
--    λ> length (show (cuentaMonedas4 2000))
--    467
--    (0.04 secs, 11,454,248 bytes)

-- En lo que sigue usaremos la 4ª definición
cuentaMonedas :: Integer -> Integer
cuentaMonedas = cuentaMonedas4

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Con la definición del apartado anterior, evaluar (en
-- menos de 10 segundos), la siguiente expresión para que de un valor
-- distinto de Nothing 
--    timeout (10^7) (return $! (length (show (cuentaMonedas 100000))))
-- ---------------------------------------------------------------------

-- El cálculo es
--    λ> timeout (10^7) (return $! (length (show (cuentaMonedas 100000))))
--    Just 23175
--    (2.21 secs, 3,888,064,112 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    reduceCola :: (Eq a) => Cola a -> Cola a
-- tal que (reduceCola c) es la cola obtenida eliminando los elementos
-- consecutivos repetidos de la cola c. Por ejemplo,
--    λ> c = foldr inserta vacia [1,1,2,2,2,3,4,4,2,2,3,3]
--    λ> c
--    C [3,3,2,2,4,4,3,2,2,2,1,1]
--    λ> reduceCola c
--    C [3,2,4,3,2,1]
-- ---------------------------------------------------------------------

reduceCola :: Eq a => Cola a -> Cola a
reduceCola = aux vacia
  where aux r c | esVacia c  = r
                | esVacia rc = inserta pc r
                | pc == sc   = aux r rc
                | otherwise  = aux (inserta pc r) rc
          where rc = resto c
                pc = primero c
                sc = primero rc

