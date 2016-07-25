-- Informática (1º del Grado en Matemáticas)
-- Examen de la 2ª convocatoria (10 de septiembre de 2014)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1. [2 puntos] En una Olimpiada matemática de este año se
-- planteó el siguiente problema 
--    Determinar el menor entero positivo M que tiene las siguientes
--    propiedades a la vez: 
--    * El producto de los dígitos de M es 112.
--    * El producto de los dígitos de M+6 también es 112.
--
-- Definir la función
--    especiales :: Int -> Int -> [Int]
-- tal que (especiales k a) es la lista de los números naturales n tales
-- que 
--    * El producto de los dígitos de n es a.
--    * El producto de los dígitos de n+k también es a.
-- Por ejemplo, 
--    take 3 (especiales 8 24) == [38,138,226]
-- En efecto,   3*8 = 24,  38+8 = 46  y   4*6 = 24
--            2*2*6 = 24, 226+8 = 234 y 2*3*4 = 24
--
-- Usando la función especiales, calcular la solución del problema.
-- ---------------------------------------------------------------------

especiales :: Int -> Int -> [Int]
especiales k a = 
    [n | n <- [1..], product (digitos n) == a,
                     product (digitos (n+k)) == a]

digitos :: Int -> [Int]
digitos n = [read [c] | c <- show n]

-- La solución del problema es
--    ghci> head (especiales 6 112)
--    2718

-- ---------------------------------------------------------------------
-- Ejercicio 2. [2 puntos] Las expresiones aritméticas pueden
-- representarse usando el siguiente tipo de datos 
--    data Expr = N Int | S Expr Expr | P Expr Expr  
--              deriving Show
-- Por ejemplo, la expresión 2*(3+7) se representa por
--    P (N 2) (S (N 3) (N 7))
-- La dual de una expresión es la expresión obtenida intercambiando las
-- sumas y los productos. Por ejemplo, la dual de 2*(3+7) es 2+(3*7).
-- 
-- Definir la función
--    dual :: Expr -> Expr
-- tal que (dual e) es la dual de la expresión e. Por ejemplo,
--    dual (P (N 2) (S (N 3) (N 7)))  ==  S (N 2) (P (N 3) (N 7))
-- ---------------------------------------------------------------------

data Expr = N Int | S Expr Expr | P Expr Expr  
          deriving Show

dual :: Expr -> Expr
dual (N x)     = N x
dual (S e1 e2) = P (dual e1) (dual e2)
dual (P e1 e2) = S (dual e1) (dual e2)

-- ---------------------------------------------------------------------
-- Ejercicio 3. [2 puntos] La sucesión con saltos se obtiene a partir de
-- los números naturales saltando 1, cogiendo 2, saltando 3, cogiendo 4,
-- saltando 5, etc. Por ejemplo, 
--    (1), 2,3, (4,5,6), 7,8,9,10, (11,12,13,14,15), 16,17,18,19,20,21, 
-- en la que se ha puesto entre paréntesis los números que se salta; los
-- que quedan son 
--    2,3, 7,8,9,10, 16,17,18,19,20,21, ...
-- 
-- Definir la función
--    saltos :: [Integer]
-- tal que saltos es la lista de los términos de la sucesión con saltos. 
-- Por ejemplo,
--    ghci> take 22 saltos
--    [2,3, 7,8,9,10, 16,17,18,19,20,21, 29,30,31,32,33,34,35,36, 46,47]
-- ---------------------------------------------------------------------

-- 1ª solución:
saltos :: [Integer]
saltos = aux (tail (scanl (+) 0 [1..])) 
    where aux (a:b:c:ds) = [a+1..b] ++ aux (c:ds) 

-- 2ª solución:
saltos2 :: [Integer]
saltos2 = aux [1..] [1..]
    where aux (m:n:ns) xs = take n (drop m xs) ++ aux ns (drop (m+n) xs)

-- 3ª solución:
saltos3 :: [Integer]
saltos3 = aux pares [1..]
    where pares             = [(x,x+1) | x <- [1,3..]]
          aux ((m,n):ps) xs = take n (drop m xs) ++ aux ps (drop (m+n) xs)

-- 4ª solución:
saltos4 :: [Integer]
saltos4 = concat (map sig pares)
    where pares     = [(x,x+1) | x <-[1,3..]]
          sig (m,n) = take n (drop (m*(m+1) `div` 2) [1..])

-- ---------------------------------------------------------------------
-- Ejercicio 4. [2 puntos] (Basado en el problema 362 del proyecto
-- Euler). El número 54 se puede factorizar de 7 maneras distintas con
-- factores mayores que 1
--    54, 2×27, 3×18, 6×9, 3×3×6, 2×3×9 y 2×3×3×3.
-- Si exigimos que los factores sean libres de cuadrados (es decir, que
-- no se puedan dividir por ningún cuadrado), entonces sólo quedan dos
-- factorizaciones 
--    3×3×6 y 2×3×3×3.
--  
-- Definir la función 
--    factorizacionesLibresDeCuadrados :: Int -> [[Int]]
-- tal que (factorizacionesLibresDeCuadrados n) es la lista de las
-- factorizaciones de n libres de cuadrados. Por ejemplo,
--    factorizacionesLibresDeCuadrados 54  ==  [[2,3,3,3],[3,3,6]]
-- ---------------------------------------------------------------------

factorizacionesLibresDeCuadrados :: Int -> [[Int]]
factorizacionesLibresDeCuadrados n =
    [xs | xs <- factorizaciones n, listaLibreDeCuadrados xs] 

-- (factorizaciones n) es la lista creciente de números mayores que 1
-- cuyo producto es n. Por ejemplo,
--    factorizaciones 12  ==  [[2,2,3],[2,6],[3,4],[12]]
--    factorizaciones 54  ==  [[2,3,3,3],[2,3,9],[2,27],[3,3,6],[3,18],[6,9],[54]]
factorizaciones ::  Int -> [[Int]]
factorizaciones n = aux n 2
    where aux 1 _ = [[]]
          aux n a = [m:xs | m <- [a..n],
                            n `rem` m == 0,
                            xs <- aux (n `div` m) m]


-- (listaLibreDeCuadrados xs) se verifica si todos los elementos de xs
-- son libres de cuadrados. Por ejemplo,
--    listaLibreDeCuadrados [3,6,15,10]  ==  True
--    listaLibreDeCuadrados [3,6,15,20]  ==  False
listaLibreDeCuadrados :: [Int] -> Bool
listaLibreDeCuadrados = all libreDeCuadrado

-- (libreDeCuadrado n) se verifica si n es libre de cuadrado. Por
-- ejemplo, 
--    libreDeCuadrado 10  ==  True
--    libreDeCuadrado 12  ==  False
libreDeCuadrado :: Int -> Bool
libreDeCuadrado n =
    null [m | m <- [2..n], rem n (m^2) == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 5. [2 puntos] (Basado en el problema 196 del proyecto
-- Euler). Para cada número n la matriz completa de orden n es la matriz
-- cuadrada de orden n formada por los números enteros consecutivos. Por
-- ejemplo, la matriz completa de orden 3 es
--    |1 2 3|
--    |4 5 6|
--    |7 8 9|
-- las ternas primas de orden n son los listas formadas por un
-- elemento de la matriz junto con dos de sus vecinos de manera que los
-- tres son primos. Por ejemplo, en la matriz anterior una terna prima
-- es [2,3,5] (formada por el elemento 2, su vecino derecho 3 y su
-- vecino inferior 5), otra es [5,2,7] (formada por el elemento 5, su
-- vecino superior 2 y su vecino inferior-izquierda 7) y otra es [5,3,7]
-- (formada por el elemento 5, su vecino superior-derecha 3 y y su
-- vecino inferior-izquierda 7).
-- 
-- Definir la función 
--    ternasPrimasOrden :: Int -> [[Int]]
-- tal que (ternasPrimasOrden n) es el conjunto de las ternas primas de
-- la matriz completa de orden n. Por ejemplo,
--    ghci> ternasPrimasOrden 3
--    [[2,3,5],[3,2,5],[5,2,3],[5,2,7],[5,3,7]]
--    ghci> ternasPrimasOrden 4
--    [[2,3,5],[2,3,7],[2,5,7],[3,2,7],[7,2,3],[7,2,11],[7,3,11]]
-- ---------------------------------------------------------------------

import Data.Array
import Data.List

type Matriz = Array (Int,Int) Int

ternasPrimasOrden :: Int -> [[Int]]
ternasPrimasOrden = ternasPrimas . matrizCompleta

-- (ternasPrimas p) es la lista de las ternas primas de p. Por ejemplo,  
--    ghci> ternasPrimas (listArray ((1,1),(3,3)) [2,3,7,5,4,1,6,8,9])
--    [[2,3,5],[3,2,7],[3,2,5],[3,7,5],[5,2,3]]
ternasPrimas :: Matriz -> [[Int]]
ternasPrimas p = 
    [xs | xs <- ternas p, all esPrimo xs]

-- (ternas p) es la lista de las ternas de p formadas por un elemento de
-- p junto con dos vecinos. Por ejemplo,  
--    ghci>  ternas (listArray ((1,1),(3,3)) [2,3,7,5,4,0,6,8,9])
--     [[2,3,5],[2,3,4],[2,5,4],[3,2,7],[3,2,5],[3,2,4],[3,2,0],[3,7,5],
--      [3,7,4],[3,7,0],[3,5,4],[3,5,0],[3,4,0],[7,3,4],[7,3,0],[7,4,0],
--      [5,2,3],[5,2,4],[5,2,6],[5,2,8],[5,3,4],[5,3,6],[5,3,8],[5,4,6],
--      [5,4,8],[5,6,8],[4,2,3],[4,2,7],[4,2,5],[4,2,0],[4,2,6],[4,2,8],
--      [4,2,9],[4,3,7],[4,3,5],[4,3,0],[4,3,6],[4,3,8],[4,3,9],[4,7,5],
--      [4,7,0],[4,7,6],[4,7,8],[4,7,9],[4,5,0],[4,5,6],[4,5,8],[4,5,9],
--      [4,0,6],[4,0,8],[4,0,9],[4,6,8],[4,6,9],[4,8,9],[0,3,7],[0,3,4],
--      [0,3,8],[0,3,9],[0,7,4],[0,7,8],[0,7,9],[0,4,8],[0,4,9],[0,8,9],
--      [6,5,4],[6,5,8],[6,4,8],[8,5,4],[8,5,0],[8,5,6],[8,5,9],[8,4,0],
--      [8,4,6],[8,4,9],[8,0,6],[8,0,9],[8,6,9],[9,4,0],[9,4,8],[9,0,8]]
ternas :: Matriz -> [[Int]]
ternas p = 
    [[p!(i1,j1),p!(i2,j2),p!(i3,j3)] | 
     (i1,j1) <- indices p,
     ((i2,j2):ps) <- tails (vecinos (i1,j1) n),
     (i3,j3) <- ps]
    where (_,(n,_)) = bounds p

-- (vecinos (i,j) n) es la lista de las posiciones vecinas de la (i,j)
-- en una matriz cuadrada de orden n. Por ejemplo,
--    vecinos (2,3) 4  ==  [(1,2),(1,3),(1,4),(2,2),(2,4),(3,2),(3,3),(3,4)]
--    vecinos (2,4) 4  ==  [(1,3),(1,4),(2,3),(3,3),(3,4)]
--    vecinos (1,4) 4  ==  [(1,3),(2,3),(2,4)]
vecinos :: (Int,Int) -> Int -> [(Int,Int)]
vecinos (i,j) n = [(a,b) | a <- [max 1 (i-1)..min n (i+1)],
                           b <- [max 1 (j-1)..min n (j+1)],
                           (a,b) /= (i,j)]

-- (esPrimo n) se verifica si n es primo. Por ejemplo,
--    esPrimo  7  ==  True
--    esPrimo 15  ==  False
esPrimo :: Int -> Bool
esPrimo n = [x | x <- [1..n], n `rem` x == 0] == [1,n]

-- (matrizCompleta n) es la matriz completa de orden n. Por ejemplo,
--    ghci> matrizCompleta 3
--    array ((1,1),(3,3)) [((1,1),1),((1,2),2),((1,3),3),
--                         ((2,1),4),((2,2),5),((2,3),6),
--                         ((3,1),7),((3,2),8),((3,3),9)]
matrizCompleta :: Int -> Matriz
matrizCompleta n =
    listArray ((1,1),(n,n)) [1..n*n]

-- 2ª definición
-- =============

ternasPrimasOrden2 :: Int -> [[Int]]
ternasPrimasOrden2 = ternasPrimas2 . matrizCompleta

ternasPrimas2 :: Matriz -> [[Int]]
ternasPrimas2 p = 
    [[p!(i1,j1),p!(i2,j2),p!(i3,j3)] | 
     (i1,j1) <- indices p,
     esPrimo (p!(i1,j1)),
     ((i2,j2):ps) <- tails (vecinos (i1,j1) n),
     esPrimo (p!(i2,j2)),
     (i3,j3) <- ps,
     esPrimo (p!(i3,j3))]
    where (_,(n,_)) = bounds p

-- Comparación:
--    ghci> length (ternasPrimasOrden 30)
--    51
--    (5.52 secs, 211095116 bytes)
--    ghci> length (ternasPrimasOrden2 30)
--    51
--    (0.46 secs, 18091148 bytes)
