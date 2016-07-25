-- Informática (1º del Grado en Matemáticas)
-- Examen de la 3º convocatoria (4 de diciembre de 2015)
-- ---------------------------------------------------------------------

-- Puntuación: Cada uno de los 5 ejercicios vale 2 puntos.

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Matrix
import Data.Numbers.Primes
import I1M.BusquedaEnEspaciosDeEstados
import I1M.Grafo
import I1M.PolOperaciones

-- ---------------------------------------------------------------------
-- Ejercicio 1. Un número tiene factorización capicúa si puede escribir
-- como un producto de números primos tal que la concatenación de sus
-- dígitos forma un número capicúa. Por ejemplo, el 2015 tiene
-- factorización capicúa ya que 2015 = 13*5*31, los factores son primos
-- y su concatenación es 13531 que es capicúa.
-- 
-- Definir la sucesión
--    conFactorizacionesCapicuas :: [Int]
-- formada por los números que tienen factorización capicúa. Por
-- ejemplo, 
--    ghci> take 20 conFactorizacionesCapicuas
--    [1,2,3,4,5,7,8,9,11,12,16,18,20,25,27,28,32,36,39,44]
--
-- Usando conFactorizacionesCapicuas calcular cuál será el siguiente año
-- con factorización capicúa.
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

conFactorizacionesCapicuas :: [Int]
conFactorizacionesCapicuas =
    [n | n <- [1..], not (null (factorizacionesCapicua n))]

-- (factorizacionesCapicua n) es la lista de las factorizaciones
-- capicúas de n. Por ejemplo,
--    factorizacionesCapicua 2015  ==  [[13,5,31],[31,5,13]]
factorizacionesCapicua :: Int -> [[Int]]
factorizacionesCapicua n =
    [xs | xs <- permutations (factorizacion n),
          esCapicuaConcatenacion xs]

-- (factorizacion n) es la lista de todos los factores primos de n; es
-- decir, es una lista de números primos cuyo producto es n. Por ejemplo,
--    factorizacion 300  ==  [2,2,3,5,5]
factorizacion :: Int -> [Int]
factorizacion n | n == 1    = []
                | otherwise = x : factorizacion (div n x)
    where x = menorFactor n

-- (menorFactor n) es el menor factor primo de n. Por ejemplo,
--    menorFactor 15  ==  3
--    menorFactor 16  ==  2
--    menorFactor 17  == 17
menorFactor :: Int -> Int
menorFactor n = head [x | x <- [2..], rem n x == 0]

-- (esCapicuaConcatenacion xs) se verifica si la concatenación de los
-- números de xs es capicúa. Por ejemplo,
--    esCapicuaConcatenacion [13,5,31]   ==  True
--    esCapicuaConcatenacion [135,31]    ==  True
--    esCapicuaConcatenacion [135,21]    ==  False
esCapicuaConcatenacion :: [Int] -> Bool
esCapicuaConcatenacion xs = ys == reverse ys
    where ys = concatMap show xs

-- 2ª definición
-- =============

conFactorizacionesCapicuas2 :: [Int]
conFactorizacionesCapicuas2 =
    [n | n <- [1..], not (null (factorizacionesCapicua2 n))]

-- (factorizacionesCapicua2 n) es la lista de las factorizaciones
-- capicúas de n. Por ejemplo,
--    factorizacionesCapicua2 2015  ==  [[13,5,31],[31,5,13]]
factorizacionesCapicua2 :: Int -> [[Int]]
factorizacionesCapicua2 n =
    [xs | xs <- permutations (primeFactors n),
          esCapicuaConcatenacion xs]

-- 3ª definición
-- =============

conFactorizacionesCapicuas3 :: [Int]
conFactorizacionesCapicuas3 =
    [n | n <- [1..], conFactorizacionCapicua n]

-- (conFactorizacionCapicua n) se verifica si n tiene factorización
-- capicúa. Por ejemplo,
--    factorizacionesCapicua2 2015  ==  [[13,5,31],[31,5,13]]
conFactorizacionCapicua :: Int -> Bool
conFactorizacionCapicua n = 
    any listaCapicua (permutations (primeFactors n))

listaCapicua :: Show a => [a] -> Bool
listaCapicua xs = ys == reverse ys
    where ys = concatMap show xs

-- El cálculo es
--    ghci> head (dropWhile (<=2015) conFactorizacionesCapicuas)
--    2023

-- ---------------------------------------------------------------------
-- Ejercicio 2. Los árboles binarios se pueden representar mediante el
-- tipo Arbol definido por 
--    data Arbol a = H a 
--                 | N a (Arbol a) (Arbol a)
--                 deriving Show
-- Por ejemplo, el árbol
--         "C"
--         / \ 
--        /   \
--       /     \
--     "B"     "A"
--     / \     / \
--   "A" "B" "B" "C" 
-- se puede definir por 
--    ej1 :: Arbol String
--    ej1 = N "C" (N "B" (H "A") (H "B")) (N "A" (H "B") (H "C"))
--
-- Definir la función
--    renombraArbol :: Arbol t -> Arbol Int
-- tal que (renombraArbol a) es el árbol obtenido sustituyendo el valor
-- de los nodos y hojas por números tales que tengan el mismo valor si y
-- sólo si coincide su contenido. Por ejemplo,
--    ghci> renombraArbol ej1
--    N 2 (N 1 (H 0) (H 1)) (N 0 (H 1) (H 2))
-- Gráficamente, 
--          2 
--         / \ 
--        /   \
--       /     \
--      1       0 
--     / \     / \
--    0   1   1   2  
-- Nótese que los elementos del árbol pueden ser de cualquier tipo. Por
-- ejemplo,
--    ghci> renombraArbol (N 9 (N 4 (H 8) (H 4)) (N 8 (H 4) (H 9)))
--    N 2 (N 0 (H 1) (H 0)) (N 1 (H 0) (H 2))
--    ghci> renombraArbol (N True (N False (H True) (H False)) (H True))
--    N 1 (N 0 (H 1) (H 0)) (H 1)
--    ghci> renombraArbol (N False (N False (H True) (H False)) (H True))
--    N 0 (N 0 (H 1) (H 0)) (H 1)
--    ghci> renombraArbol (H False)
--    H 0
--    ghci> renombraArbol (H True)
--    H 0
-- ---------------------------------------------------------------------

data Arbol a = H a
             | N a (Arbol a) (Arbol a) 
             deriving (Show, Eq)

ej1 :: Arbol String
ej1 = N "C" (N "B" (H "A") (H "B")) (N "A" (H "B") (H "C"))

renombraArbol :: Ord t => Arbol t -> Arbol Int
renombraArbol a = aux a
    where ys            = valores a
          aux (H x)     = H (posicion x ys)
          aux (N x i d) = N (posicion x ys) (aux i) (aux d) 

-- (valores a) es la lista de los valores en los nodos y las hojas del
-- árbol a. Por ejemplo,
--    valores ej1  ==  ["A","B","C"]
valores :: Ord a => Arbol a -> [a]
valores a = sort (nub (aux a))
    where aux (H x)     = [x]
          aux (N x i d) = x : (aux i ++ aux d)

-- (posicion x ys) es la posición de x en ys. Por ejemplo.
--    posicion 7 [5,3,7,8]  ==  2
posicion :: Eq a => a -> [a] -> Int
posicion x ys = head [n | (y,n) <- zip ys [0..], y == x]

-- ---------------------------------------------------------------------
-- Ejercicio 3. El buscaminas es un juego cuyo objetivo es despejar un
-- campo de minas sin detonar ninguna. 
-- 
-- El campo de minas se representa mediante un cuadrado con NxN
-- casillas. Algunas casillas tienen un número, este número indica las
-- minas que hay en todas las casillas vecinas. Cada casilla tiene como
-- máximo 8 vecinas. Por ejemplo, el campo 4x4 de la izquierda
-- contiene dos minas, cada una representada por el número 9, y a la 
-- derecha se muestra el campo obtenido anotando las minas vecinas de
-- cada casilla
--    9 0 0 0       9 1 0 0
--    0 0 0 0       2 2 1 0
--    0 9 0 0       1 9 1 0
--    0 0 0 0       1 1 1 0
-- de la misma forma, la anotación del siguiente a la izquierda es el de
-- la derecha 
--    9 9 0 0 0     9 9 1 0 0
--    0 0 0 0 0     3 3 2 0 0
--    0 9 0 0 0     1 9 1 0 0
--
-- Utilizando la librería Data.Matrix, los campos de minas se
-- representan mediante matrices: 
--    type Campo = Matrix Int
-- Por ejemplo, los anteriores campos de la izquierda se definen por
--    ejCampo1, ejCampo2 :: Campo
--    ejCampo1 = fromLists [[9,0,0,0],
--                          [0,0,0,0], 
--                          [0,9,0,0], 
--                          [0,0,0,0]]
--    ejCampo2 = fromLists [[9,9,0,0,0],
--                          [0,0,0,0,0],
--                          [0,9,0,0,0]]
-- 
-- Definir la función
--    buscaminas :: Campo -> Campo
-- tal que (buscaminas c) es el campo obtenido anotando las minas
-- vecinas de cada casilla. Por ejemplo,
--    ghci> buscaminas ejCampo1
--    ( 9 1 0 0 )
--    ( 2 2 1 0 )
--    ( 1 9 1 0 )
--    ( 1 1 1 0 )
--    
--    ghci> buscaminas ejCampo2
--    ( 9 9 1 0 0 )
--    ( 3 3 2 0 0 )
--    ( 1 9 1 0 0 )
-- ---------------------------------------------------------------------

type Campo   = Matrix Int
type Casilla = (Int,Int)

ejCampo1, ejCampo2 :: Campo
ejCampo1 = fromLists [[9,0,0,0],
                      [0,0,0,0], 
                      [0,9,0,0], 
                      [0,0,0,0]]
ejCampo2 = fromLists [[9,9,0,0,0],
                      [0,0,0,0,0],
                      [0,9,0,0,0]]

-- 1ª solución
-- ===========

buscaminas1 :: Campo -> Campo
buscaminas1 c = matrix m n (\(i,j) -> minas c (i,j))
    where m = nrows c
          n = ncols c

-- (minas c (i,j)) es el número de minas en las casillas vecinas de la
-- (i,j) en el campo de mina c y es 9 si en (i,j) hay una mina. Por
-- ejemplo,
--    minas ejCampo (1,1)  ==  9
--    minas ejCampo (1,2)  ==  1
--    minas ejCampo (1,3)  ==  0
--    minas ejCampo (2,1)  ==  2
minas :: Campo -> Casilla -> Int
minas c (i,j) 
    | c!(i,j) == 9 = 9
    | otherwise    = length (filter (==9) 
                            [c!(x,y) | (x,y) <- vecinas m n (i,j)])
                     where m = nrows c
                           n = ncols c

-- (vecinas m n (i,j)) es la lista de las casillas vecinas de la (i,j) en
-- un campo de dimensiones mxn. Por ejemplo,
--    vecinas 4 (1,1)  ==  [(1,2),(2,1),(2,2)]
--    vecinas 4 (1,2)  ==  [(1,1),(1,3),(2,1),(2,2),(2,3)]
--    vecinas 4 (2,3)  ==  [(1,2),(1,3),(1,4),(2,2),(2,4),(3,2),(3,3),(3,4)]
vecinas :: Int -> Int -> Casilla -> [Casilla]
vecinas m n (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)],
                             b <- [max 1 (j-1)..min n (j+1)],
                             (a,b) /= (i,j)]

-- 2ª solución
-- ===========

buscaminas2 :: Campo -> Campo
buscaminas2 c = matrix m n (\(i,j) -> minas (i,j))
    where m = nrows c
          n = ncols c
          minas :: Casilla -> Int
          minas (i,j) 
              | c!(i,j) == 9 = 9
              | otherwise    = 
                  length (filter (==9) [c!(x,y) | (x,y) <- vecinas (i,j)])
          vecinas :: Casilla -> [Casilla]
          vecinas (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)],
                                   b <- [max 1 (j-1)..min n (j+1)],
                                   (a,b) /= (i,j)]

-- ---------------------------------------------------------------------
-- Ejercicio 4. La codificación de Fibonacci de un número n es una
-- cadena d = d(0)d(1)...d(k-1)d(k) de ceros y unos tal que 
--    n = d(0)*F(2) + d(1)*F(3) +...+ d(k-1)*F(k+1) 
--    d(k-1) = d(k) = 1
-- donde F(i) es el i-ésimo término de la sucesión de Fibonacci
--    0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ...
-- Por ejemplo, la codificación de Fibonacci de 4 es "1011" ya que los
-- dos últimos elementos son iguales a 1 y
--    1*F(2) + 0*F(3) + 1*F(4) = 1*1 + 0*2 + 1*3 = 4
-- La codificación de Fibonacci de los primeros números se muestra en la
-- siguiente tabla
--     1  = 1     = F(2)           ≡       11
--     2  = 2     = F(3)           ≡      011
--     3  = 3     = F(4)           ≡     0011
--     4  = 1+3   = F(2)+F(4)      ≡     1011
--     5  = 5     = F(5)           ≡    00011
--     6  = 1+5   = F(2)+F(5)      ≡    10011
--     7  = 2+5   = F(3)+F(5)      ≡    01011
--     8  = 8     = F(6)           ≡   000011
--     9  = 1+8   = F(2)+F(6)      ≡   100011
--    10  = 2+8   = F(3)+F(6)      ≡   010011
--    11  = 3+8   = F(4)+F(6)      ≡   001011
--    12  = 1+3+8 = F(2)+F(4)+F(6) ≡   101011
--    13  = 13    = F(7)           ≡  0000011
--    14  = 1+13  = F(2)+F(7)      ≡  1000011
--
-- Definir la función
--    codigoFib :: Integer -> String
-- tal que (codigoFib n) es la codificación de Fibonacci del número
-- n. Por ejemplo,
--    ghci> codigoFib 65
--    "0100100011"
--    ghci> [codigoFib n | n <- [1..7]]
--    ["11","011","0011","1011","00011","10011","01011"]
-- ---------------------------------------------------------------------

codigoFib :: Integer -> String
codigoFib = (concatMap show) . codificaFibLista

-- (codificaFibLista n) es la lista correspondiente a la codificación de
-- Fibonacci del número n. Por ejemplo,
--    ghci> codificaFibLista 65
--    [0,1,0,0,1,0,0,0,1,1]
--    ghci> [codificaFibLista n | n <- [1..7]]
--    [[1,1],[0,1,1],[0,0,1,1],[1,0,1,1],[0,0,0,1,1],[1,0,0,1,1],[0,1,0,1,1]]
codificaFibLista :: Integer -> [Integer]
codificaFibLista n = map f [2..head xs] ++ [1]
    where xs = map fst (descomposicion n)
          f i | elem i xs = 1
              | otherwise = 0

-- (descomposicion n) es la lista de pares (i,f) tales que f es el
-- i-ésimo número de Fibonacci y las segundas componentes es una
-- sucesión decreciente de números de Fibonacci cuya suma es n. Por
-- ejemplo, 
--    descomposicion 65  ==  [(10,55),(6,8),(3,2)]
--    descomposicion 66  ==  [(10,55),(6,8),(4,3)]
descomposicion :: Integer -> [(Integer, Integer)]
descomposicion 0 = []
descomposicion 1 = [(2,1)]
descomposicion n = (i,x) : descomposicion (n-x)
    where (i,x) = fibAnterior n

-- (fibAnterior n) es el mayor número de Fibonacci menor o igual que
-- n. Por ejemplo,
--    fibAnterior 33  ==  (8,21)
--    fibAnterior 34  ==  (9,34)
fibAnterior :: Integer -> (Integer, Integer)
fibAnterior n = last (takeWhile p fibsConIndice)
    where p (i,x) = x <= n

-- fibsConIndice es la sucesión de los números de Fibonacci junto con
-- sus índices. Por ejemplo,
--    ghci> take 10 fibsConIndice
--    [(0,0),(1,1),(2,1),(3,2),(4,3),(5,5),(6,8),(7,13),(8,21),(9,34)]
fibsConIndice :: [(Integer, Integer)]
fibsConIndice = zip [0..] fibs

-- fibs es la sucesión de Fibonacci. Por ejemplo, 
--    take 10 fibs  ==  [0,1,1,2,3,5,8,13,21,34]
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir las funciones
--    grafo   :: [(Int,Int)] -> Grafo Int Int
--    caminos :: Grafo Int Int -> Int -> Int -> [[Int]]
-- tales que 
-- + (grafo as) es el grafo no dirigido definido cuyas aristas son as. Por
--   ejemplo, 
--      ghci> grafo [(2,4),(4,5)]
--      G ND (array (2,5) [(2,[(4,0)]),(3,[]),(4,[(2,0),(5,0)]),(5,[(4,0)])])
-- + (caminos g a b) es la lista los caminos en el grafo g desde a hasta
--   b sin pasar dos veces por el mismo nodo. Por ejemplo,
--      ghci> sort (caminos (grafo [(1,3),(2,5),(3,5),(3,7),(5,7)]) 1 7)
--      [[1,3,5,7],[1,3,7]]
--      ghci> sort (caminos (grafo [(1,3),(2,5),(3,5),(3,7),(5,7)]) 2 7)
--      [[2,5,3,7],[2,5,7]]
--      ghci> sort (caminos (grafo [(1,3),(2,5),(3,5),(3,7),(5,7)]) 1 2)
--      [[1,3,5,2],[1,3,7,5,2]]
--      ghci> caminos (grafo [(1,3),(2,5),(3,5),(3,7),(5,7)]) 1 4
--      []
--      ghci> length (caminos (grafo [(i,j) | i <- [1..10], j <- [i..10]]) 1 10)
--      109601
-- ---------------------------------------------------------------------

grafo :: [(Int,Int)] -> Grafo Int Int
grafo as = creaGrafo ND (m,n) [(x,y,0) | (x,y) <- as]
    where ns = map fst as ++ map snd as
          m  = minimum ns
          n  = maximum ns

-- 1ª solución
-- ===========

caminos :: Grafo Int Int -> Int -> Int -> [[Int]]
caminos g a b = aux [[b]] where 
    aux [] = []
    aux ((x:xs):yss)
        | x == a    = (x:xs) : aux yss
        | otherwise = aux ([z:x:xs | z <- adyacentes g x
                                   , z `notElem` (x:xs)] 
                           ++ yss) 

-- 2ª solución (mediante espacio de estados)
-- =========================================

caminos2 :: Grafo Int Int -> Int -> Int -> [[Int]]
caminos2 g a b = buscaEE sucesores esFinal inicial
    where inicial          = [b]
          sucesores (x:xs) = [z:x:xs | z <- adyacentes g x
                                     , z `notElem` (x:xs)] 
          esFinal (x:xs)   = x == a

-- Comparación de eficiencia
-- =========================

--    ghci> length (caminos (grafo [(i,j) | i <- [1..10], j <- [i..10]]) 1 10)
--    109601
--    (3.57 secs, 500533816 bytes)
--    ghci> length (caminos2 (grafo [(i,j) | i <- [1..10], j <- [i..10]]) 1 10)
--    109601
--    (3.53 secs, 470814096 bytes)
