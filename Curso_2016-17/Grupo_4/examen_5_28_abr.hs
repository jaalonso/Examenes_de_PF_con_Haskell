-- Informática (1º del Grado en Matemáticas)
-- 5º examen de evaluación continua (28 de abril de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Char
import Data.Matrix

-- ---------------------------------------------------------------------
-- Ejercicio 1. Las rotaciones de 928160 son 928160, 281609, 816092,
-- 160928, 609281 y 92816. De las cuales, las divisibles por 4 son
-- 928160, 816092, 160928 y 92816.
--
-- Definir la función
--    nRotacionesDivisibles :: Integer -> Int
-- tal que (nRotacionesDivisibles n) es el número de rotaciones del
-- número n divisibles por 4. Por ejemplo,
--    nRotacionesDivisibles 928160         ==  4
--    nRotacionesDivisibles 44             ==  2
--    nRotacionesDivisibles (1234^1000)    ==  746
--    nRotacionesDivisibles (1234^100000)  ==  76975
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

nRotacionesDivisibles :: Integer -> Int
nRotacionesDivisibles n =
  length [x | x <- rotacionesNumero n, x `mod` 4 == 0]

-- (rotacionesNumero) es la lista de la rotaciones del número n. Por
-- ejemplo,
--    rotacionesNumero 235  ==  [235,352,523]
rotacionesNumero :: Integer -> [Integer]
rotacionesNumero = map read . rotaciones . show

-- (rotaciones xs) es la lista de las rotaciones obtenidas desplazando
-- el primer elemento xs al final. Por ejemplo,
--    rotaciones [2,3,5]  ==  [[2,3,5],[3,5,2],[5,2,3]]
rotaciones :: [a] -> [[a]]
rotaciones xs = take (length xs) (iterate rota xs)

-- (rota xs) es la lista añadiendo el primer elemento de xs al
-- final. Por ejemplo, 
--    rota [3,2,5,7]  ==  [2,5,7,3]
rota :: [a] -> [a]
rota (x:xs) = xs ++ [x]

-- 2ª definición
-- =============

nRotacionesDivisibles2 :: Integer -> Int
nRotacionesDivisibles2 n = 
  length [x | x <- pares n, x `mod` 4 == 0]

-- (pares n) es la lista de pares de elementos consecutivos, incluyendo
-- el último con el primero. Por ejemplo,
--    pares 928160  ==  [9,92,28,81,16,60]
pares :: Integer -> [Int]
pares n =
  read [last ns,head ns] : [read [a,b] | (a,b) <- zip ns (tail ns)]
  where ns = show n

-- 3ª definición
-- =============

nRotacionesDivisibles3 :: Integer -> Int
nRotacionesDivisibles3 n =
  ( length
  . filter (0 ==)
  . map (`mod` 4)
  . zipWith (\x y -> 2*x + y) d
  . tail
  . (++[i])) d
  where
    d@(i:dn) = (map digitToInt . show) n

-- Comparación de eficiencia
-- =========================

--    ghci> nRotacionesDivisibles (123^1500)
--    803
--    (8.15 secs, 7,109,852,800 bytes)
--    ghci> nRotacionesDivisibles2 (123^1500)
--    803
--    (0.05 secs, 0 bytes)
--    ghci> nRotacionesDivisibles3 (123^1500)
--    803
--    (0.02 secs, 0 bytes)
--
--    ghci> nRotacionesDivisibles2 (1234^50000)
--    38684
--    (2.24 secs, 1,160,467,472 bytes)
--    ghci> nRotacionesDivisibles3 (1234^50000)
--    38684
--    (0.31 secs, 68,252,040 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Los árboles binarios se pueden representar con el tipo
-- de dato algebraico Arbol definido por  
--    data Arbol a = H
--                 | N a (Arbol a) (Arbol a)
--                 deriving (Show, Eq)
-- Por ejemplo, los árboles
--        3                7     
--       / \              / \    
--      2   4            5   8   
--     / \   \          / \   \  
--    1   3   5        6   4   10
--                        /   /
--                       9   1
-- se representan por
--    ej1, ej2 :: Arbol Int
--    ej1 = N 3 (N 2 (N 1 H H) (N 3 H H)) (N 4 H (N 5 H H))
--    ej2 = N 7 (N 5 (N 6 H H) (N 4 (N 9 H H) H)) (N 8 H (N 10 (N 1 H H) H))
--
-- Definir la función
--    aplica :: (a -> b) -> (a -> b) -> Arbol a -> Arbol b
-- tal que (aplica f g) es el árbol obtenido aplicando la función f a
-- los nodos que están a una distancia par de la raíz del árbol y la
-- función g a los nodos que están a una distancia impar de la raíz. Por
-- ejemplo, 
--    ghci> aplica (+1) (*10) ej1
--    N 4 (N 20 (N 2 H H) (N 4 H H)) (N 40 H (N 6 H H))
--    ghci> aplica even odd ej2
--    N False (N True (N True H H) (N True (N True H H) H))
--            (N False H (N True (N True H H) H))
--    ghci> let ej3 = (N "bac" (N "de" (N "tg" (N "hi" H H) (N "js" H H)) H) H)
--    ghci> aplica head last ej3
--    N 'b' (N 'e' (N 't' (N 'i' H H) (N 's' H H)) H) H
-- ---------------------------------------------------------------------

data Arbol a = H 
             | N a (Arbol a) (Arbol a)
             deriving (Show, Eq)

ej1, ej2 :: Arbol Int
ej1 = N 3 (N 2 (N 1 H H) (N 3 H H)) (N 4 H (N 5 H H))
ej2 = N 7 (N 5 (N 6 H H) (N 4 (N 9 H H) H)) (N 8 H (N 10 (N 1 H H) H))

aplica :: (a -> b) -> (a -> b) -> Arbol a -> Arbol b
aplica _ _ H         = H
aplica f g (N x i d) = N (f x) (aplica g f i) (aplica g f d)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir, usando Data.Matrix, la función
--    ampliaMatriz :: Matrix a -> Int -> Int -> Matrix a
-- tal que (ampliaMatriz p f c) es la matriz obtenida a partir de p
-- repitiendo cada fila f veces y cada columna c veces. Por ejemplo, si
-- ejM es la matriz definida por
--    ejM :: Matrix Char
--    ejM = fromLists [" x ",
--                     "x x",
--                     " x "]
-- entonces 
--    ghci> ampliaMatriz ejM 1 2
--    ( ' ' ' ' 'x' 'x' ' ' ' ' )
--    ( 'x' 'x' ' ' ' ' 'x' 'x' )
--    ( ' ' ' ' 'x' 'x' ' ' ' ' )
--    
--    ghci> ampliaMatriz ejM 2 1
--    ( ' ' 'x' ' ' )
--    ( ' ' 'x' ' ' )
--    ( 'x' ' ' 'x' )
--    ( 'x' ' ' 'x' )
--    ( ' ' 'x' ' ' )
--    ( ' ' 'x' ' ' )
-- ---------------------------------------------------------------------

ejM :: Matrix Char
ejM = fromLists [" x ",
                 "x x",
                 " x "]

-- 1ª definición
-- =============

ampliaMatriz :: Matrix a -> Int -> Int -> Matrix a
ampliaMatriz p f =
  ampliaColumnas (ampliaFilas p f) 

ampliaFilas :: Matrix a -> Int -> Matrix a
ampliaFilas p f =
  matrix (f*m) n (\(i,j) -> p!(1 + (i-1) `div` f, j))
  where m = nrows p
        n = ncols p

ampliaColumnas :: Matrix a -> Int -> Matrix a
ampliaColumnas p c =
  matrix m (c*n) (\(i,j) -> p!(i,1 + (j-1) `div` c))
  where m = nrows p
        n = ncols p

-- 2ª definición
-- =============

ampliaMatriz2 :: Matrix a -> Int -> Int -> Matrix a
ampliaMatriz2 p f c =
  ( fromLists
  . concatMap (replicate f . concatMap (replicate c))
  . toLists) p

-- Comparación de eficiencia
-- =========================

ejemplo :: Int -> Matrix Int
ejemplo n = fromList n n [1..]

--    ghci> maximum (ampliaMatriz (ejemplo 10) 100 200)
--    100
--    (6.44 secs, 1,012,985,584 bytes)
--    ghci> maximum (ampliaMatriz2 (ejemplo 10) 100 200)
--    100
--    (2.38 secs, 618,096,904 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Una serie de potencias es una serie de la forma
--    a(0) + a(1)x + a(2)x^2 + a(3)x^3 + ...
--
-- Las series de potencias se pueden representar mediante listas
-- infinitas. Por ejemplo, la serie de la función exponencial es
--    e^x = 1 + x + x^2/2! + x^3/3! + ...
-- se puede representar por [1, 1, 1/2, 1/6, 1/24, 1/120, ...]
--
-- Las operaciones con series se pueden ver como una generalización de
-- las de los polinomios.
-- 
-- Definir la función     
--    producto :: Num a => [a] -> [a] -> [a]
-- tal que (producto xs ys) es el producto de las series xs e ys. Por
-- ejemplo, 
--    ghci> take 15 (producto [3,5..] [2,4..])
--    [6,22,52,100,170,266,392,552,750,990,1276,1612,2002,2450,2960]
--    ghci> take 14 (producto [10,20..] [1,3..])
--    [10,50,140,300,550,910,1400,2040,2850,3850,5060,6500,8190,10150]
--    ghci> take 16 (producto [1..] primes)
--    [2,7,17,34,62,103,161,238,338,467,627,824,1062,1343,1671,2052]
--    ghci> take 16 (producto [1,1..] primes)
--    [2,5,10,17,28,41,58,77,100,129,160,197,238,281,328,381]
--    ghci> take 16 (scanl1 (+) primes)
--    [2,5,10,17,28,41,58,77,100,129,160,197,238,281,328,381]
-- ---------------------------------------------------------------------

producto :: Num a => [a] -> [a] -> [a]
producto (x:xs) zs@(y:ys) = 
  x*y : suma (producto xs zs) (map (x*) ys)

-- (suma xs ys) es la suma de las series xs e ys. Por ejemplo,
--    ghci> take 7 (suma [1,3..] [2,4..])
--    [3,7,11,15,19,23,27]
suma :: Num a => [a] -> [a] -> [a]
suma = zipWith (+)
