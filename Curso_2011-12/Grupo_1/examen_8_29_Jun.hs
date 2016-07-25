-- Informática (1º del Grado en Matemáticas, Grupo 2)
-- Examen de la 1ª convocatoria (29 de junio de 2012)
-- ---------------------------------------------------------------------

import Data.List
import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1. [2 puntos] Definir la función
--    paresOrdenados :: [a] -> [(a,a)]
-- tal que (paresOrdenados xs) es la lista de todos los pares de
-- elementos (x,y) de xs, tales que x ocurren en xs antes que y. Por
-- ejemplo,  
--    paresOrdenados [3,2,5,4] == [(3,2),(3,5),(3,4),(2,5),(2,4),(5,4)]
--    paresOrdenados [3,2,5,3] == [(3,2),(3,5),(3,3),(2,5),(2,3),(5,3)]
-- ---------------------------------------------------------------------

-- 1ª definición:
paresOrdenados :: [a] -> [(a,a)]
paresOrdenados []     = []
paresOrdenados (x:xs) = [(x,y) | y <- xs] ++ paresOrdenados xs

-- 2ª definición:
paresOrdenados2 :: [a] -> [(a,a)]
paresOrdenados2 [] = []
paresOrdenados2 (x:xs) = 
    foldr (\y ac -> (x,y):ac) (paresOrdenados2 xs) xs

-- 3ª definición (con repeat):
paresOrdenados3 :: [a] -> [(a,a)]
paresOrdenados3 []     = []
paresOrdenados3 (x:xs) = zip (repeat x) xs ++ paresOrdenados3 xs

-- ---------------------------------------------------------------------
-- Ejercicio 2. [2 puntos] Definir la función 
--    sumaDeDos :: Int -> [Int] -> Maybe (Int,Int)
-- tal que (sumaDeDos x ys) decide si x puede expresarse como suma de
-- dos elementos de ys y, en su caso, devuelve un par de elementos de ys
-- cuya suma es x. Por ejemplo,
--    sumaDeDos 9 [7,4,6,2,5]  ==  Just (7,2)
--    sumaDeDos 5 [7,4,6,2,5]  ==  Nothing
-- ---------------------------------------------------------------------

sumaDeDos :: Int -> [Int] -> Maybe (Int,Int)
sumaDeDos _ []  =  Nothing
sumaDeDos _ [_] =  Nothing
sumaDeDos y (x:xs) | y-x `elem` xs = Just (x,y-x)
                   | otherwise     = sumaDeDos y xs

-- 2ª definición (usando paresOrdenados):
sumaDeDos2 :: Int -> [Int] -> Maybe (Int,Int)
sumaDeDos2 x xs 
    | null ys   = Nothing
    | otherwise = Just (head ys)
    where ys = [(a,b) | (a,b) <- paresOrdenados xs , a+b == x]

-- ---------------------------------------------------------------------
-- Ejercicio 3. [2 puntos] Definir la función
--    esProductoDeDosPrimos :: Int -> Bool
-- tal que (esProductoDeDosPrimos n) se verifica si n es el producto de
-- dos primos distintos. Por ejemplo,
--    esProductoDeDosPrimos 6  ==  True
--    esProductoDeDosPrimos 9  ==  False
-- ---------------------------------------------------------------------

esProductoDeDosPrimos :: Int -> Bool
esProductoDeDosPrimos n =
    [x | x <- primosN, 
         mod n x == 0, 
         div n x /= x, 
         div n x `elem` primosN] /= []
    where primosN = takeWhile (<=n) primos

primos :: [Int]
primos = criba [2..]
    where criba []     = []
          criba (n:ns) = n : criba (elimina n ns)
          elimina n xs = [x | x <- xs, x `mod` n /= 0]

-- ---------------------------------------------------------------------
-- Ejercicio 4. [2 puntos] La expresiones aritméticas se pueden
-- representar mediante el siguiente tipo 
--    data Expr = V Char 
--              | N Int 
--              | S Expr Expr
--              | P Expr Expr
--              deriving Show
-- por ejemplo, representa la expresión "z*(3+x)" se representa por
-- (P (V 'z') (S (N 3) (V 'x'))). 
--
-- Definir la función
--    sustitucion :: Expr -> [(Char, Int)] -> Expr
-- tal que (sustitucion e s) es la expresión obtenida sustituyendo las
-- variables de la expresión e según se indica en la sustitución s. Por
-- ejemplo, 
--    ghci> sustitucion (P (V 'z') (S (N 3) (V 'x'))) [('x',7),('z',9)]
--    P (N 9) (S (N 3) (N 7))
--    ghci> sustitucion (P (V 'z') (S (N 3) (V 'y'))) [('x',7),('z',9)]
--    P (N 9) (S (N 3) (V 'y'))
-- ---------------------------------------------------------------------
                   
data Expr = V Char 
          | N Int 
          | S Expr Expr
          | P Expr Expr
          deriving Show

sustitucion :: Expr -> [(Char, Int)] -> Expr
sustitucion e [] = e
sustitucion (V c) ((d,n):ps) | c == d = N n
                             | otherwise = sustitucion (V c) ps
sustitucion (N n) _ = N n                                 
sustitucion (S e1 e2) ps = S (sustitucion e1 ps) (sustitucion e2 ps)
sustitucion (P e1 e2) ps = P (sustitucion e1 ps) (sustitucion e2 ps)

-- ---------------------------------------------------------------------
-- Ejercicio 5. [2 puntos] (Problema 345 del proyecto Euler) Las
-- matrices puede representarse mediante tablas cuyos índices son pares
-- de números naturales:   
--    type Matriz = Array (Int,Int) Int
-- Definir la función 
--    maximaSuma :: Matriz -> Int
-- tal que (maximaSuma p) es el máximo de las sumas de las listas de
-- elementos de la matriz p tales que cada elemento pertenece sólo a una
-- fila y a una columna. Por ejemplo, 
--    ghci> maximaSuma (listArray ((1,1),(3,3)) [1,2,3,8,4,9,5,6,7])
--    17
-- ya que las selecciones, y sus sumas, de la matriz
--    |1 2 3|
--    |8 4 9|
--    |5 6 7|
-- son
--    [1,4,7] --> 12
--    [1,9,6] --> 16
--    [2,8,7] --> 17
--    [2,9,5] --> 16
--    [3,8,6] --> 17
--    [3,4,5] --> 12
-- Hay dos selecciones con máxima suma: [2,8,7] y [3,8,6].
-- ---------------------------------------------------------------------

type Matriz = Array (Int,Int) Int

maximaSuma :: Matriz -> Int
maximaSuma p = maximum [sum xs | xs <- selecciones p]

-- (selecciones p) es la lista de las selecciones en las que cada
-- elemento pertenece a un única fila y a una única columna de la matriz
-- p. Por ejemplo,
--    ghci> selecciones (listArray ((1,1),(3,3)) [1,2,3,8,4,9,5,6,7])
--    [[1,4,7],[2,8,7],[3,4,5],[2,9,5],[3,8,6],[1,9,6]]
selecciones :: Matriz -> [[Int]]
selecciones p = 
    [[p!(i,j) | (i,j) <- ijs] | 
     ijs <- [zip [1..n] xs | xs <- permutations [1..n]]] 
    where (_,(m,n)) = bounds p

-- Nota: En la anterior definición se ha usado la función pernutations
-- de Data.List. También se puede definir mediante
permutaciones :: [a] -> [[a]]
permutaciones []     = [[]]
permutaciones (x:xs) = 
    concat [intercala x ys | ys <- permutaciones xs]

-- (intercala x ys) es la lista de las listas obtenidas intercalando x
-- entre los elementos de ys. Por ejemplo, 
--    intercala 1 [2,3]  ==  [[1,2,3],[2,1,3],[2,3,1]]
intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys) : [y:zs | zs <- intercala x ys]

-- 2ª solución (mediante submatrices):
maximaSuma2 :: Matriz -> Int
maximaSuma2 p 
    | (m,n) == (1,1) = p!(1,1)
    | otherwise = maximum [p!(1,j) 
                  + maximaSuma2 (submatriz 1 j p) | j <- [1..n]]
    where (m,n) = dimension p

-- (dimension p) es la dimensión de la matriz p.
dimension :: Matriz -> (Int,Int)
dimension = snd . bounds

-- (submatriz i j p) es la matriz obtenida a partir de la p eliminando
-- la fila i y la columna j. Por ejemplo, 
--    ghci> submatriz 2 3 (listArray ((1,1),(3,3)) [1,2,3,8,4,9,5,6,7])
--    array ((1,1),(2,2)) [((1,1),1),((1,2),2),((2,1),5),((2,2),6)]
submatriz :: Int -> Int -> Matriz -> Matriz
submatriz i j p = 
    array ((1,1), (m-1,n -1))
          [((k,l), p ! f k l) | k <- [1..m-1], l <- [1.. n-1]]
    where (m,n) = dimension p
          f k l | k < i  && l < j  = (k,l)
                | k >= i && l < j  = (k+1,l)
                | k < i  && l >= j = (k,l+1)
                | otherwise        = (k+1,l+1)
