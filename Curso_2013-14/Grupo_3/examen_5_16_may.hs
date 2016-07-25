-- Informática (1º del Grado en Matemáticas)
-- 5º examen de evaluación continua (16 de mayo de 2014)
-- ---------------------------------------------------------------------

import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1. [2 puntos] Un mínimo local de una lista es un elemento
-- de la lista que es menor que su predecesor y que su sucesor en la
-- lista. Por ejemplo, 1 es un mínimo local de [3,2,1,3,7,7,1,0,2] ya
-- que es menor  que 2 (su predecesor) y que 3 (su sucesor).
-- 
-- Definir la función
--    minimosLocales :: Ord a => [a] -> [a]
-- tal que (minimosLocales xs) es la lista de los mínimos locales de la
-- lista xs. Por ejemplo,
--    minimosLocales [3,2,1,3,7,7,9,6,8]  ==  [1,6]
--    minimosLocales [1..100]             ==  []
--    minimosLocales "mqexvzat"           ==  "eva"
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión):
minimosLocales1 :: Ord a => [a] -> [a]
minimosLocales1 (x:y:z:xs) | y < x && y < z = y : minimosLocales1 (z:xs)
                           | otherwise      = minimosLocales1 (y:z:xs)
minimosLocales1 _                           = []

-- 2ª definición (por comprensión):
minimosLocales2 :: Ord a => [a] -> [a]
minimosLocales2 xs = 
    [y | (x,y,z) <- zip3 xs (tail xs) (drop 2 xs), y < x, y < z]

-- ---------------------------------------------------------------------
-- Ejercicio 2. [2 puntos] Definir la función
--    sumaExtremos :: Num a => [a] -> [a]
-- tal que (sumaExtremos xs) es la lista sumando el primer elemento de
-- xs con el último, el segundo con el penúltimo y así
-- sucesivamente. Por ejemplo,
--    sumaExtremos [6,5,3,1]              ==  [7,8]
--    sumaExtremos [6,5,3]                ==  [9,10]
--    sumaExtremos [3,2,3,2]              ==  [5,5]
--    sumaExtremos [6,5,3,1,2,0,4,7,8,9]  ==  [15,13,10,5,2]
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión):
sumaExtremos1 :: Num a => [a] -> [a]
sumaExtremos1 []     = []
sumaExtremos1 [x]    = [x+x]
sumaExtremos1 (x:xs) = (x + last xs) : sumaExtremos1 (init xs)


-- 2ª definición (por recursión):
sumaExtremos2 :: Num a => [a] -> [a]
sumaExtremos2 xs = aux (take n xs) (take n (reverse xs))
    where aux [] []         = []
          aux (x:xs) (y:ys) = x+y : aux xs ys
          m = length xs
          n | even m    = m `div` 2
            | otherwise = 1 + (m `div` 2)

-- 3ª definición (con zip):
sumaExtremos3 :: Num a => [a] -> [a]
sumaExtremos3 xs = take n [x+y | (x,y) <- zip xs (reverse xs)]
    where m = length xs
          n | even m    = m `div` 2
            | otherwise = 1 + (m `div` 2)

-- 4ª definición (con zipWith):
sumaExtremos4 :: Num a => [a] -> [a]
sumaExtremos4 xs = take n (zipWith (+) xs (reverse xs))
    where m = length xs
          n | even m    = m `div` 2
            | otherwise = 1 + (m `div` 2)

-- ---------------------------------------------------------------------
-- Ejercicio 3. [2 puntos] Definir la función
--    listaRectangular :: Int -> Int -> a -> [a] -> [[a]] 
-- tal que (listaRectangular m n x xs) es una lista de m listas de
-- longitud n formadas con los elementos de xs completada con x, si no
-- xs no tiene suficientes elementos. Por ejemplo,
--    listaRectangular 2 4 7 [0,3,5,2,4]  ==  [[0,3,5,2],[4,7,7,7]]
--    listaRectangular 4 2 7 [0,3,5,2,4]  ==  [[0,3],[5,2],[4,7],[7,7]]
--    listaRectangular 2 3 7 [0..]        ==  [[0,1,2],[3,4,5]]
--    listaRectangular 3 2 7 [0..]        ==  [[0,1],[2,3],[4,5]]
--    listaRectangular 3 2 'p' "eva"      ==  ["ev","ap","pp"]
--    listaRectangular 3 2 'p' ['e'..]    ==  ["ef","gh","ij"]
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión):
listaRectangular1 :: Int -> Int -> a -> [a] -> [[a]] 
listaRectangular1 m n x xs =
    take m (grupos n (xs ++ repeat x))

-- (grupos n xs) es la lista obtenida agrupando los elementos de xs en
-- grupos de n elementos, salvo el último que puede tener menos. Por
-- ejemplo, 
--    grupos 2 [4,2,5,7,6]     ==  [[4,2],[5,7],[6]]
--    take 3 (grupos 3 [1..])  ==  [[1,2,3],[4,5,6],[7,8,9]]
grupos :: Int -> [a] -> [[a]]
grupos _ [] = []
grupos n xs = take n xs : grupos n (drop n xs)

-- 2ª definición (por comprensión)
listaRectangular2 :: Int -> Int -> a -> [a] -> [[a]]
listaRectangular2 m n x xs = 
    take m [take n ys | m <- [0,n..n^2],
                        ys <- [drop m xs ++ (replicate m x)]]

-- 3ª definición (por iteración):
listaRectangular3 :: Int -> Int -> a -> [a] -> [[a]] 
listaRectangular3 m n x xs =
    take n [take n ys | ys <- iterate (drop n) (xs ++ repeat x)]

-- 4ª definición (sin el 4º argumento):
listaRectangular4 :: Int -> Int -> a -> [a] -> [[a]] 
listaRectangular4 m n x = 
    take m . map (take n) . iterate (drop n) . (++ repeat x)

-- ---------------------------------------------------------------------
-- Ejercicio 4 [2 puntos] Las expresiones aritméticas se pueden definir
-- usando el siguiente tipo de datos
--    data Expr = N Int 
--              | S Expr Expr 
--              | P Expr Expr 
--              deriving (Eq, Show)
-- Por ejemplo, la expresión 
--    3*5 + 6*7
-- se puede definir por
--    S (P (N 3) (N 5)) (P (N 6) (N 7))
-- 
-- Definir la función  
--    aplica :: (Int -> Int) -> Expr -> Expr
-- tal que (aplica f e) es la expresión obtenida aplicando la función f
-- a cada uno de los números de la expresión e. Por ejemplo, 
--    ghci> aplica (+2) (S (P (N 3) (N 5)) (P (N 6) (N 7)))
--    S (P (N 5) (N 7)) (P (N 8) (N 9))
--    ghci> aplica (*2) (S (P (N 3) (N 5)) (P (N 6) (N 7)))
--    S (P (N 6) (N 10)) (P (N 12) (N 14))
-- ---------------------------------------------------------------------

data Expr = N Int 
          | S Expr Expr 
          | P Expr Expr 
          deriving (Eq, Show)

aplica :: (Int -> Int) -> Expr -> Expr
aplica f (N x)     = N (f x)
aplica f (S e1 e2) = S (aplica f e1) (aplica f e2)
aplica f (P e1 e2) = P (aplica f e1) (aplica f e2)

-- ---------------------------------------------------------------------
-- Ejercicio 5. [2 puntos] Las matrices enteras se pueden representar
-- mediante tablas con índices enteros: 
--    type Matriz = Array (Int,Int) Int
-- Por ejemplo, la matriz
--    |4 1 3|
--    |1 2 8|
--    |6 5 7|
-- se puede definir por
--    listArray ((1,1),(3,3)) [4,1,3, 1,2,8, 6,5,7]		
-- 
-- Definir la función
--    sumaColumnas :: Matriz -> Matriz
-- tal que (sumaColumnas p) es la matriz obtenida sumando a cada columna
-- la anterior salvo a la primera que le suma la última columna. Por
-- ejemplo, 
--    ghci> sumaColumnas (listArray ((1,1),(3,3)) [4,1,3, 1,2,8, 6,5,7])
--    array ((1,1),(3,3)) [((1,1),7), ((1,2),5), ((1,3),4),
--                         ((2,1),9), ((2,2),3), ((2,3),10),
--                         ((3,1),13),((3,2),11),((3,3),12)]
-- es decir, el resultado es la matriz
--    | 7  5  4|
--    | 9  3 10|
--    |13 11 12|
-- ------------------------------------------------------------------

type Matriz = Array (Int,Int) Int

sumaColumnas :: Matriz -> Matriz
sumaColumnas p =  
    array ((1,1),(m,n)) 
          [((i,j), f i j) | i <- [1..m], j <- [1..n]] 
    where (_,(m,n)) = bounds p
          f i 1 = p!(i,1) + p!(i,m)
          f i j = p!(i,j) + p!(i,j-1)
