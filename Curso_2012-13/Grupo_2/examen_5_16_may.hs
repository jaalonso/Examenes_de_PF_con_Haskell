-- Informática (1º del Grado en Matemáticas)
-- 5º examen de evaluación continua (16 de mayo de 2013)
-- ---------------------------------------------------------------------

import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    empiezanPorUno :: [Int] -> [Int]
-- tal que (empiezanPorUno xs) es la lista de los elementos de xs que
-- empiezan por uno. Por ejemplo,
--    empiezanPorUno [31,12,7,143,214]  ==  [12,143]
-- ---------------------------------------------------------------------

-- 1ª definición: Por comprensión:
empiezanPorUno1 :: [Int] -> [Int]
empiezanPorUno1 xs = 
  [x | x <- xs, head (show x) == '1']
  
-- 2ª definición: Por filtrado:  
empiezanPorUno2 :: [Int] -> [Int]
empiezanPorUno2 xs =
  filter empiezaPorUno xs
  
empiezaPorUno :: Int -> Bool  
empiezaPorUno x =
  head (show x) == '1'
  
-- 3ª definición: Por recursión:
empiezanPorUno3 :: [Int] -> [Int]
empiezanPorUno3 [] = []
empiezanPorUno3 (x:xs) | empiezaPorUno x = x : empiezanPorUno3 xs
                       | otherwise       = empiezanPorUno3 xs
  
-- 4ª definición: Por plegado:
empiezanPorUno4 :: [Int] -> [Int]
empiezanPorUno4 = foldr f []
  where f x ys | empiezaPorUno x = x : ys
               | otherwise       = ys
                                   
-- ---------------------------------------------------------------------
-- Ejercicio 2. Esta semana A. Helfgott ha publicado la primera
-- demostración de la conjetura débil de Goldbach que dice que todo
-- número impar mayor que 5 es suma de tres números primos (puede
-- repetirse alguno).  
-- 
-- Definir la función
--    sumaDe3Primos :: Int -> [(Int,Int,Int)]
-- tal que (sumaDe3sPrimos n) es la lista de las distintas
-- descomposiciones de n como suma de tres números primos. Por ejemplo, 
--    sumaDe3Primos 7  ==  [(2,2,3)]
--    sumaDe3Primos 9  ==  [(2,2,5),(3,3,3)]
-- Calcular cuál es el menor número que se puede escribir de más de 500
-- formas como suma de tres números primos.         
-- ---------------------------------------------------------------------

sumaDe3Primos :: Int -> [(Int,Int,Int)]
sumaDe3Primos n = 
    [(x,y,n-x-y) | y <- primosN, 
                   x <- takeWhile (<=y) primosN,
                   x+y <= n,
                   y <= n-x-y,
                   elem (n-x-y) primosN]
    where primosN = takeWhile (<=n) primos

-- (esPrimo n) se verifica si n es primo.
esPrimo :: Int-> Bool
esPrimo n = [x | x <- [1..n], rem n x == 0] == [1,n]

-- primos es la lista de los números primos.
primos :: [Int]
primos = 2 : [n | n <- [3,5..], esPrimo n]

-- El cálculo es
--    ghci> head [n | n <- [1..], length (sumaDe3Primos n) > 500]
--    587

-- ---------------------------------------------------------------------
-- Ejercicio 3. Los polinomios pueden representarse de forma densa. Por
-- ejemplo, el polinomio 6x^4-5x^2+4x-7 se puede representar por 
-- [(4,6),(2,-5),(1,4),(0,-7)].  
-- 
-- Definir la función 
--    suma :: (Num a, Eq a) => [(Int,a)] -> [(Int,a)] -> [(Int,a)] 
-- tal que (suma p q) es suma de los polinomios p y q representados de
-- forma densa. Por ejemplo,
--    ghci> suma [(5,3),(1,2),(0,1)] [(1,6),(0,4)]
--    [(5,3),(1,8),(0,5)]
--    ghci> suma [(1,6),(0,4)] [(5,3),(1,2),(0,1)] 
--    [(5,3),(1,8),(0,5)]
--    ghci> suma [(5,3),(1,2),(0,1)] [(5,-3),(1,6),(0,4)]
--    [(1,8),(0,5)]
--    ghci> suma [(5,3),(1,2),(0,1)] [(5,4),(1,-2),(0,4)]
--    [(5,7),(0,5)]
-- ---------------------------------------------------------------------

suma :: (Num a, Eq a) => [(Int,a)] -> [(Int,a)] -> [(Int,a)] 
suma [] q = q
suma p [] = p
suma ((n,b):p) ((m,c):q)
    | n > m      = (n,b) : suma p ((m,c):q)
    | n < m      = (m,c) : suma ((n,b):p) q
    | b + c == 0 = suma p q
    | otherwise  = (n,b+c) : suma p q

-- ---------------------------------------------------------------------
-- Ejercicio 4. Se define el tipo de las matrices enteras por
--    type Matriz = Array (Integer,Integer) Integer 
-- Definir la función 
--    borraCols :: Integer -> Integer -> Matriz -> Matriz
-- tal que (borraCols j1 j2 p) es la matriz obtenida borrando las
-- columnas j1 y j2 (con j1 < j2) de la matriz p. Por ejemplo,
--    ghci> let p = listArray ((1,1),(2,4)) [1..8]
--    ghci> p
--    array ((1,1),(2,4)) [((1,1),1),((1,2),2),((1,3),3),((1,4),4),
--                         ((2,1),5),((2,2),6),((2,3),7),((2,4),8)]
--    ghci> borraCols 1 3 p
--    array ((1,1),(2,2)) [((1,1),2),((1,2),4),((2,1),6),((2,2),8)]
--    ghci> borraCols 2 3 p
--    array ((1,1),(2,2)) [((1,1),1),((1,2),4),((2,1),5),((2,2),8)]
-- ---------------------------------------------------------------------
        
type Matriz = Array (Integer,Integer) Integer

-- 1ª definición: 
borraCols :: Integer -> Integer -> Matriz -> Matriz
borraCols j1 j2 p = 
  borraCol (j2-1) (borraCol j1 p)

-- (borraCol j1 p) es la matriz obtenida borrando la columna j1 de la
-- matriz p. Por ejemplo,
--    ghci> let p = listArray ((1,1),(2,4)) [1..8]
--    ghci> borraCol 2 p
--    array ((1,1),(2,3)) [((1,1),1),((1,2),3),((1,3),4),((2,1),5),((2,2),7),((2,3),8)]
--    ghci> borraCol 3 p
--    array ((1,1),(2,3)) [((1,1),1),((1,2),2),((1,3),4),((2,1),5),((2,2),6),((2,3),8)]
borraCol :: Integer -> Matriz -> Matriz
borraCol j1 p = 
  array ((1,1),(m,n-1))
        [((i,j), f i j)| i <- [1..m], j <- [1..n-1]]
  where (_,(m,n)) = bounds p
        f i j | j < j1    = p!(i,j)
              | otherwise = p!(i,j+1)

-- 2ª definición: 
borraCols2 :: Integer -> Integer -> Matriz -> Matriz
borraCols2 j1 j2 p = 
  array ((1,1),(m,n-2))
        [((i,j), f i j)| i <- [1..m], j <- [1..n-2]]
  where (_,(m,n)) = bounds p
        f i j | j < j1    = p!(i,j)
              | j < j2-1  = p!(i,j+1)
              | otherwise = p!(i,j+2)

-- 3ª definición: 
borraCols3 :: Integer -> Integer -> Matriz -> Matriz
borraCols3 j1 j2 p = 
  listArray ((1,1),(n,m-2)) [p!(i,j) | i <- [1..n], j <- [1..m], j/=j1 && j/=j2]
  where (_,(n,m)) = bounds p
