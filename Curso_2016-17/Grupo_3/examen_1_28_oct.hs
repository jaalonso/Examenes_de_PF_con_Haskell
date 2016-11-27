-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 1º examen de evaluación continua (28 de octubre de 2016)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Decimos el número y es colega de x si la suma de los
-- dígitos de x coincide con el producto de los dígitos de y. Por
-- ejemplo, 24 es colega de 23.
-- 
-- Definir la función
--    esColega :: Int -> Int -> Bool
-- tal que (esColega x y) que se verifique si y es colega de x. Por
-- ejemplo,  
--    esColega 24 23   ==  True
--    esColega 23 24   ==  False
--    esColega 24 611  ==  True
-- ---------------------------------------------------------------------

esColega :: Int -> Int -> Bool
esColega x y = sum (digitos x) == product (digitos y)

digitos :: Int -> [Int]
digitos n = [read [x] | x <- show n]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función
--    colegas3 :: Int -> [Int]
-- tal que (colegas3 x) es la lista de los colegas de x con tres
-- dígitos. Por ejemplo,
--    colegas3 24  ==  [116,123,132,161,213,231,312,321,611]
-- ---------------------------------------------------------------------

-- 1ª solución
colegas3 :: Int -> [Int]
colegas3 x = [y | y <- [100 .. 999], esColega x y]

-- 2ª solución
colegas3b :: Int -> [Int]
colegas3b x = filter (x `esColega`) [100 .. 999]

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la función
--    colegasN :: Int -> Int -> [Int]
-- tal que (colegasN x n) es la lista de colegas de x con menos de n
-- dígitos. Por ejemplo,
--    ghci> colegasN 24 4
--    [6,16,23,32,61,116,123,132,161,213,231,312,321,611]
-- ---------------------------------------------------------------------

-- 1ª solución
colegasN :: Int -> Int -> [Int]
colegasN x n = [y | y <- [1..10^(n-1)-1], esColega x y]

-- 2ª solución
colegasN2 :: Int -> Int -> [Int]
colegasN2 x n = filter (x `esColega`) [1..10^(n-1)-1]

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir, usando condicionales sin guardas, la función
--    divideMitad1 :: [a] -> ([a],[a])
-- tal que (divideMitad1 xs) es el par de lista de igual longitud en que
-- se divide la lista xs (eliminando el elemento central si la longitud
-- de xs es impar). Por ejemplo,   
--    divideMitad1 [2,3,5,1]  ==  ([2,3],[5,1])
--    divideMitad1 [2,3,5]    ==  ([2],[5])
--    divideMitad1 [2]        ==  ([],[])
-- ---------------------------------------------------------------------

divideMitad1 :: [a] -> ([a], [a])
divideMitad1 xs =
  if even (length xs) 
  then (take n xs, drop n xs)
  else (take n xs, drop (n+1) xs)
  where n = div (length xs) 2

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, usando guardas sin condicionales, la función
--    divideMitad2 :: [a] -> ([a],[a])
-- tal que (divideMitad2 xs) es el par de lista de igual longitud en que
-- se divide la lista xs (eliminando el elemento central si la longitud
-- de xs es impar). Por ejemplo,   
--    divideMitad2 [2,3,5,1]  ==  ([2,3],[5,1])
--    divideMitad2 [2,3,5]    ==  ([2],[5])
--    divideMitad2 [2]        ==  ([],[])
-- ---------------------------------------------------------------------

divideMitad2 :: [a] -> ([a], [a])
divideMitad2 xs
  | even (length xs) = (take n xs, drop n xs)
  | otherwise        = (take n xs, drop (n+1) xs)
  where n = div (length xs) 2

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Comprobar con QuickCheck que las funciones
-- divideMitad1 y divideMitad2 son equivalentes para listas de números
-- enteros. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_divideMitad :: [Int] -> Bool
prop_divideMitad xs =
  divideMitad1 xs == divideMitad2 xs

-- La comprobacion es:
--    ghci> quickCheck prop_divideMitad
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    inserta :: [a] -> [[a]] -> [[a]]
-- tal que (inserta xs yss) es la lista obtenida insertando 
-- + el primer elemento de xs como primero en la primera lista de yss,
-- + el segundo elemento de xs como segundo en la segunda lista de yss
--   (si la segunda lista de yss tiene al menos un elemento), 
-- + el tercer elemento de xs como tercero en la tercera lista de yss
--   (si la tercera lista de yss tiene al menos dos elementos), 
-- y así sucesivamente. Por ejemplo, 
--    inserta [1,2,3] [[4,7],[6],[9,5,8]]  ==  [[1,4,7],[6,2],[9,5,3,8]]
--    inserta [1,2,3] [[4,7],[] ,[9,5,8]]  ==  [[1,4,7],[],   [9,5,3,8]]
--    inserta [1,2]   [[4,7],[6],[9,5,8]]  ==  [[1,4,7],[6,2],[9,5,8]]
--    inserta [1,2,3] [[4,7],[6]]          ==  [[1,4,7],[6,2]]
--    inserta "tad"   ["odo","pra","naa"]  ==  ["todo","para","nada"]
-- ---------------------------------------------------------------------

inserta :: [a] -> [[a]] -> [[a]]
inserta xs yss = aux xs yss 0 where
    aux [] yss _ = yss
    aux xs []  _ = []
    aux (x:xs) (ys:yss) n 
        | length us == n = (us ++ x : vs) : aux xs yss (n+1)
        | otherwise      = ys : aux xs yss (n+1)
        where (us,vs) = splitAt n ys

-- ---------------------------------------------------------------------
-- Ejercicio 4. Un elemento de una lista es un pivote si ninguno de
-- los siguientes en la lista es mayor que él.
--
-- Definirla función 
--    pivotes :: Ord a => [a] -> [a]
-- tal que (pivotes xs) es la lista de los pivotes de xs. Por
-- ejemplo, 
--    pivotes [80,1,7,8,4]  ==  [80,8,4]
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión)
-- ===============================

pivotes :: Ord a => [a] -> [a]
pivotes xs = [x | (x,n) <- zip xs [1 ..], esMayor x (drop n xs)] 

-- (esMayor x ys) se verifica si x es mayor que todos los elementos de
-- ys. Por ejemplo,
esMayor :: Ord a =>  a -> [a] -> Bool
esMayor x xs = and [x > y | y <- xs]

-- 2ª definición (por recursión)
-- =============================

pivotes2 :: Ord a => [a] -> [a]
pivotes2 [] = []
pivotes2 (x:xs) | esMayor2 x xs = x : pivotes2 xs
                | otherwise     = pivotes2 xs

esMayor2 :: Ord a =>  a -> [a] -> Bool
esMayor2 x xs = all (x>) xs

-- 3ª definición
-- =============

pivotes3 :: Ord a => [a] -> [a]
pivotes3 xs =
  [y | (y:ys) <- init (tails xs)
     , y `esMayor` ys]
