-- Informática (1º del Grado en Matemáticas)
-- 2º examen de evaluación continua (2 de diciembre de 2016)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- --------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    elevaSumaR  :: [Integer] -> [Integer] -> Integer
-- tal que (elevaSuma xs ys) es la suma de la potencias de los elementos 
-- de xs elevados a los elementos de ys respectivamente. Por ejemplo,
--    elevaSuma  [2,6,9]  [3,2,0]   = 8 + 36 + 1    = 45
--    elevaSuma  [10,2,5] [3,4,1,2] = 1000 + 16 + 5 = 1021
-- ---------------------------------------------------------------------

-- 1ª definición (Por recursión)
elevaSumaR :: [Integer] -> [Integer] -> Integer
elevaSumaR [] _ = 0
elevaSumaR _ [] = 0
elevaSumaR (x:xs) (y:ys) = x^y + elevaSumaR xs ys 

-- 2ª definición (Por plegado a la derecha)
elevaSumaPR :: [Integer] -> [Integer] -> Integer
elevaSumaPR xs ys = foldr f 0 (zip xs ys)
    where f (a,b) ys = a^b + ys

-- 3ª definición (Por comprensión)
elevaSumaC :: [Integer] -> [Integer] -> Integer
elevaSumaC xs ys = sum [a^b | (a,b) <- zip xs ys]
 
-- 4ª definición (Con orden superior)
elevaSumaS  :: [Integer] -> [Integer] -> Integer
elevaSumaS xs ys = sum (map f (zip xs ys))
  where f (x,y) = x^y

-- 5ª definición (Con acumulador)
elevaSumaA :: [Integer] -> [Integer] -> Integer
elevaSumaA xs ys = aux (zip xs ys) 0
  where aux [] ac = ac
        aux ((a,b):ps) ac = aux ps (a^b + ac)

-- 6ª definición (Por plegado a la izquierda)
elevaSumaPL :: [Integer] -> [Integer] -> Integer
elevaSumaPL xs ys = foldl f 0 (zip xs ys)
    where f ac (a,b) = ac + (a^b)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Una lista de números es prima si la mayoría de sus
-- elementos son primos.
-- 
-- Definir la función
--    listaPrima:: [Int] -> Bool
-- tal que (listaPrima xs) se verifica si xs es prima. Por ejemplo,
--    listaPrima [3,2,6,1,5,11,19] == True
--    listaPrima [80,12,7,8,3]     == False
--    listaPrima [1,7,8,4]         ==  False
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
listaPrimaC :: [Int] -> Bool
listaPrimaC xs =
  length [x | x <- xs, esPrimo x] > length xs `div` 2

esPrimo :: Int -> Bool
esPrimo n = factores n == [1,n]

factores :: Int -> [Int]
factores n = [x | x <-[1 .. n], mod n x == 0]

-- 2ª definición (con filter)
listaPrimaS :: [Int] -> Bool
listaPrimaS xs =
  length (filter esPrimo xs) > length xs `div` 2

-- 3ª definición (usando recursión):
listaPrimaR :: [Int] -> Bool
listaPrimaR xs = length (aux xs) > length xs `div` 2
  where  aux [] = []
         aux (x:xs) | esPrimo x = x : aux xs
                    | otherwise = aux xs

-- 4ª definición (con plegado a la derecha):
listaPrimaPR :: [Int] -> Bool
listaPrimaPR xs = length (foldr f [] xs) > length xs `div` 2
  where f x ys | esPrimo x = x:ys 
               | otherwise = ys

-- 5ª definición (usando acumuladores)
listaPrimaA :: [Int] -> Bool
listaPrimaA xs = length (aux xs []) > length xs `div` 2
  where aux [] ac = ac
        aux (y:ys) ac | esPrimo y = aux ys (ac ++ [y]) 
                      | otherwise = aux ys ac

-- 6ª definición (usando plegado a la izquierda):
listaPrimaPL :: [Int] -> Bool
listaPrimaPL xs = length (foldl g [] xs) > length xs `div` 2
  where g acum prim | esPrimo prim = acum ++ [prim]
                    | otherwise    = acum
