-- Informática: 3º examen de evaluación continua (23 de enero de 2014)
-- ---------------------------------------------------------------------

-- Puntuación: Cada uno de los 4 ejercicios vale 2.5 puntos.

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función
--    divisoresConUno :: Integer -> Bool
-- tal que (divisoresConUno n) se verifica si todos sus divisores
-- contienen el dígito 1. Por ejemplo,
--    divisoresConUno 671  ==  True
--    divisoresConUno 100  ==  False
-- ya que los divisores de 671 son 1, 11, 61 y 671 y todos contienen el
-- número 1; en cambio, 25 es un divisor de 100 que no contiene el
-- dígito 1. 
-- ---------------------------------------------------------------------

divisoresConUno :: Integer -> Bool
divisoresConUno n = all contieneUno (divisores n)

-- 2ª definición (sin all) 
divisoresConUno2 :: Integer -> Bool
divisoresConUno2 n = and [contieneUno x | x <- divisores n]

-- 3ª definición (por recursión)
divisoresConUno3 :: Integer -> Bool
divisoresConUno3 n = aux (divisores n)
    where aux []     = True
          aux (x:xs) = contieneUno x && aux xs

-- 4ª definición (por plegado)
divisoresConUno4 :: Integer -> Bool
divisoresConUno4 n = foldr f True (divisores n)
    where f x y = contieneUno x && y

-- 5ª definición (por plegado y lambda)
divisoresConUno5 :: Integer -> Bool
divisoresConUno5 n = 
    foldr (\x y -> contieneUno x && y) True (divisores n)

-- (divisores n) es la lista de los divisores de n. Por ejemplo,
--    divisores 12  ==  [1,2,3,4,6,12]
divisores :: Integer -> [Integer]
divisores n = [x | x <- [1..n], rem n x == 0]

-- (contienUno n) se verifica si n contiene el dígito 1. Por ejemplo, 
--    contieneUno 214  ==  True
--    contieneUno 234  ==  False
contieneUno :: Integer -> Bool
contieneUno n = elem '1' (show n)

-- 2ª definición (por recursión sin show)
contieneUno2 :: Integer -> Bool
contieneUno2 1 = True
contieneUno2 n | n < 10          = False
               | n `rem` 10 == 1 = True
               | otherwise       = contieneUno2 (n `div` 10)

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. ¿Cuál será el próximo año en el que todos sus divisores
-- contienen el dígito 1? ¿y el anterior?
-- ---------------------------------------------------------------------

-- El cálculo es
--    ghci> head [n | n <- [2014..], divisoresConUno n]
--    2017
--    ghci> head [n | n <- [2014,2013..], divisoresConUno n]
--    2011

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Un elemento de una lista es permanente si ninguno de
-- los siguientes es mayor que él. 
-- 
-- Definir, por recursión, la función 
--    permanentesR :: [Int] -> [Int]
-- tal que (permanentesR xs) es la lista de los elementos permanentes de
-- xs. Por ejemplo,
--    permanentesR [80,1,7,8,4]  ==  [80,8,4]
-- ---------------------------------------------------------------------

-- 1ª definición:
permanentesR :: [Int] -> [Int]
permanentesR [] = []
permanentesR (x:xs) | x == maximum (x:xs) = x:permanentesR xs
                    | otherwise           = permanentesR xs

-- 2ª definición (sin usar maximum):
permanentesR2 :: [Int] -> [Int]
permanentesR2 [] = []
permanentesR2 (x:xs) | and [x>=y|y<-xs] = x:permanentesR2 xs
                     | otherwise        = permanentesR2 xs

-- Nota: Comparación de eficiencia
--    ghci> let xs = [1..1000] in last (permanentesR (xs ++ reverse xs))
--    1
--    (0.22 secs, 41105812 bytes)
--    ghci> let xs = [1..1000] in last (permanentesR2 (xs ++ reverse xs))
--    1
--    (0.96 secs, 31421308 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, por plegado, la función 
--    permanentesP :: [Int] -> [Int]
-- tal que (permanentesP xs) es la lista de los elementos permanentes de
-- xs. Por ejemplo,
--    permanentesP [80,1,7,8,4]  ==  [80,8,4]
-- ---------------------------------------------------------------------

-- 1ª definición:
permanentesP :: [Int] -> [Int]
permanentesP = foldr f []
    where f x ys | x == maximum (x:ys) = x:ys
                 | otherwise           = ys

-- 2ª definición:
permanentesP2 :: [Int] -> [Int]
permanentesP2 xs = foldl f [] (reverse xs)
    where f ac x | x == maximum (x:ac) = x:ac
                 | otherwise           = ac

-- Nota: Comparación de eficiencia
--    ghci> let xs = [1..1000] in last (permanentesP (xs ++ reverse xs))
--    1
--    (0.22 secs, 52622056 bytes)
--    ghci> let xs = [1..1000] in last (permanentesP2 (xs ++ reverse xs))
--    1
--    (0.23 secs, 52918324 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    especial :: Int -> [[Int]] -> Bool
-- tal que (especial k xss) se verifica si cada uno de los diez dígitos
-- 0, 1, 2,..., 9 aparece k veces entre todas las listas de xss. Por
-- ejemplo, 
--    especial 1 [[12,40],[5,79,86,3]]                          == True
--    especial 2 [[107,32,89],[58,76,94],[63,120,45]]           == True
--    especial 3 [[1329,276,996],[534,867,1200],[738,1458,405]] == True
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
especial :: Int -> [[Int]] -> Bool
especial k xss =
    sort (concat [show n | xs <- xss, n <- xs])
    == concat [replicate k d | d <- ['0'..'9']]

-- 2ª definición (con map)
especial2 :: Int -> [[Int]] -> Bool
especial2 k xss = 
    sort (concat (concat (map (map cifras) xss))) 
    == concat [replicate k d | d <- [0..9]]

cifras:: Int -> [Int]
cifras n = [read [x] | x <-show n]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función 
--    primosConsecutivosConIgualFinal :: Int -> [Integer]
-- tal que (primosConsecutivosConIgualFinal n) es la lista de los
-- primeros n primos consecutivos que terminan en el  mismo dígito. Por
-- ejemplo, 
--    primosConsecutivosConIgualFinal 2 == [139, 149]
--    primosConsecutivosConIgualFinal 3 == [1627, 1637, 1657]
-- ---------------------------------------------------------------------

primosConsecutivosConIgualFinal :: Int -> [Integer]
primosConsecutivosConIgualFinal n = consecutivosConPropiedad p n primos
    where p []     = True
          p (x:xs) = and [r == rem y 10 | y <- xs]
              where r = rem x 10

-- (consecutivosConPropiedad p n xs) es la lista con los n primeros
-- elementos consecutivos de zs que verifican la propiedad p. Por
-- ejemplo, 
--    ghci> consecutivosConPropiedad (\xs -> sum xs > 20) 2 [5,2,1,17,4,25]
--    [17,4]
consecutivosConPropiedad :: ([a] -> Bool) -> Int -> [a] -> [a]
consecutivosConPropiedad p n zs = 
    head [xs | xs <- [take n ys | ys <- tails zs], p xs]

-- primos es la lista de los números primos. Por ejemplo,
--    ghci> take 20 primos
--    [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71]
primos :: [Integer]
primos = [n | n <- 2:[3,5..], primo n]

-- (primo n) se verifica si n es un número primo. Por ejemplo,
--    primo 7  ==  True
--    primo 8  ==  False
primo :: Integer -> Bool
primo n = [x | x <- [1..n], rem n x == 0] == [1,n]

