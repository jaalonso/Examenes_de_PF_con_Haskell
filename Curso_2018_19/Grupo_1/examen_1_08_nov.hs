-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 1º examen de evaluación continua (8 de noviembre de 2017)
-- ---------------------------------------------------------------------

-- -----------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    porTramos :: (Num a, Ord a) => [a] -> Bool
-- tal que (porTramos xs) se verifica si la diferencia entre numeros
-- consecutivos de xs es siempre, en valor absoluto, mayor que 2. Por
-- ejemplo, 
--    porTramos [1,5,8,23,5] == True
--    porTramos [3,6,8,1]    == False
--    porTramos [-5,-2,4 ]   == True
--    porTramos [5,2]        == True
-- ---------------------------------------------------------------------

-- 1ª definición
porTramos :: (Num a, Ord a) => [a] -> Bool
porTramos xs = and [abs (x-y) > 2 | (x,y) <- zip xs (tail xs)]

-- 2ª definición
porTramos2 :: (Num a, Ord a) => [a] -> Bool
porTramos2 []       = True
porTramos2 [_]      = True
porTramos2 (x:y:xs) = abs (x-y) > 2 && porTramos2 (y:xs)

-- 3ª definición
porTramos3 :: (Num a, Ord a) => [a] -> Bool
porTramos3 (x:y:xs) = abs (x-y) > 3 && porTramos3 (y:xs)
porTramos3 _        = True

-- ----------------------------------------------------------------------
-- Ejercicio 2. Definir la funcion
--    sonMenores :: Ord a => [a] -> [a] -> Int
-- tal que (sonMenores xs ys) es el número de elementos de xs son
-- menores que los que ocupan la misma posicion en ys hasta el primero
-- que no lo sea. Por ejemplo,
--    sonMenores "prueba" "suspenso"       == 2
--    sonMenores [1,2,3,4,5] [6,5,4,3,8,9] == 3
-- ---------------------------------------------------------------------

-- 1ª definición
sonMenores :: Ord a => [a] -> [a] -> Int
sonMenores (x:xs) (y:ys)
  | x < y    = 1 + sonMenores xs ys
  |otherwise = 0
sonMenores _ _ = 0

-- 2ª definición
sonMenores2 :: Ord a => [a] -> [a] -> Int
sonMenores2 xs ys =
  length (takeWhile (\(x,y) -> x < y) (zip xs ys))

-- ----------------------------------------------------------------------
-- Ejercicio 3.1. Se dice que un número entero positivo es raro si al
-- sumar cada una de sus cifras elevadas al numero de cifras que lo
-- forman, obtenemos el propio numero. Por ejemplo, el 153 (que tiene 3
-- cifras) es raro ya que 153 es 1^3 + 5^3 + 3^3. 
-- 
-- Definir la función
--    esRaro :: Integer -> Bool
-- tal que (esRaro x) se verifica si x es raro. Por ejemplo,
--    esRaro 3    == True
--    esRaro 153  == True
--    esRaro 12   == False
--    esRaro 8208 == True
-- ---------------------------------------------------------------------

esRaro :: Integer -> Bool
esRaro x = sum [y^n | y <- ds] == x
  where ds = digitos x
        n  = length ds

digitos :: Integer -> [Integer]
digitos x = [read [y] | y <- show x]

-- ----------------------------------------------------------------------
-- Ejercicio 3.2. Definir la funcion
--    primerosRaros :: Int -> [Integer]
-- tal que (primerosRaros n) que es la lista de los n primeros números
-- raros. Por ejemplo,
--    primerosRaros 15 == [1,2,3,4,5,6,7,8,9,153,370,371,407,1634,8208]
-- ---------------------------------------------------------------------

primerosRaros :: Int -> [Integer]
primerosRaros n = take n [x | x <- [1 ..], esRaro x]

-- ----------------------------------------------------------------------
-- Ejercicio 4. Definir la funcion
--    quitaCentro :: [a] -> [a]
-- tal que (quitaCentro xs) es la lista obtenida eliminando en xs su
-- elemento central si xs es de longitud impar y sus dos elementos
-- centrales si es de longitud par. Por ejemplo,
--    quitaCentro [1,2,3,4]             == [1,4]
--    quitaCentro [1,2,3]               == [1,3]
--    quitaCentro "examen1"             == "exaen1"
--    quitaCentro [[1,2],[3,2,5],[5,4]] == [[1,2],[5,4]]
--    quitaCentro [6]                   == []
-- ---------------------------------------------------------------------

-- 1ª solución
quitaCentro :: [a] -> [a]
quitaCentro [] = []
quitaCentro xs
  | even n    = init (take m xs) ++ drop (m+1) xs
  | otherwise = take m xs ++ drop (m+1) xs
  where n = length xs
        m = n `div` 2

-- 2ª solución
quitaCentro2 :: [a] -> [a]
quitaCentro2 xs = 
  [x | (x,y) <- zip xs [1..], y `notElem` numeroscentro xs]

numeroscentro :: [a] -> [Int]
numeroscentro xs | even n    = [m,m+1]
                 | otherwise = [m+1]
  where n = length xs
        m = n `div` 2

-- 2ª solución
quitaCentro3 :: [a] -> [a]
quitaCentro3 [] = []
quitaCentro3 xs
  | even n    = init as ++ bs
  | otherwise = as ++ bs
  where n         = length xs
        m         = n `div` 2
        (as,_:bs) = splitAt m xs
