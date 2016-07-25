-- Informática: 3º examen de evaluación continua (25 de enero de 2016)
-- ---------------------------------------------------------------------

-- Puntuación: Cada uno de los 4 ejercicios vale 2.5 puntos.

import Test.QuickCheck
import Data.Numbers.Primes

-- ---------------------------------------------------------------------
-- Ejercicio 1. Los máximos y mínimos de una función son sus valores
-- óptimos respecto de las relaciones > y <, respectivamente. Por
-- ejemplo, para la lista xs = ["ab","c","de","f"], la función longitud
-- alcanza sus valores máximos (es decir, óptimos respecto >) en "ab" y
-- "de" (que son los elementos de xs de mayor longitud) y alcanza sus
-- valores mínimos (es decir, óptimos respecto <) en "c" y "f" (que son
-- los elementos de xs de menor longitud).  
-- 
-- Definir la función
--    optimos :: Eq b => (b -> b -> Bool) -> (a -> b) -> [a] -> [a]
-- tal que (optimos r f xs) es la lista de los elementos de xs donde la
-- función f alcanza sus valores óptimos respecto de la relación r. Por
-- ejemplo, 
--    optimos (>) length ["ab","c","de","f"]  ==  ["ab","de"]
--    optimos (<) length ["ab","c","de","f"]  ==  ["c","f"]
-- ---------------------------------------------------------------------

optimos :: Eq a => (b -> b -> Bool) -> (a -> b) -> [a] -> [a]
optimos r f xs = 
    [x | x <- xs, null [y | y <- xs, x /= y, r (f y) (f x)]]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Los árboles se pueden representar mediante el siguiente
-- tipo de datos 
--    data Arbol a = N a [Arbol a]
--                   deriving Show
-- Por ejemplo, los árboles
--      1               3
--     / \             /|\ 
--    2   3           / | \
--        |          5  4  7
--        4          |     /\ 
--                   6    2  1
-- se representan por
--    ej1, ej2 :: Arbol Int
--    ej1 = N 1 [N 2 [],N 3 [N 4 []]]
--    ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]
-- 
-- Definir la función
--    mayorProducto :: (Ord a, Num a) => Arbol a -> a
-- tal que (mayorProducto a) es el mayor producto de las ramas del árbol
-- a. Por ejemplo,
--    ghci> mayorProducto (N 1 [N 2 [], N  3 []])
--    3
--    ghci> mayorProducto (N 1 [N 8 [], N  4 [N 3 []]])
--    12
--    ghci> mayorProducto (N 1 [N 2 [],N 3 [N 4 []]])
--    12
--    ghci> mayorProducto (N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]])
--    90
-- ---------------------------------------------------------------------

data Arbol a = N a [Arbol a]
               deriving Show

-- 1º definición
mayorProducto1 :: (Ord a, Num a) => Arbol a -> a
mayorProducto1 (N x []) = x
mayorProducto1 (N x xs) = x * maximum [mayorProducto1 a | a <- xs]

-- Se puede usar map en lugar de comprensión:
mayorProducto1a :: (Ord a, Num a) => Arbol a -> a
mayorProducto1a (N x []) = x
mayorProducto1a (N x xs) = x * maximum (map mayorProducto1a xs)

-- 2ª definición
mayorProducto2 :: (Ord a, Num a) => Arbol a -> a
mayorProducto2 a = maximum [product xs | xs <- ramas a]

-- (ramas a) es la lista de las ramas del árbol a. Por ejemplo,
--    ghci> ramas (N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]])
--    [[3,5,6],[3,4],[3,7,2],[3,7,1]]
ramas :: Arbol b -> [[b]]
ramas (N x []) = [[x]]
ramas (N x as) = [x : xs | a <- as, xs <- ramas a]

-- En la definición de mayorProducto2 se puede usar map en lugar de
-- comprensión. 
mayorProducto2a :: (Ord a, Num a) => Arbol a -> a
mayorProducto2a a = maximum (map product (ramas a))

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la sucesión
--    sumasDeDosPrimos :: [Integer]
-- cuyos elementos son los números que se pueden escribir como suma de
-- dos números primos. Por ejemplo,  
--    ghci> take 20 sumasDeDosPrimos
--    [4,5,6,7,8,9,10,12,13,14,15,16,18,19,20,21,22,24,25,26]
--    ghci> sumasDeDosPrimos !! 2016
--    3146
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

sumasDeDosPrimos1 :: [Integer]
sumasDeDosPrimos1 =
    [n | n <- [1..], not (null (sumaDeDosPrimos1 n))]

-- (sumasDeDosPrimos1 n) es la lista de pares de primos cuya suma es
-- n. Por ejemplo,
--    sumaDeDosPrimos  9  ==  [(2,7),(7,2)]
--    sumaDeDosPrimos 16  ==  [(3,13),(5,11),(11,5),(13,3)]
--    sumaDeDosPrimos 17  ==  []
sumaDeDosPrimos1 :: Integer -> [(Integer,Integer)]
sumaDeDosPrimos1 n = 
    [(x,n-x) | x <- primosN, isPrime (n-x)]
    where primosN = takeWhile (< n) primes

-- 2ª definición
-- =============

sumasDeDosPrimos2 :: [Integer]
sumasDeDosPrimos2 =
    [n | n <- [1..], not (null (sumaDeDosPrimos2 n))]

-- (sumasDeDosPrimos2 n) es la lista de pares (x,y) de primos cuya suma
-- es n y tales que x <= y. Por ejemplo,
--    sumaDeDosPrimos2  9  ==  [(2,7)]
--    sumaDeDosPrimos2 16  ==  [(3,13),(5,11)]
--    sumaDeDosPrimos2 17  ==  []
sumaDeDosPrimos2 :: Integer -> [(Integer,Integer)]
sumaDeDosPrimos2 n = 
    [(x,n-x) | x <- primosN, isPrime (n-x)]
    where primosN = takeWhile (<= (n `div` 2)) primes

-- 3ª definición
-- =============

sumasDeDosPrimos3 :: [Integer]
sumasDeDosPrimos3 = filter esSumaDeDosPrimos3 [4..]

-- (esSumaDeDosPrimos3 n) se verifica si n es suma de dos primos. Por
-- ejemplo, 
--    esSumaDeDosPrimos3  9  ==  True
--    esSumaDeDosPrimos3 16  ==  True
--    esSumaDeDosPrimos3 17  ==  False
esSumaDeDosPrimos3 :: Integer -> Bool
esSumaDeDosPrimos3 n
    | odd n     = isPrime (n-2)
    | otherwise = any isPrime [n-x | x <- takeWhile (<= (n `div` 2)) primes]

-- 4ª definición
-- =============

-- Usando la conjetura de Goldbach que dice que "Todo número par mayor
-- que 2 puede escribirse como suma de dos números primos" .

sumasDeDosPrimos4 :: [Integer]
sumasDeDosPrimos4 = filter esSumaDeDosPrimos4 [4..]

-- (esSumaDeDosPrimos4 n) se verifica si n es suma de dos primos. Por
-- ejemplo, 
--    esSumaDeDosPrimos4  9  ==  True
--    esSumaDeDosPrimos4 16  ==  True
--    esSumaDeDosPrimos4 17  ==  False
esSumaDeDosPrimos4 :: Integer -> Bool
esSumaDeDosPrimos4 n = even n || isPrime (n-2)

-- Comparación de eficiencia
-- =========================

--    ghci> sumasDeDosPrimos1 !! 3000
--    4731
--    (6.66 secs, 3,278,830,304 bytes)
--    ghci> sumasDeDosPrimos2 !! 3000
--    4731
--    (3.69 secs, 1,873,984,088 bytes)
--    ghci> sumasDeDosPrimos3 !! 3000
--    4731
--    (0.35 secs, 175,974,016 bytes)
--    ghci> sumasDeDosPrimos4 !! 3000
--    4731
--    (0.07 secs, 18,396,432 bytes)
--    
--    ghci> sumasDeDosPrimos3 !! 30000
--    49785
--    (6.65 secs, 3,785,736,416 bytes)
--    ghci> sumasDeDosPrimos4 !! 30000
--    49785
--    (1.06 secs, 590,767,736 bytes)

-- En lo que sigue usaremos la 1ª definición
sumasDeDosPrimos :: [Integer]
sumasDeDosPrimos = sumasDeDosPrimos1

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir el procedimiento
--    termino :: IO ()
-- que pregunta por una posición y escribe el término de la sucesión
-- sumasDeDosPrimos en dicha posición. Por ejemplo,
--    ghci> termino
--    Escribe la posicion: 5
--    El termino en la posicion 5 es 9
--    ghci> termino
--    Escribe la posicion: 19
--    El termino en la posicion 19 es 26
-- ---------------------------------------------------------------------

termino :: IO ()
termino = do
  putStr "Escribe la posicion: "
  xs <- getLine
  let n = read xs
  putStr "El termino en la posicion "
  putStr xs
  putStr " es "
  putStrLn (show (sumasDeDosPrimos !! n))

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Una función f entre dos conjuntos A e B se puede
-- representar mediante una lista de pares de AxB tales que para cada
-- elemento a de A existe un único elemento b de B tal que (a,b)
-- pertenece a f. Por ejemplo,  
--    + [(1,2),(3,6)] es una función de [1,3] en [2,4,6];
--    + [(1,2)] no es una función de [1,3] en [2,4,6], porque no tiene
--      ningún par cuyo primer elemento sea igual a 3;
--    + [(1,2),(3,6),(1,4)] no es una función porque hay dos pares
--      distintos cuya primera coordenada es 1.
-- 
-- Definir la función
--    funciones :: [a] -> [a] -> [[(a,a)]]
-- tal que (funciones xs ys) es el conjunto de las funciones de xs en
-- ys. Por ejemplo,
--    ghci> funciones [] [2,4,6]
--    [[]]
--    ghci> funciones [3] [2,4,6]
--    [[(3,2)],[(3,4)],[(3,6)]]
--    ghci> funciones [1,3] [2,4,6]
--    [[(1,2),(3,2)], [(1,2),(3,4)], [(1,2),(3,6)], [(1,4),(3,2)], [(1,4),(3,4)],
--     [(1,4),(3,6)], [(1,6),(3,2)], [(1,6),(3,4)], [(1,6),(3,6)]]
-- ---------------------------------------------------------------------

funciones :: [a] -> [a] -> [[(a,a)]]
funciones [] _      = [[]]
funciones [x] ys    = [[(x,y)] | y <- ys]
funciones (x:xs) ys = [((x,y):f) | y <- ys, f <- fs]
    where fs = funciones xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Comprobar con QuickCheck que si xs es un conjunto con n
-- elementos e ys un conjunto con m elementos, entonces (funciones xs ys)
-- tiene m^n elementos.
--
-- Nota. Al hacer la comprobación limitar el tamaño de las pruebas como
-- se indica a continuación
--    ghci> quickCheckWith (stdArgs {maxSize=7}) prop_funciones
--    +++ OK, passed 100 tests.
-- ---------------------------------------------------------------------

prop_funciones :: [Int] -> [Int] -> Bool
prop_funciones xs ys =
    length (funciones xs ys) == (length ys)^(length xs)
