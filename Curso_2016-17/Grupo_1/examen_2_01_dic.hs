import Data.List 
import Data.Numbers.Primes
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    menorFactorialDiv :: Integer -> Integer
-- tal que (menorFactorialDiv n) es el menor m tal que n divide a m!
-- Por ejemplo,
--    menorFactorialDiv 10    == 5
--    menorFactorialDiv 25    == 10
--    menorFactorialDiv 10000 == 20
--    menorFactorialDiv1 1993 == 1993
-- 
-- ¿Cuáles son los números n tales que (menorFactorialDiv n) == n?
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

menorFactorialDiv1 :: Integer -> Integer
menorFactorialDiv1 n =
  head [x | x <- [1..], product [1..x] `mod` n == 0]

-- Los números n tales que
--    (menorFactorialDiv n) == n
-- son el 1, el 4 y los números primos, lo que permite la segunda
-- definición. 

-- 2ª solución
-- ===========

menorFactorialDiv2 :: Integer -> Integer
menorFactorialDiv2 n
  | isPrime n = n
  | otherwise = head [x | x <- [1..], product [1..x] `mod` n == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Dado un entero positivo n, consideremos la suma de los
-- cuadrados de sus divisores. Por ejemplo,  
--    f(10) = 1 + 4 + 25 + 100 = 130
--    f(42) = 1 + 4 +  9 +  36 + 49 + 196 + 441 + 1764 = 2500
-- Decimos que n es un número "sumaCuadradoPerfecto" si f(n) es un
-- cuadrado perfecto. En los ejemplos anteriores, 42 es
-- sumaCuadradoPerfecto y 10 no lo es. 
-- 
-- Definir la función 
--    sumaCuadradoPerfecto:: Int -> Bool
-- tal que (sumaCuadradoPerfecto x) se verifica si x es un número es
-- sumaCuadradoPerfecto. Por ejemplo, 
--    sumaCuadradoPerfecto 42  ==  True
--    sumaCuadradoPerfecto 10  ==  False
--    sumaCuadradoPerfecto 246 ==  True
-- 
-- Calcular todos los números sumaCuadradoPerfectos de tres cifras.
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

sumaCuadradoPerfecto :: Int -> Bool
sumaCuadradoPerfecto n =
  esCuadrado (sum (map (^2) (divisores n)))

-- 2ª definición
-- =============

sumaCuadradoPerfecto2 :: Int -> Bool
sumaCuadradoPerfecto2 =
  esCuadrado . sum . map (^2) . divisores

-- (esCuadrado n) se verifica si n es un cuadrado perfecto. Por ejemplo,  
--    esCuadrado 36  ==  True
--    esCuadrado 40  ==  False
esCuadrado :: Int -> Bool
esCuadrado n = y^2 == n
  where y = floor (sqrt (fromIntegral n))

-- (divisores n) es la lista de los divisores de n. Por ejemplo,
--    divisores 36  ==  [1,2,3,4,6,9,12,18,36]
divisores :: Int -> [Int]        
divisores n = [x | x <- [1..n], rem n x == 0] 

-- El cálculo es
--    ghci> filter sumaCuadradoPerfecto [100..999]
--    [246,287,728]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Un lista de números enteros se llama alternada si sus
-- elementos son alternativamente par/impar o impar/par.
-- 
-- Definir la función
--    alternada :: [Int] -> Bool
-- tal que (alternada xs) se verifica si xs es una lista alternada. Por
-- ejemplo,
--    alternada [1,2,3]     == True
--    alternada [1,2,3,4]   == True
--    alternada [8,1,2,3,4] == True
--    alternada [8,1,2,3]   == True
--    alternada [8]         == True
--    alternada [7]         == True
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

alternada1 :: [Int] -> Bool
alternada1 (x:y:xs)
  | even x    = odd y  && alternada1 (y:xs)
  | otherwise = even y && alternada1 (y:xs)
alternada1 _ = True

-- 2ª solución
-- ===========

alternada2 :: [Int] -> Bool
alternada2 xs = all odd (zipWith (+) xs (tail xs))

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Generalizando, una lista es alternada respecto de un
-- predicado p si sus elementos verifican alternativamente el predicado
-- p.
-- 
-- Definir la función
--    alternadaG :: (a -> Bool) -> [a] -> Bool
-- tal que (alternadaG p xs) compruebe se xs es una lista alternada
-- respecto de p. Por ejemplo,
--    alternadaG (>0) [-2,1,3,-9,2]  == False
--    alternadaG (>0) [-2,1,-3,9,-2] == True
--    alternadaG (<0) [-2,1,-3,9,-2] == True
--    alternadaG even [8,1,2,3]      == True
-- ---------------------------------------------------------------------

alternadaG :: (a -> Bool) -> [a] -> Bool
alternadaG p (x:y:xs)
  | p x       = not (p y) && alternadaG p (y:xs)
  | otherwise = p y && alternadaG p (y:xs)
alternadaG _ _ = True    

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Redefinir la función alternada usando alternadaG y
-- comprobar con QuickCheck que ambas definiciones coinciden.
-- ---------------------------------------------------------------------

alternada3 :: [Int] -> Bool
alternada3 = alternadaG even

-- La propiedad es
propAlternada :: [Int] -> Bool
propAlternada xs = alternada1 xs == alternada3 xs

-- Su comprobación es
--    ghci> quickCheck propAlternada
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4. Una lista de listas es engarzada si el último elemento
-- de cada lista coincide con el primero de la siguiente.
-- 
-- Definir la función
--    engarzada :: Eq a => [[a]] -> Bool
-- tal que (engarzada xss) se verifica si xss es una lista engarzada.
-- Por ejemplo,
--    engarzada [[1,2,3], [3,5], [5,9,0]] == True
--    engarzada [[1,4,5], [5,0], [1,0]]   == False
--    engarzada [[1,4,5], [], [1,0]]      == False
--    engarzada [[2]]                     == True
-- ---------------------------------------------------------------------

-- 1ª solución:
engarzada :: Eq a => [[a]] -> Bool
engarzada (xs:ys:xss) =
     not (null xs) && not (null ys) && last xs == head ys
  && engarzada (ys:xss)
engarzada _ = True

-- 2ª solución:
engarzada2 :: Eq a => [[a]] -> Bool
engarzada2 (xs:ys:xss) =
  and [ not (null xs)
      , not (null ys)
      , last xs == head ys
      , engarzada2 (ys:xss) ]
engarzada2 _ = True

-- 3ª solución:
engarzada3 :: Eq a => [[a]] -> Bool
engarzada3 xss =
  and [ not (null xs) && not (null ys) && last xs == head ys
      | (xs,ys) <- zip xss (tail xss)]

-- 4ª solución:
engarzada4 :: Eq a => [[a]] -> Bool
engarzada4 xss =
  all engarzados (zip xss (tail xss))
  where engarzados (xs,ys) =
          not (null xs) && not (null ys) && last xs == head ys

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. La sucesión de números de Pell se construye de la
-- forma siguiente:
--    P(0) = 0
--    P(1) = 1
--    P(n) = 2*P(n-1) + P(n-2)
-- Sus primeros términos son
--    0, 1, 2, 5, 12, 29, 70, 169, 408, 985, 2378, 5741, 13860, 33461,...
--
-- Definir la constante
--   sucPell :: [Integer]
-- tal que sucPell es la lista formada por los términos de la
-- sucesión. Por ejemplo, 
--    ghci> take 15 sucPell
--    [0,1,2,5,12,29,70,169,408,985,2378,5741,13860,33461,80782]
-- ---------------------------------------------------------------------

sucPell :: [Integer]
sucPell = 0 : 1 : zipWith (+) sucPell (map (2*) (tail sucPell))

-- ---------------------------------------------------------------------
-- Ejercicio 5.2.  Definir la función
--    pell :: Int -> Integer
-- tal que (pell n) es el n-ésimo número de Pell. Por ejemplo,
--    pell 0   == 0
--    pell 10  == 2378
--    pell 100 == 66992092050551637663438906713182313772
-- ---------------------------------------------------------------------

pell :: Int -> Integer
pell n = sucPell !! n

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Los números de Pell verifican que los cocientes
--    ((P(m-1) + P(m))/P(m)
-- proporcionan una aproximación de la raiz cuadrada de 2.
-- 
-- Definir la función
--    aproxima :: Int -> [Double]
-- tal que (aproxima n) es la lista de las sucesivas aproximaciones
-- a la raíz de 2, desde m = 1 hasta n. Por ejemplo,
--    aproxima 3  == [1.0,1.5,1.4]
--    aproxima 4  == [1.0,1.5,1.4,1.4166666666666667]
--    aproxima 10 ==  [1.0,1.5,1.4,
--                     1.4166666666666667,1.4137931034482758,
--                     1.4142857142857144,1.4142011834319526,
--                     1.4142156862745099,1.4142131979695431,
--                     1.4142136248948696]
-- ---------------------------------------------------------------------

aproxima :: Int -> [Double]
aproxima n = map aprox2 [1..n]
  where aprox2 n =
          fromIntegral (pell (n-1) + pell  n) / fromIntegral (pell n)

