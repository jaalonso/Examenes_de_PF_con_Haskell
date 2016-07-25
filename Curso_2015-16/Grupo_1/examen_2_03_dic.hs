-- Informática (1º del Grado en Matemáticas)
-- 2º examen de evaluación continua (3 de diciembre de 2015)
-- ---------------------------------------------------------------------

import Data.List
import Data.Numbers.Primes
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir una función 
--    sumaCuadradosDivisores1 :: Integer -> Integer
-- que calcule la suma de los cuadrados de los divisores de n. Por
-- ejemplo:
--    sumaCuadradosDivisores1 6  ==  50
-- ---------------------------------------------------------------------

sumaCuadradosDivisores1 :: Integer -> Integer
sumaCuadradosDivisores1 n = sum [x^2 | x <- divisores n]

sumaCuadradosDivisores1' :: Integer -> Integer
sumaCuadradosDivisores1' = sum . map (^2) . divisores

-- (divisores n) es la lista de los divisores de n. 
divisores :: Integer -> [Integer]
divisores n = [x | x <- [1..n], n `rem` x == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. La suma de los cuadrados de los divisores de un número
-- se puede calcular a partir de su factorización prima. En efecto, si
-- la factorización prima de n es 
--    a^x*b^y*...*c^z
-- entonces, la suma de los divisores de n es
--   (1+a+a^2+...+a^x) * (1+b+b^2+...+b^y) *...* (1+c+c^2+...+c^z)
-- es decir,
--   ((a^(x+1)-1)/(a-1)) * ((b^(y+1)-1)/(b-1)) *...* ((c^(z+1)-1)/(c-1))
-- Por tanto, la suma de sus cuadrados de los divisores de n
--   ((a^(2*(x+1))-1)/(a^2-1)) * ((b^(2*(y+1))-1)/(b^2-1)) *...* 
-- 
-- Definir, a partir de la nota anterior, la función
--    sumaCuadradosDivisores2 :: Int -> Integer
-- tal que (sumaCuadradosDivisores2 n) es la suma de los cuadrados de  
-- los divisores de n. Por ejemplo,
--    sumaCuadradosDivisores2 6  ==  50
-- ---------------------------------------------------------------------

sumaCuadradosDivisores2 :: Integer -> Integer
sumaCuadradosDivisores2 n =
    product [(a^(2*(m+1))-1) `div` (a^2-1) | (a,m) <- factorizacion n]

-- (factorizacion n) es la factorización prima de n. 
factorizacion :: Integer -> [(Integer,Integer)]
factorizacion n = 
    [(head xs, genericLength xs) | xs <- group (primeFactors n)]

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comparar las estadísticas del cálculo de las
-- siguientes expresiones 
--    sumaCuadradosDivisores1 1000000
--    sumaCuadradosDivisores2 1000000
-- El cálculo es
--    ghci> sumaCuadradosDivisores1 1000000
--    1388804117611
--    (2.91 secs, 104321520 bytes)
--    ghci> sumaCuadradosDivisores2 1000000
--    1388804117611
--    (0.01 secs, 550672 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2: Definir la función
--    cerosDelFactorial :: Integer -> Integer
-- tal que (cerosDelFactorial n) es el número de ceros en que termina el 
-- factorial de n. Por ejemplo,
--    cerosDelFactorial 24                           ==  4
--    cerosDelFactorial 25                           ==  6
--    length (show (cerosDelFactorial (1234^5678)))  ==  17552
-- ---------------------------------------------------------------------
 
-- 1ª definición
-- =============
 
cerosDelFactorial1 :: Integer -> Integer
cerosDelFactorial1 n = ceros (factorial n)
 
-- (factorial n) es el factorial n. Por ejemplo,
--    factorial 3  ==  6
factorial :: Integer -> Integer
factorial n = product [1..n]
 
-- (ceros n) es el número de ceros en los que termina el número n. Por
-- ejemplo, 
--    ceros 320000  ==  4
ceros :: Integer -> Integer
ceros n | rem n 10 /= 0 = 0
        | otherwise     = 1 + ceros (div n 10)
 
-- 2ª definición
-- =============
 
cerosDelFactorial2 :: Integer -> Integer
cerosDelFactorial2 n = ceros2 (factorial n)
 
-- (ceros n) es el número de ceros en los que termina el número n. Por
-- ejemplo, 
--    ceros 320000  ==  4
ceros2 :: Integer -> Integer
ceros2 n = genericLength (takeWhile (=='0') (reverse (show n)))
 
-- 3ª definición
-- =============
 
cerosDelFactorial3 :: Integer -> Integer
cerosDelFactorial3 n | n < 5     = 0
                     | otherwise = m + cerosDelFactorial3 m
                     where m = n `div` 5
 
-- Comparación de la eficiencia
--    ghci> cerosDelFactorial1 (3*10^4)
--    7498
--    (3.96 secs, 1,252,876,376 bytes)
--    ghci> cerosDelFactorial2 (3*10^4)
--    7498
--    (3.07 secs, 887,706,864 bytes)
--    ghci> cerosDelFactorial3 (3*10^4)
--    7498
--    (0.03 secs, 9,198,896 bytes)


-- ---------------------------------------------------------------------
-- Ejercicio 3.1. El Triángulo de Floyd, llamado así en honor a Robert
-- Floyd, es un triángulo rectángulo formado con números naturales. Para
-- crear un triángulo de Floyd, se comienza con un 1 en la esquina
-- superior izquierda, y se continúa escribiendo la secuencia de los
-- números naturales de manera que cada línea contenga un número más que
-- la anterior: 
--    1
--    2    3
--    4    5       6
--    7    8       9       10
--    11   12      13      14      15
-- 
-- Definir la función
--    trianguloFloyd :: [[Int]]
-- tal que trianguloFloyd es la lista formada por todas las líneas del
-- triángulo. Por ejemplo,
--    ghci> take 10 trianguloFloyd
--    [[1],
--     [2,3],
--     [4,5,6],
--     [7,8,9,10],
--     [11,12,13,14,15],
--     [16,17,18,19,20,21],
--     [22,23,24,25,26,27,28],
--     [29,30,31,32,33,34,35,36],
--     [37,38,39,40,41,42,43,44,45],
--     [46,47,48,49,50,51,52,53,54,55]]
-- ---------------------------------------------------------------------

trianguloFloyd :: [[Integer]]
trianguloFloyd = iterate siguiente [1]

siguiente :: [Integer] -> [Integer]
siguiente xs = [a..a+n]
  where a = 1+last xs
        n = genericLength xs
        
-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función 
--   filaTrianguloFloyd :: Int -> [Int]
-- tal que (filaTrianguloFloyd n) es la n-sima fila del triángulo de
-- Floyd. Por ejemplo,
--   filaTrianguloFloyd 6 == [16,17,18,19,20,21]
-- ---------------------------------------------------------------------

filaTrianguloFloyd :: Integer -> [Integer]
filaTrianguloFloyd n = trianguloFloyd `genericIndex` (n - 1)

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Comprobar con QuickCheck la siguiente propiedad: la
-- suma de los números de la línea n es n(n^2 + 1)/2 .
-- ---------------------------------------------------------------------

-- La propiedad es
prop_trianguloFloyd n =        
  n > 0 ==> sum (filaTrianguloFloyd n) == (n*(n^2+1)) `div` 2
  
-- La comprobación es
--    ghci> quickCheck prop_trianguloFloyd
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4. Los árboles binarios con valores en las hojas y en los
-- nodos se definen por
--    data Arbol a = H a
--                 | N a (Arbol a) (Arbol a) 
--                 deriving Show
-- Por ejemplo, los árboles
--         5              8             5           5
--        / \            / \           / \         / \
--       /   \          /   \         /   \       /   \
--      9     7        9     3       9     2     4     7
--     / \   / \      / \   / \     / \               / \
--    1   4 6   8    1   4 6   2   1   4             6   2
-- se pueden representar por
--    ej3arbol1, ej3arbol2, ej3arbol3, ej3arbol4 :: Arbol Int
--    ej3arbol1 = N 5 (N 9 (H 1) (H 4)) (N 7 (H 6) (H 8))
--    ej3arbol2 = N 8 (N 9 (H 1) (H 4)) (N 3 (H 6) (H 2))
--    ej3arbol3 = N 5 (N 9 (H 1) (H 4)) (H 2)
--    ej3arbol4 = N 5 (H 4) (N 7 (H 6) (H 2))
--
-- Definir la función
--    igualEstructura :: Arbol -> Arbol -> Bool
-- tal que (igualEstructura a1 a1) se verifica si los árboles a1 y a2 
-- tienen la misma estructura. Por ejemplo,
--    igualEstructura ej3arbol1 ej3arbol2 == True
--    igualEstructura ej3arbol1 ej3arbol3 == False
--    igualEstructura ej3arbol1 ej3arbol4 == False
-- ---------------------------------------------------------------------

data Arbol a = H a
              | N a (Arbol a) (Arbol a) 
              deriving Show

ej3arbol1, ej3arbol2, ej3arbol3, ej3arbol4 :: Arbol Int
ej3arbol1 = N 5 (N 9 (H 1) (H 4)) (N 7 (H 6) (H 8))
ej3arbol2 = N 8 (N 9 (H 1) (H 4)) (N 3 (H 6) (H 2))
ej3arbol3 = N 5 (N 9 (H 1) (H 4)) (H 2)
ej3arbol4 = N 5 (H 4) (N 7 (H 6) (H 2))

igualEstructura :: Arbol a -> Arbol a -> Bool
igualEstructura (H _) (H _) = True
igualEstructura (N r1 i1 d1) (N r2 i2 d2) = 
    igualEstructura i1 i2 && igualEstructura d1 d2
igualEstructura _ _                       = False  
