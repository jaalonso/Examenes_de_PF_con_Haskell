-- Informática (1º del Grado en Matemáticas, Grupos 4 y 5)
-- 6º examen de evaluación continua (12 de junio de 2018)
-- ---------------------------------------------------------------------

-- Nota: La puntuación de cada ejercicio es 2.5 puntos.

-- ---------------------------------------------------------------------
-- § Librerías                                                        --
-- ---------------------------------------------------------------------

import Data.List
import Data.Matrix
import I1M.PolOperaciones
import Test.QuickCheck
import qualified Data.Set as S

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. La sucesión de polinomios de Fibonacci se define por
--    p(0) = 0
--    p(1) = 1
--    p(n) = x*p(n-1) + p(n-2)
-- Los primeros términos de la sucesión son
--    p(2) = x
--    p(3) = x^2 + 1
--    p(4) = x^3 + 2*x
--    p(5) = x^4 + 3*x^2 + 1
--
-- Definir la lista
--    sucPolFib :: [Polinomio Integer]
-- tal que sus elementos son los polinomios de Fibonacci. Por ejemplo,
--    λ> take 7 sucPolFib
--    [0,1,1*x,x^2 + 1,x^3 + 2*x,x^4 + 3*x^2 + 1,x^5 + 4*x^3 + 3*x]
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

sucPolFib :: [Polinomio Integer]
sucPolFib = [polFibR n | n <- [0..]]

polFibR :: Integer -> Polinomio Integer
polFibR 0 = polCero
polFibR 1 = polUnidad
polFibR n = 
  sumaPol (multPol (consPol 1 1 polCero) (polFibR (n-1)))
          (polFibR (n-2))

-- 2ª definición (dinámica)
-- ========================

sucPolFib2 :: [Polinomio Integer]
sucPolFib2 = 
  polCero : polUnidad : zipWith f (tail sucPolFib2) sucPolFib2
  where f p = sumaPol (multPol (consPol 1 1 polCero) p)

-- --------------------------------------------------------------------- 
-- Ejercicio 1.2. Comprobar con QuickCheck que el valor del n-ésimo
-- término de sucPolFib para x=1 es el n-ésimo término de la sucesión de
-- Fibonacci 0, 1, 1, 2, 3, 5, 8, ... 
--
-- Nota. Limitar la búsqueda a ejemplos pequeños usando
--    quickCheckWith (stdArgs {maxSize=5}) prop_polFib
-- ---------------------------------------------------------------------

prop_polFib :: Integer -> Property
prop_polFib n = 
  n >= 0 ==> valor (polFib n) 1 == fib n
  where polFib n = sucPolFib2 `genericIndex` n
        fib n    = fibs `genericIndex` n

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- La comprobación es
--    ghci> quickCheckWith (stdArgs {maxSize=5}) prop_polFib
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2. Las expresiones aritméticas pueden representarse usando
-- el siguiente tipo de datos   
--    data Expr = N Int | S Expr Expr | P Expr Expr  
--      deriving (Eq, Ord, Show)
-- Por ejemplo, la expresión 2*(3+7) se representa por
--    P (N 2) (S (N 3) (N 7))
--
-- Definir la función
--    subexpresiones :: Expr -> S.Set Expr
-- tal que (subexpresiones e) es el conjunto de las subexpresiones de
-- e. Por ejemplo,
--    λ> subexpresiones (S (N 2) (N 3))
--    fromList [N 2,N 3,S (N 2) (N 3)]
--    λ> subexpresiones (P (S (N 2) (N 2)) (N 7))
--    fromList [N 2,N 7,S (N 2) (N 2),P (S (N 2) (N 2)) (N 7)]
-- ---------------------------------------------------------------------

data Expr = N Int | S Expr Expr | P Expr Expr  
  deriving (Eq, Ord, Show)

subexpresiones :: Expr -> S.Set Expr
subexpresiones (N x)   = S.singleton (N x)
subexpresiones (S i d) =
  S i d `S.insert` (subexpresiones i `S.union` subexpresiones d)
subexpresiones (P i d) =
  P i d `S.insert` (subexpresiones i `S.union` subexpresiones d)

-- ---------------------------------------------------------------------
-- Ejercicio 3. El triángulo de Pascal es un triángulo de números 
--          1
--         1 1
--        1 2 1
--      1  3 3  1
--     1 4  6  4 1
--    1 5 10 10 5 1
--   ...............
-- construido de la siguiente forma
-- + la primera fila está formada por el número 1;
-- + las filas siguientes se construyen sumando los números adyacentes
--   de la fila superior y añadiendo un 1 al principio y al final de la
--   fila. 
--
-- La matriz de Pascal es la matriz cuyas filas son los elementos de la
-- correspondiente fila del triángulo de Pascal completadas con
-- ceros. Por ejemplo, la matriz de Pascal de orden 6 es
--    |1 0  0  0 0 0|
--    |1 1  0  0 0 0|
--    |1 2  1  0 0 0|
--    |1 3  3  1 0 0|
--    |1 4  6  4 1 0|
--    |1 5 10 10 5 1|
-- 
-- Definir la función
--    matrizPascal :: Int -> Matriz Int 
-- tal que (matrizPascal n) es la matriz de Pascal de orden n. Por
-- ejemplo, 
--    λ> matrizPascal 6
--    (  1  0  0  0  0  0 )
--    (  1  1  0  0  0  0 )
--    (  1  2  1  0  0  0 )
--    (  1  3  3  1  0  0 )
--    (  1  4  6  4  1  0 )
--    (  1  5 10 10  5  1 )
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

matrizPascal :: Int -> Matrix Integer 
matrizPascal 1 = fromList 1 1 [1]
matrizPascal n = matrix n n f 
  where f (i,j) | i < n && j <  n  = p!(i,j)
                | i < n && j == n  = 0
                | j == 1 || j == n = 1
                | otherwise        = p!(i-1,j-1) + p!(i-1,j)
        p = matrizPascal (n-1)

-- 2ª solución
-- ===========

matrizPascal2 :: Int -> Matrix Integer
matrizPascal2 n = fromLists xss
  where yss = take n pascal
        xss = map (take n) (map (++ repeat 0) yss)
        
pascal :: [[Integer]]
pascal = [1] : map f pascal
    where f xs = zipWith (+) (0:xs) (xs ++ [0])

-- 3ª solución
-- ===========

matrizPascal3 :: Int -> Matrix Integer
matrizPascal3 n =  matrix n n f
  where f (i,j) | i >=  j   = comb (i-1) (j-1)
                | otherwise = 0

-- (comb n k) es el número de combinaciones (o coeficiente binomial) de
-- n sobre k. Por ejemplo,
comb :: Int -> Int -> Integer
comb n k = product [n',n'-1..n'-k'+1] `div` product [1..k']
  where n' = fromIntegral n
        k' = fromIntegral k

-- 4ª solución
-- ===========

matrizPascal4 :: Int -> Matrix Integer
matrizPascal4 n = p
  where p = matrix n n (\(i,j) -> f i j)
        f i 1 = 1
        f i j
          | j >  i    = 0
          | i == j    = 1
          | otherwise = p!(i-1,j) + p!(i-1,j-1)

-- Comparación de eficiencia
-- =========================

--    λ> maximum (matrizPascal 150)
--    46413034868354394849492907436302560970058760
--    (2.58 secs, 394,030,504 bytes)
--    λ> maximum (matrizPascal2 150)
--    46413034868354394849492907436302560970058760
--    (0.03 secs, 8,326,784 bytes)
--    λ> maximum (matrizPascal3 150)
--    46413034868354394849492907436302560970058760
--    (0.38 secs, 250,072,360 bytes)
--    λ> maximum (matrizPascal4 150)
--    46413034868354394849492907436302560970058760
--    (0.10 secs, 13,356,360 bytes)
--    
--    λ> length (show (maximum (matrizPascal2 300)))
--    89
--    (0.06 secs, 27,286,296 bytes)
--    λ> length (show (maximum (matrizPascal3 300)))
--    89
--    (2.74 secs, 2,367,037,536 bytes)
--    λ> length (show (maximum (matrizPascal4 300)))
--    89
--    (0.36 secs, 53,934,792 bytes)
--    
--    λ> length (show (maximum (matrizPascal2 700)))
--    209
--    (0.83 secs, 207,241,080 bytes)
--    λ> length (show (maximum (matrizPascal4 700)))
--    209
--    (2.22 secs, 311,413,008 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la función
--    sumas  :: Int -> [[Int]]
-- tal que (sumas n) es la lista de las descomposiciones de n como sumas
-- cuyos sumandos son 1 ó 2. Por ejemplo,
--    sumas 1            ==  [[1]]
--    sumas 2            ==  [[1,1],[2]]
--    sumas 3            ==  [[1,1,1],[1,2],[2,1]]
--    sumas 4            ==  [[1,1,1,1],[1,1,2],[1,2,1],[2,1,1],[2,2]]
-- ---------------------------------------------------------------------

-- 1ª definición 
sumas1 :: Int -> [[Int]]
sumas1 0 = [[]]
sumas1 1 = [[1]]
sumas1 n = [1:xs | xs <- sumas1 (n-1)] ++ [2:xs | xs <- sumas1 (n-2)]

-- 2ª definición 
sumas2 :: Int -> [[Int]]
sumas2 n = aux !! n
    where aux     = [[]] : [[1]] : zipWith f (tail aux) aux
          f xs ys = map (1:) xs ++ map (2:) ys

-- Comparación de las definiciones de sumas
--    ghci> length (sumas 25)
--    121393
--    (1.84 secs, 378307888 bytes)
--    ghci> length (sumas 26)
--    196418
--    (3.09 secs, 623707712 bytes)
--    ghci> length (sumas2 25)
--    121393
--    (0.11 secs, 39984864 bytes)
--    ghci> length (sumas2 26)
--    196418
--    (0.17 secs, 63880032 bytes)

-- La segunda definición es más eficiente y es la que usaremos en lo
-- sucesivo:
sumas :: Int -> [[Int]]
sumas = sumas2

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función
--    nSumas :: Int -> Integer
-- tal que (nSumas n) es el número de descomposiciones de n como sumas
-- cuyos sumandos son 1 ó 2. Por ejemplo, 
--    nSumas 4  ==  5
--    nSumas 7  ==  21
-- ---------------------------------------------------------------------

-- 1ª definición
nSumas1 :: Int -> Integer
nSumas1 = genericLength . sumas2

-- 2ª definición
nSumas2 :: Int -> Integer
nSumas2 0 = 1
nSumas2 1 = 1
nSumas2 n = nSumas2 (n-1) + nSumas2 (n-2)

-- 3ª definición
nSumas3 :: Int -> Integer
nSumas3 n = aux `genericIndex` n
    where aux = 1 : 1 : zipWith (+) aux (tail aux) 

-- Comparación de las definiciones de nSumas
--    ghci> nSumas1 33
--    5702887
--    (4.33 secs, 1831610456 bytes)
--    ghci> nSumas2 33
--    5702887
--    (12.33 secs, 1871308192 bytes)
--    ghci> nSumas3 33
--    5702887
--    (0.01 secs, 998704 bytes)

-- Nota. El valor de (nSumas n) es el n-ésimo término de la sucesión de
-- Fibonacci 1, 1, 2, 3, 5, 8, ...
