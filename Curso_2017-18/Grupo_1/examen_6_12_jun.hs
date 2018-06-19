-- Informática (1º del Grado en Matemáticas, Grupos 1, 2 y 3)
-- 6º examen de evaluación continua (12 de junio de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Array
import Data.List
import Data.Matrix 
import Test.QuickCheck

-- ----------------------------------------------------------------------
-- Ejercicio 1.1. Decimos que una lista de números enteros está reducida
-- si no tiene dos elementos consecutivos tales que uno sea el sucesor
-- del otro. Por ejemplo, la lista [-6,-8,-7,3,2,4] no está reducida
-- porque tiene dos elementos consecutivos (el -8 y el -7) tales que -7
-- es el sucesor de -8 (también el 3 y el 2).
--
-- Una forma de reducir la lista es repetir el proceso de sustituir el
-- primer par de elementos consecutivos de la lista por el mayor de los
-- dos hasta que la lista quede reducida. Por ejemplo,
--        [-6,-8,-7,3,2,4]
--    ==> [-6,   -7,3,2,4]
--    ==> [-6,      3,2,4]
--    ==> [-6,      3,  4]
--    ==> [-6,          4]
--
-- Definir la función
--    reducida :: (Num a, Ord a) => [a] -> [a]
-- tal que (reducida xs) es la lista reducida obtenida a partir de
-- xs. Por ejemplo,
--    reducida [-6,-8,-7,3,2,4]  == [-6,4]
--    reducida [4,-7,-6,4,4,3,5] == [4,-6,5]
--    reducida [3,4,5,4,3,4]     == [5]
--    reducida [1..10]           == [10]
--    reducida [1,3..15]         == [1,3,5,7,9,11,13,15]
-- ---------------------------------------------------------------------

reducida :: (Num a, Ord a) => [a] -> [a]
reducida = until esReducida reduce 

-- (esReducida xs) se verifica si xs es una lista reducida. Por ejemplo,
--    esReducida [-6,-9,-7,2,0,4]  ==  True
--    esReducida [-6,-8,-7,2,3,4]  ==  False
esReducida :: (Num a, Ord a) => [a] -> Bool
esReducida xs =
  reduce xs == xs

reduce :: (Num a, Ord a) => [a] -> [a]
reduce []  = []
reduce [x] = [x]
reduce (x:y:xs) | abs (x-y) == 1 = max x y : xs
                | otherwise      = x : reduce (y:xs)

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que, si n es un número
-- positivo, la reducida de la lista [1..n] es la lista [n].
-- ---------------------------------------------------------------------

-- La propiedad es
propReducida :: Int -> Property
propReducida n =
  n > 0 ==> reducida [1..n] == [n]

-- La comprobación es
--    λ> quickCheck propReducida
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.1 Para cada número n con k dígitos se define una sucesión
-- de tipo Fibonacci cuyos k primeros elementos son los dígitos de n y
-- los siguientes se obtienen sumando los k anteriores términos de la
-- sucesión. Por ejemplo, la sucesión definida por 197 es
--    1, 9, 7, 17, 33, 57, 107, 197, ...
--
-- Definir la función
--    sucFG :: Integer -> [Integer]
-- tal que (sucFG n) es la sucesión de tipo Fibonacci definida por
-- n. Por ejemplo,
--    take 10 (sucFG 197) == [1,9,7,17,33,57,107,197,361,665]
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

sucFG :: Integer -> [Integer]
sucFG k = suc
  where ks  = digitos k
        d   = length ks
        suc = ks ++ aux [drop r suc | r <- [0..d-1]]
          where aux xss = sum (map head xss) : aux (map tail xss)

digitos :: Integer -> [Integer]
digitos n = [read [x] | x <- show n]

-- 2ª solución
-- ===========

sucFG2 :: Integer -> [Integer]
sucFG2 k = init ks ++ map last (iterate f ks)
  where ks   = digitos k
        f xs = tail xs ++ [sum xs]

-- Comparación de eficiencia
-- =========================

--    λ> length (show (sucFG 197 !! 60000))
--    15880
--    (2.05 secs, 475,918,520 bytes)
--    λ> length (show (sucFG2 197 !! 60000))
--    15880
--    (1.82 secs, 451,127,104 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Un número n > 9 es un número de Keith si n aparece en
-- la sucesión de tipo Fibonacci definida por n. Por ejemplo, 197 es un
-- número de Keith.
--
-- Definir la función
--    esKeith :: Integer -> Bool
-- tal que (esKeith n) se verifica si n es un número de Keith. Por
-- ejemplo, 
--    esKeith 197   == True
--    esKeith 54798 == False
-- ---------------------------------------------------------------------

esKeith :: Integer -> Bool
esKeith n = n == head (dropWhile (< n) (sucFG n))

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Las expresiones aritméticas construidas con una
-- variable, los números enteros y las operaciones de sumar y
-- multiplicar se pueden representar mediante el tipo de datos Exp
-- definido por
--    data Exp = Var | Const Int | Sum Exp Exp | Mul Exp  Exp
--      deriving Show
--
-- Por ejemplo, la expresión 3+5x^2 se puede representar por
--    exp1 :: Exp
--    exp1 = Sum (Const 3) (Mul Var (Mul Var (Const 5)))
--
-- Por su parte, los polinomios se pueden representar por la lista de
-- sus coeficientes. Por ejemplo, el polinomio 3+5x^2 se puede
-- representar por [3,0,5].
--
-- Definir la función
--    valorE :: Exp -> Int -> Int
-- tal que (valorE e n) es el valor de la expresión e cuando se
-- sustituye su variable por n. Por ejemplo,
--    λ> valorE (Sum (Const 3) (Mul Var (Mul Var (Const 5)))) 2
--    23
-- ---------------------------------------------------------------------

data Exp = Var | Const Int | Sum Exp Exp | Mul Exp  Exp
  deriving Show

valorE :: Exp -> Int -> Int
valorE Var n         = n
valorE (Const c) n   = c
valorE (Sum e1 e2) n = valorE e1 n + valorE e2 n
valorE (Mul e1 e2) n = valorE e1 n * valorE e2 n

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    expresion :: [Int] -> Exp
-- tal que (expresion p) es una expresión aritmética equivalente al
-- polinomio p. Por ejemplo,
--     λ> expresion [3,0,5]
--     Sum (Const 3) (Mul Var (Sum (Const 0) (Mul Var (Const 5))))
-- ---------------------------------------------------------------------

expresion :: [Int] -> Exp
expresion []     = Const 0
expresion [c]    = Const c
expresion (x:xs) = Sum (Const x) (Mul Var (expresion xs))

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir la función
--    valorP :: [Int] -> Int -> Int
-- tal que (valorP p n) es el valor del polinomio p cuando se sustituye
-- su variable por n. Por ejemplo,
--    valorP [3,0,5] 2  ==  23
-- ---------------------------------------------------------------------

-- 2ª solución
valorP :: [Int] -> Int -> Int
valorP xs n = valorE (expresion xs) n 

-- 2ª solución
valorP2 :: [Int] -> Int -> Int
valorP2 = valorE . expresion 

-- 3ª solución
valorP3 :: [Int] -> Int -> Int
valorP3 xs n = foldr f 0 xs
  where f x y = x + y*n


-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Dado n, consideremos la matriz cuadrada (nxn) cuyas
-- diagonales secundarias están formadas por los números 1,2..,2*n-1.
-- Por ejemplo, si n = 5,

--   ( 1 2 3 4 5 )
--   ( 2 3 4 5 6 )
--   ( 3 4 5 6 7 )
--   ( 4 5 6 7 8 )
--   ( 5 6 7 8 9 )

-- Definir la función
--    matrizDiagS :: Integer -> Matrix Integer
-- tal que (matrizDiagS n) construya dicha matriz de dimensión
-- (nxn). Por ejemplo,
--    λ> matrizDiagS 3
--    ( 1 2 3 )
--    ( 2 3 4 )
--    ( 3 4 5 )
--    
--    λ> matrizDiagS 10
--    (  1  2  3  4  5  6  7  8  9 10 )
--    (  2  3  4  5  6  7  8  9 10 11 )
--    (  3  4  5  6  7  8  9 10 11 12 )
--    (  4  5  6  7  8  9 10 11 12 13 )
--    (  5  6  7  8  9 10 11 12 13 14 )
--    (  6  7  8  9 10 11 12 13 14 15 )
--    (  7  8  9 10 11 12 13 14 15 16 )
--    (  8  9 10 11 12 13 14 15 16 17 )
--    (  9 10 11 12 13 14 15 16 17 18 )
--    ( 10 11 12 13 14 15 16 17 18 19 )
-- ---------------------------------------------------------------------

matrizDiagS :: Integer -> Matrix Integer
matrizDiagS n = fromLists xss
  where xss = genericTake n (iterate (map (+1)) [1..n])

-- Con Data.Array
matrizDiagS_b :: Integer -> Array (Integer,Integer) Integer
matrizDiagS_b n =
  listArray ((1,1),(n,n)) (concat [[i .. i+n-1] | i <- [1 .. n]])

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función 
--    sumaMatrizDiagS :: Integer -> Integer
-- tal que (sumaMatrizDiagS n) es la suma de los elementos
-- (matrizDiagS n). Por ejemplo, 
--    sumaMatrizDiagS 3       ==  27
--    sumaMatrizDiagS (10^3)  ==  1000000000
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

sumaMatrizDiagS1 :: Integer -> Integer
sumaMatrizDiagS1 = sum . toList . matrizDiagS

-- Con Data.Array
sumaMatrizDiagS_b :: Integer -> Integer
sumaMatrizDiagS_b = sum . elems . matrizDiagS_b

-- 2ª solución
-- ===========

sumaMatrizDiagS2 :: Integer -> Integer
sumaMatrizDiagS2 = sum . matrizDiagS

-- 3ª solución
-- ===========

-- Contando cuántas veces aparece cada número

sumaMatrizDiagS3 :: Integer -> Integer
sumaMatrizDiagS3 n =
  sum (zipWith (*) [1..2*n-1] ([1..n] ++ [n-1,n-2..1]))

-- Comparación de eficiencia
-- =========================

--    λ> sumaMatrizDiagS1 (10^3)
--    1000000000
--    (2.85 secs, 466,422,504 bytes)
--    λ> sumaMatrizDiagS2 (10^3)
--    1000000000
--    (2.63 secs, 418,436,656 bytes)
--    λ> sumaMatrizDiagS3 (10^3)
--    1000000000
--    (0.03 secs, 900,464 bytes)

