-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 6º examen de evaluación continua (11 de abril de 2011)
-- ---------------------------------------------------------------------
 
import Data.List
import Data.Array
import Monticulo

-- ---------------------------------------------------------------------
-- Ejercicio 1. Las expresiones aritméticas pueden representarse usando
-- el siguiente tipo de datos 
--    data Expr = N Int | V Char | S Expr Expr | P Expr Expr  
--              deriving Show
-- Por ejemplo, la expresión 2*(a+5) se representa por
--    P (N 2) (S (V 'a') (N 5))
-- 
-- Definir la función
--    valor :: Expr -> [(Char,Int)] -> Int                   
-- tal que (valor x e) es el valor de la expresión x en el entorno e (es
-- decir, el valor de la expresión donde las variables de x se sustituyen
-- por los valores según se indican en el entorno e). Por ejemplo,
--    ghci> valor (P (N 2) (S (V 'a') (V 'b'))) [('a',2),('b',5)]
--    14
-- ---------------------------------------------------------------------

data Expr = N Int | V Char | S Expr Expr | P Expr Expr  
          deriving Show
                   
valor :: Expr -> [(Char,Int)] -> Int                   
valor (N x)   e = x
valor (V x)   e = head [y | (z,y) <- e, z == x]  
valor (S x y) e = valor x e + valor y e
valor (P x y) e = valor x e * valor y e

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    ocurrencias :: Ord a => a -> Monticulo a -> Int
-- tal que (ocurrencias x m) es el número de veces que ocurre el
-- elemento x en el montículo m. Por ejemplo,
--    ocurrencias 7 (foldr inserta vacio [6,1,7,8,7,5,7])  ==  3
-- ---------------------------------------------------------------------

ocurrencias :: Ord a => a -> Monticulo a -> Int
ocurrencias x m
    | esVacio m = 0
    | x < mm    = 0
    | x == mm   = 1 + ocurrencias x rm
    | otherwise = ocurrencias x rm
    where mm = menor m
          rm = resto m

-- ---------------------------------------------------------------------
-- Ejercicio 3. Se consideran los tipos de los vectores y de las
-- matrices definidos por 
--    type Vector a = Array Int a
--    type Matriz a = Array (Int,Int) a
-- 
-- Definir la función
--    diagonal :: Num a => Vector a -> Matriz a
-- tal que (diagonal v) es la matriz cuadrada cuya diagonal es el vector
-- v. Por ejemplo,
--    ghci> diagonal (array (1,3) [(1,7),(2,6),(3,5)])
--    array ((1,1),(3,3)) [((1,1),7),((1,2),0),((1,3),0),
--                         ((2,1),0),((2,2),6),((2,3),0),
--                         ((3,1),0),((3,2),0),((3,3),5)]
-- ---------------------------------------------------------------------

type Vector a = Array Int a
type Matriz a = Array (Int,Int) a

diagonal :: Num a => Vector a -> Matriz a
diagonal v =
    array ((1,1),(n,n))
          [((i,j),f i j) | i <- [1..n], j <- [1..n]]
    where n = snd (bounds v)
          f i j | i == j    = v!i
                | otherwise = 0

-- ---------------------------------------------------------------------
-- Ejercicio 4. El enunciado del problema 652 de "Números y algo más" es
-- el siguiente 
--    Si factorizamos los factoriales de un número en función de sus
--    divisores primos y sus potencias, ¿Cuál es el menor número N tal
--    que entre los factores primos y los exponentes de estos, N!
--    contiene los dígitos del cero al nueve? 
--    Por ejemplo 
--        6! = 2^4*3^2*5^1, le faltan los dígitos 0,6,7,8 y 9
--       12! = 2^10*3^5*5^2*7^1*11^1, le faltan los dígitos 4,6,8 y 9
-- 
-- Definir la función 
--    digitosDeFactorizacion :: Integer -> [Integer]
-- tal que (digitosDeFactorizacion n) es el conjunto de los dígitos que
-- aparecen en la factorización de n. Por ejemplo,
--    digitosDeFactorizacion (factorial 6)   ==  [1,2,3,4,5]
--    digitosDeFactorizacion (factorial 12)  ==  [0,1,2,3,5,7]
-- Usando la función anterior, calcular la solución del problema.
-- ---------------------------------------------------------------------

digitosDeFactorizacion :: Integer -> [Integer]
digitosDeFactorizacion n =
   sort (nub (concat [digitos x | x <- numerosDeFactorizacion n]))

-- (digitos n) es la lista de los digitos del número n. Por ejemplo, 
--    digitos 320274  ==  [3,2,0,2,7,4]
digitos :: Integer -> [Integer]
digitos n = [read [x] | x <- show n]

-- (numerosDeFactorizacion n) es el conjunto de los números en la
-- factorización de n. Por ejemplo,
--    numerosDeFactorizacion 60  ==  [1,2,3,5]
numerosDeFactorizacion :: Integer -> [Integer]
numerosDeFactorizacion n = 
   sort (nub (aux (factorizacion n)))
   where aux [] = []
         aux ((x,y):zs) = x : y : aux zs

-- (factorización n) es la factorización de n. Por ejemplo,
--    factorizacion 300  ==  [(2,2),(3,1),(5,2)]
factorizacion :: Integer -> [(Integer,Integer)]
factorizacion n = 
    [(head xs, fromIntegral (length xs)) | xs <- group (factorizacion' n)]

-- (factorizacion' n) es la lista de todos los factores primos de n; es
-- decir, es una lista de números primos cuyo producto es n. Por ejemplo,
--    factorizacion 300  ==  [2,2,3,5,5]
factorizacion' :: Integer -> [Integer]
factorizacion' n | n == 1    = []
                 | otherwise = x : factorizacion' (div n x)
                 where x = menorFactor n

-- (menorFactor n) es el menor factor primo de n. Por ejemplo,
--    menorFactor 15  ==  3
menorFactor :: Integer -> Integer
menorFactor n = head [x | x <- [2..], rem n x == 0]

-- (factorial n) es el factorial de n. Por ejemplo,
--    factorial 5  ==  120
factorial :: Integer -> Integer
factorial n = product [1..n]

-- Para calcular la solución, se define la constante
solucion = 
    head [n | n <- [1..], digitosDeFactorizacion (factorial n) == [0..9]]

-- El cálculo de la solución es
--    ghci> solucion
--    49
