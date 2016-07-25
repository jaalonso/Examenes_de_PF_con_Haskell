-- Informática (1º del Grado en Matemáticas)
-- 4º examen de evaluación continua (9 de marzo de 2015)
-- ---------------------------------------------------------------------

import I1M.PolOperaciones
import Data.List 
import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1. Un capicúa es un número que es igual leído de izquierda
-- a derecha que de derecha a izquierda. 
-- 
-- Definir la función
--    mayorCapicuaP :: Integer -> Integer
-- tal que (mayorCapicuaP n) es el mayor capicúa que es el producto de
-- dos números de n cifras. Por ejemplo,  
--    mayorCapicuaP 2  ==  9009
--    mayorCapicuaP 3  ==  906609
--    mayorCapicuaP 4  ==  99000099
--    mayorCapicuaP 5  ==  9966006699
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

mayorCapicuaP1 :: Integer -> Integer
mayorCapicuaP1 n = maximum [x*y | x <- [a,a-1..b],
                                  y <- [a,a-1..b],
                                  esCapicua (x*y)] 
    where a = 10^n-1
          b = 10^(n-1)

-- (esCapicua x) se verifica si x es capicúa. Por ejemplo,
--    esCapicua 353  ==  True
--    esCapicua 357  ==  False
esCapicua :: Integer -> Bool
esCapicua n = xs == reverse xs
    where xs = show n

-- 2ª solución
-- ===========

mayorCapicuaP2 :: Integer -> Integer
mayorCapicuaP2 n = maximum [x | y <- [a..b],
                                z <- [y..b],
                                let x = y * z,
                                let s = show x,
                                s == reverse s]
     where a = 10^(n-1)
           b = 10^n-1

-- ---------------------------------------------------------------------
-- Ejercicio 2. Sea (b(i) | i ≥ 1) una sucesión infinita de números
-- enteros mayores que 1. Entonces todo entero x mayor que cero se puede
-- escribir de forma única como
--    x = x(0) + x(1)b(1) +x(2)b(1)b(2) + ... + x(n)b(1)b(2)...b(n)
-- donde cada x(i) satisface la condición 0 ≤ x(i) < b(i+1). Se dice
-- que [x(n),x(n-1),...,x(2),x(1),x(0)] es la representación de x en la
-- base (b(i)). Por ejemplo, la representación de 377 en la base 
-- (2*i | i >= 1) es [7,5,0,1] ya que
--    377 = 1 + 0*2 + 5*2*4 + 7*2*4*6
-- y, además, 0 ≤ 1 < 2, 0 ≤ 0 < 4, 0 ≤ 5 < 6 y 0 ≤ 7 < 8.
-- 
-- Definir las funciones
--    decimalAmultiple :: [Integer] -> Integer -> [Integer]
--    multipleAdecimal :: [Integer] -> [Integer] -> Integer
-- tales que (decimalAmultiple bs x) es la representación del número x
-- en la base bs y (multipleAdecimal bs cs) es el número decimal cuya
-- representación en la base bs es cs. Por ejemplo,
--    decimalAmultiple [2,4..] 377                      ==  [7,5,0,1]
--    multipleAdecimal [2,4..] [7,5,0,1]                ==  377
--    decimalAmultiple [2,5..] 377                      ==  [4,5,3,1]
--    multipleAdecimal [2,5..] [4,5,3,1]                ==  377
--    decimalAmultiple [2^n | n <- [1..]] 2015          ==  [1,15,3,3,1]
--    multipleAdecimal [2^n | n <- [1..]] [1,15,3,3,1]  ==  2015
--    decimalAmultiple (repeat 10) 2015                 ==  [2,0,1,5]
--    multipleAdecimal (repeat 10) [2,0,1,5]            ==  2015
-- ---------------------------------------------------------------------

-- 1ª definición de decimalAmultiple (por recursión)
decimalAmultiple :: [Integer] -> Integer -> [Integer]
decimalAmultiple bs n = reverse (aux bs n)
    where aux _ 0 = []
          aux (b:bs) n = r : aux bs q
              where (q,r) = quotRem n b

-- 2ª definición de decimalAmultiple (con acumulador)
decimalAmultiple2 :: [Integer] -> Integer -> [Integer]
decimalAmultiple2 bs n = aux bs n []
    where aux _ 0  xs = xs
          aux (b:bs) n xs = aux bs q (r:xs)
              where (q,r) = quotRem n b

-- 1ª definición multipleAdecimal (por recursión)
multipleAdecimal :: [Integer] -> [Integer] -> Integer
multipleAdecimal xs ns = aux xs (reverse ns)
    where aux (x:xs) (n:ns) = n + x * (aux xs ns)
          aux _ _           = 0

-- 2ª definición multipleAdecimal (con scanl1)
multipleAdecimal2 :: [Integer] -> [Integer] -> Integer
multipleAdecimal2 bs xs =
    sum (zipWith (*) (reverse xs) (1 : scanl1 (*) bs))

-- ---------------------------------------------------------------------
-- Ejercicio 3. Las expresiones aritméticas pueden representarse usando
-- el siguiente tipo de datos  
--    data Expr = N Int | S Expr Expr | P Expr Expr  
--                deriving (Eq, Show)
-- Por ejemplo, la expresión 2*(3+7) se representa por
--    P (N 2) (S (N 3) (N 7))
--
-- Definir la función
--    subexpresiones :: Expr -> [Expr]
-- tal que (subexpresiones e) es el conjunto de las subexpresiones de
-- e. Por ejemplo,
--    ghci> subexpresiones (S (N 2) (N 3))
--    [S (N 2) (N 3),N 2,N 3]
--    ghci> subexpresiones (P (S (N 2) (N 2)) (N 7))
--    [P (S (N 2) (N 2)) (N 7),S (N 2) (N 2),N 2,N 7]
-- ---------------------------------------------------------------------

data Expr = N Int | S Expr Expr | P Expr Expr  
            deriving (Eq, Show)

subexpresiones :: Expr -> [Expr]
subexpresiones = nub . aux
    where aux (N x) = [N x]
          aux (S i d) = S i d : (subexpresiones i ++ subexpresiones d)
          aux (P i d) = P i d : (subexpresiones i ++ subexpresiones d)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    diagonalesPrincipales :: Array (Int,Int) a -> [[a]]
-- tal que (diagonalesPrincipales p) es la lista de las diagonales
-- principales de p. Por ejemplo, para la matriz
--    1  2  3  4
--    5  6  7  8
--    9 10 11 12
-- la lista de sus diagonales principales es 
--    [[9],[5,10],[1,6,11],[2,7,12],[3,8],[4]]
-- En Haskell,
--    ghci> diagonalesPrincipales (listArray ((1,1),(3,4)) [1..12])
--    [[9],[5,10],[1,6,11],[2,7,12],[3,8],[4]]
-- ---------------------------------------------------------------------

diagonalesPrincipales :: Array (Int,Int) a -> [[a]]
diagonalesPrincipales p = 
    [[p!ij1 | ij1 <- extension ij] | ij <- iniciales] 
    where (_,(m,n)) = bounds p
          iniciales = [(i,1) | i <- [m,m-1..2]] ++ [(1,j) | j <- [1..n]] 
          extension (i,j) = [(i+k,j+k) | k <- [0..min (m-i) (n-j)]]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Dado un polinomio p no nulo con coeficientes enteros, se
-- llama contenido de p al máximo común divisor de sus coeficientes. Se
-- dirá que p es primitivo si su contenido es 1. 
-- 
-- Definir la función
--    primitivo :: Polinomio Int -> Bool
-- tal que (primitivo p) se verifica si el polinomio p es primitivo. Por
-- ejemplo, 
--    ghci> let listaApol xs = foldr (\(n,b) -> consPol n b) polCero xs
--    ghci> primitivo (listaApol [(6,2),(4,3)])
--    True
--    ghci> primitivo (listaApol [(6,2),(5,3),(4,8)])
--    True
--    ghci> primitivo (listaApol [(6,2),(5,6),(4,8)])
--    False
-- ---------------------------------------------------------------------

primitivo :: Polinomio Int -> Bool
primitivo p = contenido p == 1

contenido :: Polinomio Int -> Int
contenido p 
    | n == 0 = b
    | otherwise = gcd b (contenido r)
    where n = grado p
          b = coefLider p
          r = restoPol p

