-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 2º examen de evaluación continua (19 de diciembre de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Char 
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Se dice que un número natural n es una colina si su
-- primer dígito es igual a su último dígito, los primeros dígitos son
-- estrictamente creciente hasta llegar al máximo, el máximo se puede
-- repetir y los dígitos desde el máximo al final son estrictamente
-- decrecientes. 
--
-- Definir la función
--    esColina :: Integer -> Bool
-- tal que (esColina n) se verifica si n es un número colina. Por
-- ejemplo, 
--    esColina 12377731  ==  True
--    esColina 1237731   ==  True
--    esColina 123731    ==  True
--    esColina 122731    ==  False
--    esColina 12377730  ==  False
--    esColina 12374731  ==  False
--    esColina 12377730  ==  False
--    esColina 10377731  ==  False
--    esColina 12377701  ==  False
--    esColina 33333333  ==  True
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

esColina :: Integer -> Bool
esColina n =
  head ds == last ds &&
  esCreciente xs &&
  esDecreciente ys
  where ds = digitos n
        m  = maximum ds
        xs = takeWhile (<m) ds
        ys = dropWhile (==m) (dropWhile (<m) ds)

-- (digitos n) es la lista de los dígitos de n. Por ejemplo,
--    digitos 425  ==  [4,2,5]
digitos :: Integer -> [Int]
digitos n = map digitToInt (show n)

-- (esCreciente xs) se verifica si la lista xs es estrictamente
-- creciente. Por ejemplo,
--    esCreciente [2,4,7]  ==  True
--    esCreciente [2,2,7]  ==  False
--    esCreciente [2,1,7]  ==  False
esCreciente :: [Int] -> Bool
esCreciente xs = and [x < y | (x,y) <- zip xs (tail xs)]

-- (esDecreciente xs) se verifica si la lista xs es estrictamente
-- decreciente. Por ejemplo,
--    esDecreciente [7,4,2]  ==  True
--    esDecreciente [7,2,2]  ==  False
--    esDecreciente [7,1,2]  ==  False
esDecreciente :: [Int] -> Bool
esDecreciente xs = and [x > y | (x,y) <- zip xs (tail xs)]

-- 2ª definición
-- =============

esColina2 :: Integer -> Bool
esColina2 n =
  head ds == last ds &&
  null (dropWhile (==(-1)) (dropWhile (==0) (dropWhile (==1) xs)))
  where ds = digitos n
        xs = [signum (y-x) | (x,y) <- zip ds (tail ds)] 

-- Equivalencia
-- ============

-- La propiedad de equivalencia es
prop_esColina :: Integer -> Property
prop_esColina n =
  n >= 0 ==> esColina n == esColina2 n 

-- La comprobación es
--    λ> quickCheck prop_esColina
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2. La persistencia multiplicativa de un número  es la
-- cantidad de pasos requeridos para reducirlo a un dígito multiplicando
-- sus dígitos. Por ejemplo, la persistencia de 39 es 3 porque 3*9 = 27,
-- 2*7 = 14 y 1*4 = 4.  
--
-- Definir la función
--    persistencia     :: Integer -> Integer
-- tal que (persistencia x) es la persistencia de x. Por ejemplo,
--    persistencia 39                             ==   3
--    persistencia 2677889                        ==   8
--    persistencia 26888999                       ==   9
--    persistencia 3778888999                     ==  10
--    persistencia 277777788888899                ==  11
--    persistencia 77777733332222222222222222222  ==  11
-- ---------------------------------------------------------------------

persistencia :: Integer -> Integer
persistencia x
  | x < 10    = 0
  | otherwise = 1 + persistencia (productoDigitos x)

productoDigitos :: Integer -> Integer
productoDigitos x 
  | x < 10    = x
  | otherwise = r * productoDigitos y
  where (y,r) = quotRem x 10

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    producto :: [[a]] -> [[a]]
-- tal que (producto xss) es el producto cartesiano de los conjuntos
-- xss. Por ejemplo,
--    ghci> producto [[1,3],[2,5]]
--    [[1,2],[1,5],[3,2],[3,5]]
--    ghci> producto [[1,3],[2,5],[6,4]]
--    [[1,2,6],[1,2,4],[1,5,6],[1,5,4],[3,2,6],[3,2,4],[3,5,6],[3,5,4]]
--    ghci> producto [[1,3,5],[2,4]]
--    [[1,2],[1,4],[3,2],[3,4],[5,2],[5,4]]
--    ghci> producto []
--    [[]]
-- ---------------------------------------------------------------------

-- 1ª solución
producto :: [[a]] -> [[a]]
producto []       = [[]]
producto (xs:xss) = [x:ys | x <- xs, ys <- producto xss]

-- 2ª solución
producto2 :: [[a]] -> [[a]]
producto2 = foldr f [[]]
  where f xs xss = [x:ys | x <- xs, ys <- xss]

-- 3ª solución
producto3 :: [[a]] -> [[a]]
producto3 = foldr aux [[]] 
  where aux [] _      = []
aux (x:xs) ys = map (x:) ys ++ aux xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 4. Las expresiones aritméticas. generales se contruyen con
-- las sumas generales (sumatorios) y productos generales
-- (productorios). Su tipo es 
--    data Expresion = N Int
--                   | S [Expresion]
--                   | P [Expresion]
--      deriving Show
-- Por ejemplo, la expresión (2 * (1 + 2 + 1) * (2 + 3)) + 1 se
-- representa por S [P [N 2, S [N 1, N 2, N 1], S [N 2, N 3]], N 1]
--
-- Definir la función
--    valor :: Expresion -> Int
-- tal que (valor e) es el valor de la expresión e. Por ejemplo,
--    λ> valor (S [P [N 2, S [N 1, N 2, N 1], S [N 2, N 3]], N 1])
--    41
-- ---------------------------------------------------------------------

data Expresion = N Int
               | S [Expresion]
               | P [Expresion]
  deriving Show

valor :: Expresion -> Int
valor (N x)  = x
valor (S es) = sum (map valor es)
valor (P es) = product (map valor es)
