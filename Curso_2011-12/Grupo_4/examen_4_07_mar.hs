-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 4º examen de evaluación continua (7 de marzo de 2012)
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función 
--    inicialesDistintos :: Eq a => [a] -> Int
-- tal que (inicialesDistintos xs) es el número de elementos que hay en
-- xs antes de que aparezca el primer repetido. Por ejemplo,
--    inicialesDistintos [1,2,3,4,5,3] == 2
--    inicialesDistintos [1,2,3]       == 3
--    inicialesDistintos "ahora"       == 0
--    inicialesDistintos "ahorA"       == 5
-- ---------------------------------------------------------------------

inicialesDistintos [] = 0
inicialesDistintos (x:xs) 
    | x `elem` xs = 0 
    | otherwise = 1 + inicialesDistintos xs

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Diremos que un número entero positivo es autodivisible 
-- si es divisible por todas sus cifras diferentes de 0.  Por ejemplo,
-- el número 150 es autodivisible ya que es divisible por 1 y por 5 (el
-- 0 no se usará en dicha comprobación), mientras que el 123 aunque es
-- divisible por 1 y por 3, no lo es por 2, y por tanto no es
-- autodivisible. 
--
-- Definir, por comprensión, la función
--    autodivisibleC :: Integer -> Bool
-- tal que (autodivisibleC n) se verifica si n es autodivisible. Por
-- ejemplo, 
--    autodivisibleC 0       == True
--    autodivisibleC 25      == False
--    autodivisibleC 1234    == False
--    autodivisibleC 1234608 == True
-- ---------------------------------------------------------------------

autodivisibleC :: Integer -> Bool
autodivisibleC n = and [d == 0 || n `rem` d == 0 | d <- cifras n] 

-- (cifra n) es la lista de las cifras de n. Por ejemplo,
--    cifras 325  ==  [3,2,5]
cifras :: Integer -> [Integer]
cifras n = [read [y] | y <- show n]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, por recursión, la función
--    autodivisibleR :: Integer -> Bool
-- tal que (autodivisibleR n) se verifica si n es autodivisible. Por
-- ejemplo, 
--    autodivisibleR 0       == True
--    autodivisibleR 25      == False
--    autodivisibleR 1234    == False
--    autodivisibleR 1234608 == True
-- ---------------------------------------------------------------------

autodivisibleR :: Integer -> Bool
autodivisibleR n = aux n (cifras n)
    where aux _ [] = True
          aux n (x:xs) | x == 0 || n `rem` x == 0 = aux n xs
                       | otherwise                = False

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Comprobar con QuickCheck que las definiciones
-- autodivisibleC y autodivisibleR son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_autodivisible :: Integer -> Property
prop_autodivisible n = 
    n > 0 ==> autodivisibleC n == autodivisibleR n

-- La comprobación es
--    ghci> quickCheck prop_autodivisible
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Definir la función 
--    siguienteAutodivisible :: Integer -> Integer
-- tal que (siguienteAutodivisible n) es el menor número autodivisible
-- mayor o igual que n. Por ejemplo,
--    siguienteAutodivisible 1234 == 1236
--    siguienteAutodivisible 111  == 111
-- ---------------------------------------------------------------------

siguienteAutodivisible :: Integer -> Integer
siguienteAutodivisible n = 
    head [x | x <- [n..], autodivisibleR x]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Los árboles binarios se pueden representar mediante el
-- siguiente tipo de datos 
--    data Arbol = H  
--               | N Int Arbol Arbol
-- donde H representa una hoja y N un nodo con un valor y dos ramas. Por
-- ejemplo, el árbol 
--             5
--             /\
--            /  \ 
--           /    \ 
--          1      4   
--         / \    / \
--        /   \  H   5
--       5     H    / \
--      / \        H   H
--     H   H 
-- se representa por
--    arbol1 :: Arbol
--    arbol1 = N 5 (N 1 (N 5 H H) H) (N 4 H (N 5 H H))
--
-- Definir la función 
--    cuentaArbol :: Arbol -> Int -> Int
-- tal que (cuentaArbol a x) es el número de veces aparece x en el árbol
-- a. Por ejemplo,
--    cuentaArbol arbol1 5    = 3
--    cuentaArbol arbol1 2    = 0
--    cuentaArbol (N 5 H H) 5 = 1
--    cuentaArbol H 5         = 0
-- ---------------------------------------------------------------------

data Arbol = H
           | N Int Arbol Arbol
           deriving Show

arbol1 :: Arbol
arbol1 = N 5 (N 1 (N 5 H H) H) (N 4 H (N 5 H H))

cuentaArbol :: Arbol -> Int -> Int
cuentaArbol H _ = 0
cuentaArbol (N n a1 a2) x 
    | n == x    = 1 + c1 + c2
    | otherwise = c1 + c2
    where c1 = cuentaArbol a1 x
          c2 = cuentaArbol a2 x
