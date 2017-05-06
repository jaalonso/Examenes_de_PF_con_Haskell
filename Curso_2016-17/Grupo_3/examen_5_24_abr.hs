-- Informática (1º del Grado en Matemáticas)
-- 5º examen de evaluación continua (23 de abril de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Array
import Data.List (nub, sort)
import Data.Numbers.Primes (isPrime, primeFactors)
import I1M.Pol
import I1M.Cola
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Los vectores se definen usando tablas como sigue:
--    type Vector a = Array Int a
-- 
-- Un elemento de un vector es un máximo local si no tiene ningún
-- elemento adyacente mayor o igual que él. 
-- 
-- Definir la función
--    posMaxVec :: Ord a => Vector a -> [Int]
-- tal que (posMaxVec p) es la lista de posiciones del vector p en las
-- que p tiene un máximo local. Por ejemplo, 
--    posMaxVec (listArray (1,6) [3,2,6,7,5,3]) == [1,4]
--    posMaxVec (listArray (1,2) [6,4])         == [1]
--    posMaxVec (listArray (1,2) [5,6])         ==  2]
--    posMaxVec (listArray (1,2) [5,5])         == []
--    posMaxVec (listArray (1,1) [5])           == [1]
-- ---------------------------------------------------------------------

type Vector a = Array Int a

posMaxVec :: Ord a => Vector a -> [Int]
posMaxVec v =
  [k | k <- [1..n]
     , and [v!j < v!k | j <- posAdyacentes k]]
  where (_,n)           = bounds v 
        posAdyacentes k = [k-1 | k > 1] ++ [k+1 | k < n]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Los árboles binarios se pueden representar con el tipo
-- de dato algebraico 
--    data Arbol a = H | N a (Arbol a) (Arbol a)
--                    deriving Show
-- Por ejemplo, los árboles

--         9                9                
--        / \              / 
--       /   \            /  
--      8     6          8  
--     / \   / \        / \ 
--    3   2 4   5      3   2
-- se pueden representar por
--    ej1, ej2:: Arbol Int
--    ej1 = N 9 (N 8 (N 3 H H) (N 2 H H)) (N 6 (N 4 H H) (N 5 H H))
--    ej2 = N 9 (N 8 (N 3 H H) (N 2 H H)) H
-- 
-- Para indicar las posiciones del árbol se define el tipo
--    type Posicion = [Direccion]
-- donde
--    data Direccion = D | I  deriving Eq
-- representa un movimiento hacia la derecha (D) o a la izquierda. Por
-- ejemplo, las posiciones de los elementos del ej1 son 
-- + el 9 tiene posición [] 
-- + el 8 tiene posición [I]
-- + el 3 tiene posición [I,I]
-- + el 2 tiene posición [I,D]
-- + el 6 tiene posición [D]
-- + el 4 tiene posición [D,I]
-- + el 5 tiene posición [D,D]
-- 
-- Definir la función
--    sustitucion :: Posicion -> a -> Arbol a -> Arbol a
-- tal que (sustitucion ds z x) es el árbol obtenido sustituyendo el
-- elemento del arbol x en la posición ds por z. Por ejemplo, 
--    ghci> sustitucion [I,D] 7 ej1
--    N 9 (N 8 (N 3 H H) (N 7 H H)) (N 6 (N 4 H H) (N 5 H H))
--    ghci> sustitucion [D,D] 7 ej1
--    N 9 (N 8 (N 3 H H) (N 2 H H)) (N 6 (N 4 H H) (N 7 H H))
--    ghci> sustitucion [I] 7 ej1
--    N 9 (N 7 (N 3 H H) (N 2 H H)) (N 6 (N 4 H H) (N 5 H H))
--    ghci> sustitucion [] 7 ej1
--    N 7 (N 8 (N 3 H H) (N 2 H H)) (N 6 (N 4 H H) (N 5 H H))
-- ---------------------------------------------------------------------

data Arbol a = H | N a (Arbol a) (Arbol a)
                deriving Show

ej1, ej2:: Arbol Int
ej1 = N 9 (N 8 (N 3 H H) (N 2 H H)) (N 6 (N 4 H H) (N 5 H H))
ej2 = N 9 (N 8 (N 3 H H) (N 2 H H)) H

type Posicion = [Direccion]

data Direccion = D | I  deriving Eq

sustitucion :: Posicion  -> a -> Arbol a -> Arbol a
sustitucion (I:ds) z (N x i d) = N x (sustitucion ds z i) d
sustitucion (D:ds) z (N x i d) = N x i (sustitucion ds z d)
sustitucion []     z (N _ i d) = N z i d
sustitucion _      _ H         = H

-- ---------------------------------------------------------------------
-- Ejercicio 3. Un número n es especial si al unir los dígitos de sus
-- factores primos, se obtienen exactamente los dígitos de n, aunque
-- puede ser en otro orden. Por ejemplo, 1255 es especial, pues los
-- factores primos de 1255 son 5 y 251. 
-- 
-- Definir la función
--    esEspecial :: Integer -> Bool
-- tal que (esEspecial n) se verifica si un número n es especial. Por
-- ejemplo, 
--    esEspecial 1255 == True
--    esEspecial 125  == False
--    esEspecial 132  == False
-- Comprobar con QuickCheck que todo número primo es especial.
--
-- Calcular los 5 primeros números especiales que no son primos.
-- ---------------------------------------------------------------------

esEspecial :: Integer -> Bool
esEspecial n = 
  sort (show n) == sort (concatMap show (primeFactors n))

-- La propiedad es
prop_primos :: Integer -> Property
prop_primos n = 
  isPrime (abs n) ==> esEspecial (abs n)

-- La comprobación es
--    ghci> quickCheck prop_primos
--    +++ OK, passed 100 tests.

-- El cálculo es
--    ghci> take 5 [n | n <- [2..], esEspecial n, not (isPrime n)]
--    [1255,12955,17482,25105,100255]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Un polinomio no nulo con coeficientes enteros es primo
-- si el máximo común divisor de sus coeficientes es 1.
--
-- Definir la función
--    primo :: Polinomio Int -> Bool
-- tal que (primo p) se verifica si el polinomio p es primo. Por ejemplo,
--    ghci> primo (consPol 6 2 (consPol  5 3 (consPol 4 8 polCero)))
--    True
--    ghci> primo (consPol 6 2 (consPol  5 6 (consPol 4 8 polCero)))
--    False
-- ---------------------------------------------------------------------

primo :: Polinomio Int -> Bool
primo p = foldl1 gcd (coeficientes p) == 1

coeficientes :: (Num a, Eq a) => Polinomio a -> [a]
coeficientes p = [coeficiente k p | k <- [n,n-1..0]]
  where n = grado p

coeficiente :: (Num a, Eq a) => Int -> Polinomio a -> a
coeficiente k p | k == n                 = coefLider p
                | k > grado (restoPol p) = 0
                | otherwise              = coeficiente k (restoPol p)
    where n = grado p

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    penultimo :: Cola a -> a
-- tal que (penultimo c) es el penúltimo elemento de la cola c. Si la
-- cola está vacía o tiene un sólo elemento, dará el error
-- correspondiente, "cola vacia" o bien "cola unitaria". Por ejemplo, 
--    ghci> penultimo (inserta 3 (inserta 2 vacia))
--    2
--    ghci> penultimo (inserta 5 (inserta 3 (inserta 2 vacia)))
--    3
--    ghci> penultimo vacia
--    *** Exception: cola vacia
--    ghci> penultimo (inserta 2 vacia)
--    *** Exception: cola unitaria
-- ---------------------------------------------------------------------

penultimo ::  Cola a -> a
penultimo c = aux (reverse (cola2Lista c))
  where aux []      = error "cola vacia"
        aux [_]     = error "cola unitaria"
        aux (_:x:_) = x
        
-- (cola2Lista c) es la lista formada por los elementos de p. Por
-- ejemplo,  
--    cola2Lista c2 == [17,14,11,8,5,2]
cola2Lista :: Cola a -> [a]
cola2Lista c
  | esVacia c = []
  | otherwise = pc : cola2Lista rc
  where pc = primero c
        rc = resto c
