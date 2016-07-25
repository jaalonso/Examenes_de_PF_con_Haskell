-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 6º examen de evaluación continua (15 de abril de 2011)
-- ---------------------------------------------------------------------

import Data.List
import Data.Array
import Test.QuickCheck
import Monticulo

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función 
--    mayor :: Ord a => Monticulo a -> a
-- tal que (mayor m) es el mayor elemento del montículo m. Por ejemplo,
--    mayor (foldr inserta vacio [6,8,4,1])  ==  8
-- ---------------------------------------------------------------------

mayor :: Ord a => Monticulo a -> a
mayor m | esVacio m = error "mayor: monticulo vacio"
        | otherwise = aux m (menor m)
        where aux m k | esVacio m = k
                      | otherwise = aux (resto m) (max k (menor m))

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función 
--    minMax :: Ord a => Monticulo a -> Maybe (a,a)
-- tal que (minMax m) es un par con el menor y el mayor elemento de m
-- si el montículo no es vacío. Por ejemplo,
--    minMax (foldr inserta vacio [6,1,4,8])  ==  Just (1,8)
--    minMax (foldr inserta vacio [6,8,4,1])  ==  Just (1,8)
--    minMax (foldr inserta vacio [7,5])      ==  Just (5,7)
-- ---------------------------------------------------------------------

minMax :: (Ord a) => Monticulo a -> Maybe (a,a)
minMax m | esVacio m = Nothing
         | otherwise = Just (menor m, mayor m) 

               
-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Consideremos el siguiente tipo de dato 
--    data Arbol a = H a | N (Arbol a) a (Arbol a)
-- y el siguiente ejemplo, 
--    ejArbol :: Arbol Int
--    ejArbol = N (N (H 1) 3 (H 4)) 5 (N (H 6) 7 (H 9))
-- 
-- Definir la función 
--     arbolMonticulo:: Ord t => Arbol t -> Monticulo t
-- tal que (arbolMonticulo a) es el montículo formado por los elementos
-- del árbol a. Por ejemplo, 
--    ghci> arbolMonticulo ejArbol
--    M 1 2 (M 4 1 (M 6 2 (M 9 1 Vacio Vacio) (M 7 1 Vacio Vacio)) Vacio) 
--          (M 3 1 (M 5 1 Vacio Vacio) Vacio)
-- ---------------------------------------------------------------------

data Arbol a = H a | N (Arbol a) a (Arbol a)

ejArbol :: Arbol Int
ejArbol = N (N (H 1) 3 (H 4)) 5 (N (H 6) 7 (H 9))

arbolMonticulo:: Ord t => Arbol t -> Monticulo t
arbolMonticulo = lista2Monticulo . arbol2Lista

-- (arbol2Lista a) es la lista de los valores del árbol a. Por ejemplo, 
--    arbol2Lista ejArbol  ==  [5,3,1,4,7,6,9]
arbol2Lista :: Arbol t -> [t]
arbol2Lista (H x)     = [x]
arbol2Lista (N i x d) = x : (arbol2Lista i ++ arbol2Lista d)

-- (lista2Monticulo xs) es el montículo correspondiente a la lista
-- xs. Por ejemplo,
--    ghci> lista2Monticulo [5,3,4,7]
--    M 3 2 (M 4 1 (M 7 1 Vacio Vacio) Vacio) (M 5 1 Vacio Vacio)
lista2Monticulo :: Ord t => [t] -> Monticulo t
lista2Monticulo = foldr inserta vacio 

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función 
--    minArbol :: Ord t => Arbol t -> t
-- tal que (minArbol a) es el menor elemento de a. Por ejemplo,
--    minArbol ejArbol == 1
-- ---------------------------------------------------------------------

minArbol :: Ord t => Arbol t -> t
minArbol = menor . arbolMonticulo

-- ---------------------------------------------------------------------
-- Ejercicio 3.1.  Consideremos los tipos de los vectores y las matrices
-- definidos por 
--    type Vector a = Array Int a
--    type Matriz a = Array (Int,Int) a
-- y los siguientes ejemplos
--    p1, p2, p3 :: Matriz Double
--    p1 = listArray ((1,1),(3,3)) [1.0,2,3,1,2,4,1,2,5]
--    p2 = listArray ((1,1),(3,3)) [1.0,2,3,1,3,4,1,2,5]
--    p3 = listArray ((1,1),(3,3)) [1.0,2,1,0,4,7,0,0,5]
-- 
-- Definir la función 
--    esTriangularS :: Num a => Matriz a -> Bool
-- tal que (esTriangularS p) se verifica si p es una matriz triangular
-- superior. Por ejemplo, 
--    esTriangularS p1 == False
--    esTriangularS p3 == True
-- ---------------------------------------------------------------------

type Vector a = Array Int a

type Matriz a = Array (Int,Int) a

p1, p2, p3:: Matriz Double
p1 = listArray ((1,1),(3,3)) [1.0,2,3,1,2,4,1,2,5]
p2 = listArray ((1,1),(3,3)) [1.0,2,3,1,3,4,1,2,5]
p3 = listArray ((1,1),(3,3)) [1.0,2,1,0,4,7,0,0,5]

esTriangularS:: Num a => Matriz a -> Bool
esTriangularS p = and [p!(i,j) == 0 | i <-[1..m], j <-[1..n], i > j]
    where (_,(m,n)) = bounds p

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    determinante:: Matriz Double -> Double
-- tal que (determinante p) es el determinante de la matriz p. Por
-- ejemplo, 
--    ghci> determinante (listArray ((1,1),(3,3)) [2,0,0,0,3,0,0,0,1])
--    6.0
--    ghci> determinante (listArray ((1,1),(3,3)) [1..9])
--    0.0
--    ghci> determinante (listArray ((1,1),(3,3)) [2,1,5,1,2,3,5,4,2])
--    -33.0
-- ---------------------------------------------------------------------

determinante:: Matriz Double -> Double
determinante p 
    | (m,n) == (1,1) = p!(1,1)
    | otherwise = 
        sum [((-1)^(i+1))*(p!(i,1))*determinante (submatriz i 1 p)
             | i <- [1..m]]
    where (_,(m,n)) = bounds p

-- (submatriz i j p) es la submatriz de p obtenida eliminado la fila i y
-- la columna j. Por ejemplo,
--    ghci> submatriz 2 3 (listArray ((1,1),(3,3)) [2,1,5,1,2,3,5,4,2])
--    array ((1,1),(2,2)) [((1,1),2),((1,2),1),((2,1),5),((2,2),4)]
--    ghci> submatriz 2 3 (listArray ((1,1),(3,3)) [1..9])
--    array ((1,1),(2,2)) [((1,1),1),((1,2),2),((2,1),7),((2,2),8)]
submatriz :: Num a => Int -> Int -> Matriz a -> Matriz a
submatriz i j p = 
    array ((1,1), (m-1,n -1))
          [((k,l), p ! f k l) | k <- [1..m-1], l <- [1..n-1]]
    where (_,(m,n)) = bounds p
          f k l | k < i  && l < j  = (k,l)
                | k >= i && l < j  = (k+1,l)
                | k < i  && l >= j = (k,l+1)
                | otherwise        = (k+1,l+1)

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. El número 22940075 tiene una curiosa propiedad. Si lo
-- factorizamos, obtenemos 22940075 = 5^2×229×4007. Reordenando y
-- concatenando los factores primos (5, 229, 40079 podemos obtener el
-- número original: 22940075. 
--  
-- Diremos que un número es especial si tiene esta propiedad.
-- 
-- Definir la función 
--    esEspecial :: Integer -> Bool
-- tal que (esEspecial n) se verifica si n es un número especial. Por
-- ejemplo, 
--    esEspecial 22940075  ==  True
--    esEspecial 22940076  ==  False
-- ---------------------------------------------------------------------

esEspecial :: Integer -> Bool
esEspecial n = 
    sort (concat (map cifras (nub (factorizacion n)))) == sort (cifras n)

-- (factorizacion n) es la lista de los factores de n. Por ejemplo,
--    factorizacion 22940075  ==  [5,5,229,4007]
factorizacion :: Integer -> [Integer]
factorizacion n | n == 1    = []
                | otherwise = x : factorizacion (div n x)
    where x = menorFactor n

-- (menorFactor n) es el menor factor primo de n. Por ejemplo,
--    menorFactor 22940075  ==  5
menorFactor :: Integer -> Integer
menorFactor n = head [x | x <- [2..], rem n x == 0]

-- (cifras n) es la lista de las cifras de n. Por ejemplo,
--    cifras 22940075  ==  [2,2,9,4,0,0,7,5]
cifras :: Integer -> [Integer]
cifras n = [read [x] | x <- show n]

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Comprobar con QuickCheck que todos los números primos
-- son especiales.
-- --------------------------------------------------------------------- 

-- La propiedad es
prop_Especial :: Integer -> Property
prop_Especial n =
    esPrimo m ==> esEspecial m
    where m = abs n

-- (esPrimo n) se verifica si n es primo. Por ejemplo,
--    esPrimo 7  ==  True
--    esPrimo 9  ==  False
esPrimo :: Integer -> Bool
esPrimo x = [y | y <- [1..x], x `rem` y == 0] == [1,x]
