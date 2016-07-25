-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 2º examen de evaluación continua (16 de marzo de 2010)
-- ---------------------------------------------------------------------
 
import Test.QuickCheck
import Data.Char
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Probar por inducción que para toda lista xs: 
--    length (reverse xs) = length xs 
-- 
-- Nota: Las definiciones recursivas de length y reverse son:
-- 
--    length [] = 0                        -- length.1
--    length (x:xs) = 1 + length xs        -- length.2
--    reverse [] = []                      -- reverse.1
--    reverse (x:xs) = reverse xs ++ [x]   -- reverse.2
-- ---------------------------------------------------------------------

{-
La demostración es por inducción en xs.

Base: Supongamos que xs = []. Entonces,
   length (reverse xs)  
   = length (reverse [])
   = length []                   [por reverse.1]
   = length xs.

Paso de inducción: Supongamos la hipótesis de inducción
   length (reverse xs) = length xs                 (H.I.)
y sea x un elemento cualquiera. Hay que demostrar que
   length (reverse (x:xs)) = length (x:xs)
En efecto,               
   length (reverse (x:xs))
   = length (reverse xs ++ [x])         [por reverse.2]
   = length (reverse xs) + length [x]
   = length xs + 1                      [por H.I.]
   = length (x:xs)                      [por length.2]  
-}

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir, por recursión, la función 
--    sumaVectores :: [Int] -> [Int] -> [Int]
-- tal que (sumaVectores v w) es la lista obtenida sumando los elementos
-- de v y w que ocupan las mismas posiciones. Por ejemplo,
--    sumaVectores [1,2,5,-6] [0,3,-2,9]  ==  [1,5,3,3]
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión)
sumaVectores :: [Int] -> [Int] -> [Int]
sumaVectores xs ys = [x+y | (x,y) <- zip xs ys]

-- 2ª definición (por recursión):
sumaVectores2 :: [Int] -> [Int] -> [Int]
sumaVectores2 [] _          = []
sumaVectores2 _ []          = []
sumaVectores2 (x:xs) (y:ys) = x+y : sumaVectores2 xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, por recursión, la función 
--    multPorEscalar :: Int -> [Int] -> [Int]
-- tal que (multPorEscalar x v) es la lista que resulta de multiplicar
-- todos los elementos de v por x. Por ejemplo,
--    multPorEscalar 4 [1,2,5,-6]  ==  [4,8,20,-24]
-- ---------------------------------------------------------------------

multPorEscalar :: Int -> [Int] -> [Int]
multPorEscalar _ [] = []
multPorEscalar n (x:xs) = n*x : multPorEscalar n xs

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Comprobar con QuickCheck que las operaciones
-- anteriores verifican la propiedad distributiva de multPorEscalar con
-- respecto a sumaVectores.  
-- ---------------------------------------------------------------------

-- La propiedad es
prop_distributiva :: Int -> [Int] -> [Int] -> Bool
prop_distributiva n xs ys =
    multPorEscalar n (sumaVectores xs ys) ==
    sumaVectores (multPorEscalar n xs) (multPorEscalar n ys)

-- La comprobación es
--    ghci> quickCheck prop_distributiva
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Probar, por inducción, la propiedad anterior.
-- ---------------------------------------------------------------------

{-
La demostración es por inducción en xs.

Base: Supongamos que xs = []. Entonces,
   multPorEscalar n (sumaVectores xs ys)
   = multPorEscalar n (sumaVectores [] ys)
   = multPorEscalar n []
   = []
   = sumaVectores [] (multPorEscalar n ys)
   = sumaVectores (multPorEscalar n []) (multPorEscalar n ys)
   = sumaVectores (multPorEscalar n xs) (multPorEscalar n ys)

Paso de inducción: Supongamos la hipótesis de inducción 
    multPorEscalar n (sumaVectores xs ys) 
    = sumaVectores (multPorEscalar n xs) (multPorEscalar n ys) (H.I. 1)
Hay que demostrar que 
    multPorEscalar n (sumaVectores (x:xs) ys) 
    = sumaVectores (multPorEscalar n (x:xs)) (multPorEscalar n ys) 
Lo haremos por casos en ys.

Caso 1: Supongamos que ys = []. Entonces,
   multPorEscalar n (sumaVectores xs ys)
   = multPorEscalar n (sumaVectores xs [])
   = multPorEscalar n []
   = []
   = sumaVectores (multPorEscalar n xs) []
   = sumaVectores (multPorEscalar n xs) (multPorEscalar n [])
   = sumaVectores (multPorEscalar n xs) (multPorEscalar n ys)

Caso 2: Para (y:ys). Entonces,
    multPorEscalar n (sumaVectores (x:xs) (y:ys)) 
    = multPorEscalar n (x+y : sumaVectores xs ys)  
         [por multPorEscalar.2]
    = n*(x+y) : multPorEscalar n (sumaVectores xs ys)
         [por multPorEscalar.2]
    = n*x+n*y : sumaVectores (multPorEscalar n xs) (multPorEscalar n ys) 
         [por H.I. 1]
    = sumaVectores (n*x : multPorEscalar n xs) (n*y : multPorEscalar n ys) 
         [por sumaVectores.2]
    = sumaVectores (multPorEscalar n (x:xs)) (multPorEscalar n (y:ys)) 
         [por multPorEscalar.2]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 3. Consideremos los árboles binarios definidos como sigue
--    data Arbol a = H 
--                 | N a (Arbol a) (Arbol a)
--                 deriving (Show, Eq)
-- 
-- Definir la función 
--    mapArbol :: (a -> b) -> Arbol a -> Arbol b
-- tal que (mapArbol f x) es el árbol que resulta de sustituir cada nodo
-- n del árbol x por (f n). Por ejemplo,
--    ghci> mapArbol (+1) (N 9 (N 3 (N 2 H H) (N 4 H H)) (N 7 H H))
--    N 10 (N 8 H H) (N 4 (N 5 H H) (N 3 H H))
-- ---------------------------------------------------------------------

data Arbol a = H 
             | N a (Arbol a) (Arbol a)
             deriving (Show, Eq)

mapArbol :: (a -> b) -> Arbol a -> Arbol b
mapArbol _ H         = H
mapArbol f (N x i d) = N (f x) (mapArbol f i) (mapArbol f d)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir, por comprensión, la función
--    mayorExpMenor :: Int -> Int -> Int
-- tal que (mayorExpMenor a b) es el menor n tal que a^n es mayor que
-- b. Por ejemplo,
--    mayorExpMenor 2 1000  ==  10
--    mayorExpMenor 9 7     ==   1
-- ---------------------------------------------------------------------

mayorExpMenor :: Int -> Int -> Int
mayorExpMenor a b =
    head [n | n <- [0..], a^n > b]

