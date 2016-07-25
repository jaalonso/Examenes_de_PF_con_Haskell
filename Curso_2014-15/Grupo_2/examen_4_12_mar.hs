-- Informática (1º del Grado en Matemáticas)
-- 4º examen de evaluación continua (12 de marzo de 2015)            
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Librerías auxiliares
-- ---------------------------------------------------------------------

import Data.Array
import Data.List 
import I1M.PolOperaciones
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Dado un polinomio p con coeficientes enteros, se
-- llama parejo si está formado exclusivamente por monomios de grado par.
-- 
-- Definir la funcion
--    parejo :: Polinomio Int -> Bool
-- tal que (parejo p) se verifica si el polinomio p es parejo. Por
-- ejemplo, 
--    ghci> let p1 = consPol 3 2 (consPol 4 1 polCero)
--    ghci> parejo p1 
--    False
--    ghci> let p2 = consPol 6 3 (consPol 4 1 (consPol 0 5  polCero))
--    ghci> parejo p2 
--    True
-- ---------------------------------------------------------------------

parejo :: Polinomio Int -> Bool
parejo p = all even (grados p)

grados p | esPolCero p = [0]
         | otherwise   = grado p : grados (restoPol p)

-- ---------------------------------------------------------------------
-- Ejercicio 2 . Las matrices pueden representarse mediante tablas cuyos
-- indices son pares de numeros naturales:    
--    type Matriz a = Array (Int,Int) a
--
-- Definir la funcion 
--    mayorElem :: Matriz -> Matriz
-- tal que (mayorElem p) es la matriz obtenida añadiéndole al principio
-- una columna con el mayor elemento de cada fila. Por ejemplo,
-- aplicando mayorElem a las matrices 
--    |1 8 3|      |1 2|
--    |4 5 6|      |7 4|
--                 |5 6|
-- se obtienen, respectivamente
--    |8 1 8 3|   |2 1 2|
--    |6 4 5 6|   |7 7 4|
--                |6 5 6|
-- En Haskell,
--    ghci> mayorElem (listArray ((1,1),(2,3)) [1,8,3, 4,5,6])
--          array ((1,1),(2,4))
--          [((1,1),8),((1,2),1),((1,3),8),((1,4),3),
--           ((2,1),6),((2,2),4),((2,3),5),((2,4),6)]
--    ghci> mayorElem (listArray ((1,1),(3,2)) [1,2, 7,4, 5,6])
--          array ((1,1),(3,3)) 
--          [((1,1),2),((1,2),1),((1,3),2),
--           ((2,1),7),((2,2),7),((2,3),4),
--           ((3,1),6),((3,2),5),((3,3),6)]
-- ---------------------------------------------------------------------

type Matriz a = Array (Int,Int) a

mayorElem :: Matriz  Int -> Matriz Int
mayorElem p = listArray ((1,1),(m,n+1))
                        [f i j | i <- [1..m], j <- [1..n+1]]
    where 
      m = fst(snd(bounds p)) 
      n = snd(snd(bounds p)) 
      f i j | j > 1   = p ! (i,j-1)
            | j==1    =  maximum [p!(i,j)|j<- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la sucesion (infinita) 
--    numerosConDigitosPrimos :: [Int]
-- tal que sus elementos son los números enteros positivos con todos sus
-- dígitos primos. Por ejemplo,
--    ghci> take 22 numerosConDigitosPrimos
--    [2,3,5,7,22,23,25,27,32,33,35,37,52,53,55,57,72,73,75,77,222,223]
-- ---------------------------------------------------------------------

numerosConDigitosPrimos :: [Int]
numerosConDigitosPrimos = 
    [n | n <- [2..], digitosPrimos n]

-- (digitosPrimos n) se verifica si todos los digitos de n son
-- primos. Por ejemplo,
--    digitosPrimos 352  ==  True
--    digitosPrimos 362  ==  False
digitosPrimos :: Int -> Bool
digitosPrimos n = all (`elem` "2357") (show n)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función 
--    alterna ::  Int -> Int -> Matriz Int
-- tal que (alterna n x) es la matriz de dimensiones nxn que contiene el
-- valor x alternado con 0 en todas sus posiciones. Por ejemplo,
--    ghci> alterna 4 2
--    array ((1,1),(4,4)) [((1,1),2),((1,2),0),((1,3),2),((1,4),0),
--                         ((2,1),0),((2,2),2),((2,3),0),((2,4),2),
--                         ((3,1),2),((3,2),0),((3,3),2),((3,4),0),
--                         ((4,1),0),((4,2),2),((4,3),0),((4,4),2)]
--    ghci>alterna 3 1
--    array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),1),
--                         ((2,1),0),((2,2),1),((2,3),0),
--                         ((3,1),1),((3,2),0),((3,3),1)]
-- ---------------------------------------------------------------------

alterna ::  Int -> Int -> Matriz Int
alterna n x =
    array ((1,1),(n,n)) [((i,j),f i j) | i <- [1..n], j <- [1..n]]
    where f i j | even (i+j) = x
                | otherwise  = 0

-- ---------------------------------------------------------------------
-- Ejercicio 5. Los árboles binarios con datos en nodos y hojas se
-- define por
--    data Arbol a = H a | N a (Arbol a) (Arbol a) deriving Show
-- Por ejemplo, el árbol 
--           3
--          / \
--         /   \
--        4     7
--       / \   / \
--      5   0 0   3
--     / \
--    2   0   
-- se representa por
--    ejArbol :: Arbol Integer
--    ejArbol = N 3 (N 4 (N 5 (H 2)(H 0)) (H 0)) (N 7 (H 0) (H 3))
-- 
-- Definir la función 
--    caminos :: Eq a => a -> Arbol a -> [[a]]
-- tal que (caminos x ar) es la lista de caminos en el arbol ar hasta
-- llegar a x. Por ejemplo
--    caminos 0 ejArbol  ==  [[3,4,5,0],[3,4,0],[3,7,0]]
--    caminos 3 ejArbol  ==  [[3],[3,7,3]]
--    caminos 1 ejArbol  ==  []
-- ---------------------------------------------------------------------

data Arbol a = H a | N a (Arbol a) (Arbol a) deriving Show

ejArbol :: Arbol Integer
ejArbol = N 3 (N 4 (N 5 (H 2)(H 0)) (H 0)) (N 7 (H 0) (H 3))

caminos :: Eq a => a -> Arbol a -> [[a]]
caminos x (H y) | x == y    = [[y]] 
                | otherwise = []
caminos x (N y i d) 
    | x == y    = [y] : [y:xs | xs <- caminos x i ++ caminos x d ]
    | otherwise = [y:xs | xs <- caminos x i ++ caminos x d]

