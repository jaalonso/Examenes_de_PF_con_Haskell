-- Informática (1º del Grado en Matemáticas, Grupo 2)
-- 5º examen de evaluación continua (6 de mayo de 2016)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Librerías auxiliares                                               --
-- ---------------------------------------------------------------------

import Data.List (nub, isPrefixOf)
import Data.Ix
import Test.QuickCheck
import Data.Matrix
import I1M.Grafo

-- ---------------------------------------------------------------------
-- Ejercicio 1. Los números ondulados son aquellos tales que sólo tienen
-- dos dígitos distintos como máximo que se repiten periódicamente. Por
-- ejemplo, 23232, 8989, 363, 38 son ondulados.  
-- 
-- Definir la función 
--    esOndulado :: Integer -> Bool 
-- tal que (esOndulado n) se verifica si n es ondulado. Por ejemplo,
--    esOndulado 12     = True
--    esOndulado 312    = False
--    esOndulado 313    = True
--    esOndulado 313131 = True
--    esOndulado 54543  = False
--    esOndulado 3      = True
-- ---------------------------------------------------------------

esOndulado :: Integer -> Bool 
esOndulado n = 
  length (nub xs) <= 2 && 
  xs `isPrefixOf` (cycle (take 2 xs))
  where xs = show n

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la funcion 
--    onda :: Integer -> Matrix Integer
-- tal que (onda x) es la matriz cuadrada cuyas filas son los dígitos de
-- x. Por ejemplo,
--    ghci> onda 323
--    ( 3 2 3 )
--    ( 3 2 3 )
--    ( 3 2 3 )
--    
--    ghci> onda 46464
--    ( 4 6 4 6 4 )
--    ( 4 6 4 6 4 )
--    ( 4 6 4 6 4 )
--    ( 4 6 4 6 4 )
--    ( 4 6 4 6 4 )
-- ---------------------------------------------------------------------

onda :: Integer -> Matrix Integer
onda x = fromLists (replicate n ds)
  where ds = digitos x
        n  = length ds

digitos :: Integer -> [Integer]
digitos n = [read [c] | c <- show n]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la funcion 
--    esOnda :: Matrix Integer -> Bool
-- tal que (esOnda p) se verifica si es existe un número ondulado x tal
-- que p es (onda x). Por ejemplo. 
--    esOnda (fromLists [[3,2,3],[3,2,3],[3,2,3]])  ==  True
--    esOnda (fromLists [[3,2,2],[3,2,2],[3,2,2]])  ==  False
--    esOnda (fromLists [[3,2,3],[3,2,3]])          ==  False
-- ---------------------------------------------------------------------

esOnda :: Matrix Integer -> Bool
esOnda p = esOndulado x && p == onda x
  where x = digitosAnumero [p!(1,j) | j <- [1..ncols p]]

-- (digitosAnumero xs) es el número cuyos dígitos son los elementos de
-- xs. Por ejemplo,
--    digitosAnumero [3,2,5]  ==  325
digitosAnumero :: [Integer] -> Integer
digitosAnumero xs =
  read (concatMap show xs)

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con QuickCheck que toda matriz generada por
-- un número  onduladoa es una onda.
-- ---------------------------------------------------------------------

-- ondulado es un generador de números ondulados. Por ejemplo, 
--    ghci> sample genOndulado
--    2
--    757
--    16161
--    6
--    86
--    686
--    4646
--    5959595
--    56565656565
--    2929292929292
--    8585858585
ondulado :: Gen Integer
ondulado = do
  x <- choose (1,9)
  y <- choose (0,9)
  n <- arbitrary
  return (digitosAnumero (take (1 + abs n) (cycle [x,y])))

-- La propiedad es  
prop_ondas :: Property
prop_ondas =
  forAll ondulado (\x -> esOnda (onda x))

-- La comprobación es
--    ghci> quickCheck prop_ondas
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4. Los árboles binarios con datos en los nodos y hojas se
-- definen por 
--    data Arbol a = H 
--                 | N a (Arbol a) (Arbol a) 
--                   deriving (Eq, Show)
-- Por ejemplo, el árbol
--    
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
-- Si asociamos a cada elemento del árbol anterior su profundidad dentro
-- del mismo, se obtiene el árbol siguiente
--    
--    profundidad 0:          (3,0)
--                             /  \
--                            /    \
--                           /      \  
--    profundidad 1:      (4,1)    (7,1)
--                         / \      /  \
--    profundidad 2:    (5,2)(0,2)(0,2)(3,2)
--                       / \
--    profundidad 3: (2,3) (0,3)
-- 
-- Definir la función
--    profArbol :: Arbol a -> Arbol (a,Int)
-- tal que (profArbol x) es el árbol obtenido asociando los elementos de
-- x a su profundidad. Por ejemplo, 
--   ghci> profArbol ejArbol
--   N (3,0) 
--    (N (4,1) 
--       (N (5,2) (H (2,3)) (H (0,3))) 
--       (H (0,2))) 
--    (N (7,1) (H (0,2)) (H (3,2)))
-- ---------------------------------------------------------------------

data Arbol a = H a 
             | N a (Arbol a) (Arbol a) 
             deriving (Eq, Show)

ejArbol :: Arbol Integer
ejArbol = N 3 (N 4 (N 5 (H 2)(H 0)) (H 0)) (N 7 (H 0) (H 3))
 
profArbol :: Arbol a -> Arbol (a,Int)
profArbol (H x)     = H (x,0)
profArbol (N x i d) = aux (N x i d) 0
  where aux (H x)     n = H (x,n)
        aux (N x i d) n = N (x,n) (aux i (n+1)) (aux d (n+1))  

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Dado un grafo no dirigido G, un camino en G es una
-- secuencia de nodos [v(1),v(2),v(3),...,v(n)] tal que para todo i
-- entre 1 y n-1, (v(i),v(i+1)) es una arista de G. Por ejemplo, dados
-- los grafos  
--    g1 = creaGrafo ND (1,3) [(1,2,3),(1,3,2),(2,3,5)]
--    g2 = creaGrafo ND (1,4) [(1,2,3),(1,3,2),(1,4,5),(2,4,7),(3,4,0)]
-- la lista [1,2,3] es un camino en g1, pero no es un camino en g2
-- puesto que la arista (2,3) no existe en g2. 
--
-- Definir la función
--    camino :: (Ix a, Num t) => (Grafo a t) -> [a] -> Bool
-- tal que (camino g vs) se verifica si la lista de nodos vs es un camino
-- en el grafo g. Por ejemplo, 
--    camino g1 [1,2,3]  ==  True
--    camino g2 [1,2,3]  ==  False
-- ---------------------------------------------------------------------

g1 = creaGrafo ND (1,3) [(1,2,3),(1,3,2),(2,3,5)]
g2 = creaGrafo ND (1,4) [(1,2,3),(1,3,2),(1,4,5),(2,4,7),(3,4,0)]

camino :: (Ix a, Num t) => (Grafo a t) -> [a] -> Bool
camino g vs = all (aristaEn g) (zip vs (tail vs))

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir la función
--    coste :: (Ix a, Num t) => (Grafo a t) -> [a] -> Maybe t
-- tal que (coste g vs) es la suma de los pesos de las aristas del
-- camino vs en el grafo g o Nothing, si vs no es un camino en g. Por
-- ejemplo, 
--    coste g1 [1,2,3]  ==  Just 8
--    coste g2 [1,2,3]  ==  Nothing
-- ----------------------------------------------------------------------------

coste :: (Ix a, Num t) => (Grafo a t) -> [a] -> Maybe t
coste g vs
  | camino g vs = Just (sum [peso x y g | (x,y) <- zip vs (tail vs)])
  | otherwise   = Nothing
  
