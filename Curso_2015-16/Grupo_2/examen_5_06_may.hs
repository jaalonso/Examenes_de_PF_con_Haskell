-- Informática (1º del Grado en Matemáticas, Grupo 2)
-- 5º examen de evaluación continua (6 de mayo de 2016)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Librerías auxiliares                                               --
-- ---------------------------------------------------------------------

import Data.List (nub, isPrefixOf)
import Data.Array
import Test.QuickCheck
import qualified Data.Matrix as M
import I1M.Grafo

-- ---------------------------------------------------------------------
-- Ejercicio 1. Los números ondulados son aquellos tales que sólo tienen
-- dos dígitos distintos como máximo y los dígitos consecutivos son
-- iguales. Por ejemplo, 23232, 8989, 363, 38 son ondulados. 
-- 
-- Definir la función 
--    ondulado :: Integer -> Bool 
-- tal que (ondulado n) se verifica si n es ondulado. Por ejemplo,
--    ondulado 12     = True
--    ondulado 312    = False
--    ondulado 313    = True
--    ondulado 313131 = True
--    ondulado 54543  = False
--    ondulado 3      = True
-- ---------------------------------------------------------------

ondulado :: Integer -> Bool 
ondulado n = 
    length (nub xs) <= 2 && 
    xs `isPrefixOf` (cycle (take 2 xs))
    where xs = show n

-- ---------------------------------------------------------------
-- Ejercicio 2. Definir la función 
--    onda :: Integer -> Array (Int,Int) Integer
-- o bien con la librería Data.Matrix
--    onda :: Integer -> M.Matrix Integer
-- tal que, para un numero ondulado m, (onda m) es la matriz que se  
-- obtiene con los digitos de m como en los ejemplos siguientes.
-- (onda 46464)  y (onda 323) seran las matrices cuadradas:
-- 4 6 4 6 4          3 2 3
-- 4 6 4 6 4          3 2 3
-- 4 6 4 6 4          3 2 3
-- 4 6 4 6 4
-- 4 6 4 6 4
-- Con Data.Array:
-- Main> onda 323 = array ((1,1),(3,3))
-- [((1,1),3),((1,2),2),((1,3),3),((2,1),3),((2,2),2),((2,3),3),
-- ((3,1),3),((3,2),2),((3,3),3)]
-- Con Data.Matrix:
-- Main> onda 323
-- ( 3 2 3 )
-- ( 3 2 3 )
-- ( 3 2 3 )
-- SOLUCION:
-- CON Data.Array
onda:: Integer -> Array (Int,Int) Integer
onda = undefined
-- CON Data.Matrix
-- onda :: Integer -> M.Matrix Integer
-- onda n = undefined

-- ---------------------------------------------------------------
-- Ejercicio 3.1. Definir la función 
--    esOnda :: Array (Int,Int) Integer -> Bool
-- de manera que, para una matriz m,  (esOnda m) se verifica si m está
-- generada por un numero ondulado. 
-- SOLUCION:
esOnda :: Array (Int,Int) Integer -> Bool
esOnda = undefined

-- ---------------------------------------------------------------
-- Ejercicio 3.2. Define una propiedad 
--    prop_ondas :: Integer -> Bool
-- que compruebe con quickCheck que toda matriz generada por un 
-- numero ondulado es una onda.
prop_ondas :: Integer -> Bool
prop_ondas = undefined

-- ----------------------------------------------------------------
-- Ejercicio 4. Los árboles binarios con datos en los nodos y hojas
-- se definen por el tipo:
-- data Arbol a = H 
--              | N a (Arbol a) (Arbol a) 
--                deriving (Eq, Show)
-- Por ejemplo, el árbol
--
--        3
--       / \
--      /   \
--     4     7
--    / \   / \
--   5   0 0   3
--  / \
-- 2   0
-- se representa por
-- ejArbol :: Arbol Integer
-- ejArbol = N 3 (N 4 (N 5 (H 2)(H 0)) (H 0)) (N 7 (H 0) (H 3))
-- Si asociamos a cada elemento del árbol anterior su profundidad dentro
-- del mismo, se obtiene el árbol siguiente
--
-- profundidad 0:           (3,0)
--                          /  \
--                         /    \
--                        /      \  
-- profundidad 1:      (4,1)    (7,1)
--                      / \      /  \
-- profundidad 2:    (5,2)(0,2)(0,2)(3,2)
--                    / \
-- profundidad 3: (2,3) (0,3)
-- Definir la función
-- profArbol :: Arbol a -> Arbol (a,Int)
-- tal que (profArbol x) es el árbol obtenido asociando a los elementos de
-- x su profundidad. Por ejemplo, 
--   Main> propArbol ejArbol
--    N (3,0) 
--     (N (4,1) 
--        (N (5,2) (H (2,3)) (H (0,3))) 
--        (H (0,2))) 
--     (N (7,1) (H (0,2)) (H (3,2)))
-- SOLUCION:
data Arbol a = H a 
             | N a (Arbol a) (Arbol a) 
             deriving (Eq, Show)
ejArbol :: Arbol Integer
ejArbol = N 3 (N 4 (N 5 (H 2)(H 0)) (H 0)) (N 7 (H 0) (H 3))
 
profArbol :: Arbol a -> Arbol (a,Int)
profArbol = undefined

-- --------------------------------------------------------------
-- Ejercicio 5.1. Definir la función
--    esTrayecto :: (Ix v,Num p) => Grafo v p -> [v] -> Bool
-- tal que '(esTrayecto g vs)' se verifica si los vertices de la lista
-- 'vs'estan conectados sucesivamente por aristas en el grafo 'g'. 
-- Grafos de ejemplo.
g1,g2 :: Grafo Int Int
g1 = creaGrafo D (1,6) [(1,2,0),(1,3,0),(1,4,0),(3,6,0),
                        (5,4,0),(6,2,0),(6,5,0)]
g2 = creaGrafo D (1,6) [(1,3,2),(1,5,4),(3,5,6),(5,1,8),(5,5,10),
                        (2,4,1),(2,6,3),(4,6,5),(4,4,7),(6,4,9)]
--    esTrayecto g1 [1,3,6,2]  ==  True
--    esTrayecto g1 [1,3,5,4]  ==  False
--    esTrayecto g2 [1,3,5,5]  ==  True
-- --------------------------------------------------------------

esTrayecto :: (Ix v,Num p) => Grafo v p -> [v] -> Bool
esTrayecto = undefined
-- --------------------------------------------------------------
-- Ejercicio 5.2. Definir la función
--    costeT :: (Ix v,Num p) => Grafo v p -> [v] -> p
-- tal que '(costeT g vs)' es la suma de los pesos de las aristas del
-- trayecto 'vs' en el grafo 'g'. Si vs no es un trayecto, la función
-- dara el mensaje correspondiente.
-- Por ejemplo,
--    costeT g2 [1,3,5,1]  ==  16
--    costeT g1 [1,4,5,4] == *** Exception: No es un trayecto valido.
-- --------------------------------------------------------------

costeT :: (Ix v,Ord p, Num p) => Grafo v p -> [v] -> p
costeT = undefined
