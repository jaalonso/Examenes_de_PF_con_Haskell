-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 7º examen de evaluación continua (11 de junio de 2012)
-- ---------------------------------------------------------------------

import Data.List
import Data.Array
import Test.QuickCheck
import PolRepTDA
import GrafoConVectorDeAdyacencia
import ConjuntoConListasOrdenadasSinDuplicados

-- ---------------------------------------------------------------------
-- Ejercicio 1. Diremos que una lista de números es una reducción
-- general de un número entero N si el sumatorio de L es igual a N y,
-- además, L es una lista de números enteros consecutivos y ascendente,
-- con más de un elemento. Por ejemplo, las listas [1,2,3,4,5], [4,5,6]
-- y [7,8] son reducciones generales de 15 
-- 
-- Definir, por comprensión, la función
--    reduccionesBasicas :: Integer -> [[Integer]]
-- tal que (reduccionesBasicas n) es la lista de reducciones de n cuya
-- longitud (número de elementos) sea menor o igual que la raíz cuadrada
-- de n. Por ejemplo,
--    reduccionesBasicas 15   ==  [[4,5,6],[7,8]]
--    reduccionesBasicas 232  ==  []
-- ---------------------------------------------------------------------

-- 1ª definición:
reduccionesBasicasC :: Integer -> [[Integer]]
reduccionesBasicasC n = 
    [[i..j] | i <- [1..n-1], j <- [i+1..i+r-1], sum [i..j] == n]
    where r = truncate (sqrt (fromIntegral n))

-- 2ª definición:
reduccionesBasicasC2 :: Integer -> [[Integer]]
reduccionesBasicasC2 n = 
    [[i..j] | i <- [1..n-1], j <- [i+1..i+r-1], (i+j)*(1+j-i) `div` 2 == n]
    where r = truncate (sqrt (fromIntegral n))

-- ---------------------------------------------------------------------
-- Ejercicio 2. Dada una matriz numérica A de dimensiones (m,n) y una
-- matriz booleana B de las mismas dimensiones, y dos funciones f y g,
-- la transformada de A respecto de B, f y g es la matriz C (de las
-- mismas dimensiones), tal que, para cada celda (i,j): 
--    C(i,j) = f(A(i,j)) si B(i,j) es verdadero
--    C(i,j) = f(A(i,j)) si B(i,j) es falso
-- Por ejemplo, si A y B son las matrices
--    |1 2|   |True  False|  
--    |3 4|   |False True |
-- respectivamente, y f y g son dos funciones tales que f(x) = x+1 y
-- g(x) = 2*x, entonces la transformada de A respecto de B, f y g es
--    |2 4|
--    |6 5|
-- 
-- En Haskell,
--    a :: Array (Int,Int) Int
--    a = array ((1,1),(2,2)) [((1,1),1),((1,2),2),((2,1),3),((2,2),4)]
--    
--    b :: Array (Int,Int) Bool
--    b = array ((1,1),(2,2)) [((1,1),True),((1,2),False),((2,1),False),((2,2),True)]
--     
-- Definir la función
--    transformada :: Array (Int,Int) a -> Array (Int,Int) Bool -> 
--                    (a -> b) -> (a -> b) -> Array (Int,Int) b
-- tal que (transformada a b f g) es la transformada de A respecto de B,
-- f y g. Por ejemplo,
--    ghci> transformada a b (+1) (*2)
--    array ((1,1),(2,2)) [((1,1),2),((1,2),4),((2,1),6),((2,2),5)]
-- ---------------------------------------------------------------------

a :: Array (Int,Int) Int
a = array ((1,1),(2,2)) [((1,1),1),((1,2),2),((2,1),3),((2,2),4)]

b :: Array (Int,Int) Bool
b = array ((1,1),(2,2)) [((1,1),True),((1,2),False),((2,1),False),((2,2),True)]

transformada :: Array (Int,Int) a -> Array (Int,Int) Bool -> 
                (a -> b) -> (a -> b) -> Array (Int,Int) b
transformada a b f g = 
    array ((1,1),(m,n)) [((i,j),aplica i j) | i <- [1..m], j <- [1..m]]
    where (m,n) = snd (bounds a)
          aplica i j | b!(i,j)   = f (a!(i,j))
                     | otherwise = g (a!(i,j))

-- ---------------------------------------------------------------------
-- Ejercicio 3. Dado un grafo dirigido G, diremos que un nodo está
-- aislado si o bien de dicho nodo no sale ninguna arista o bien no
-- llega al nodo ninguna arista. Por ejemplo, en el siguiente grafo
-- (Tema 22, pag. 31) 
--    g = creaGrafo D (1,6) [(1,2,0),(1,3,0),(1,4,0),(3,6,0),
--                           (5,4,0),(6,2,0),(6,5,0)]
-- podemos ver que del nodo 1 salen 3 aristas pero no llega ninguna, por
-- lo que lo consideramos aislado. Así mismo, a los nodos 2 y 4 llegan
-- aristas pero no sale ninguna, por tanto también estarán aislados.
--
-- Definir la función 
--    aislados :: (Ix v, Num p) => Grafo v p -> [v]
-- tal que (aislados g) es la lista de nodos aislados del grafo g. Por
-- ejemplo, 
--    aislados g == [1,2,4]
-- ---------------------------------------------------------------------
                    
g = creaGrafo D (1,6) [(1,2,0),(1,3,0),(1,4,0),(3,6,0),
                       (5,4,0),(6,2,0),(6,5,0)]
    
aislados :: (Ix v, Num p) => Grafo v p -> [v]
aislados g = [n | n <- nodos g, adyacentes g n == [] || incidentes g n == [] ]

-- (incidentes g v) es la lista de los nodos incidentes con v en el
-- grafo g. Por ejemplo,
--    incidentes g 2  ==  [1,6]
--    incidentes g 1  ==  []
incidentes :: (Ix v,Num p) => Grafo v p -> v -> [v]
incidentes g v = [x | x <- nodos g, v `elem` adyacentes g x]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    gradosCoeficientes :: (Ord a, Num a) => 
--                          [Polinomio a] -> [(Int,Conj a)]
-- tal que (gradosCoeficientes ps) es una lista de pares, donde cada par
-- de la lista contendrá como primer elemento un número entero
-- (correspondiente a un grado) y el segundo elemento será un conjunto
-- que contendrá todos los coeficientes distintos de 0 que aparecen para
-- dicho grado en la lista de polinomios ps. Esta lista estará 
-- ordenada de menor a mayor para todos los grados posibles de la lista de 
-- polinomios. Por ejemplo, dados los siguientes polinomios
--    p1, p2, p3, p4 :: Polinomio Int
--    p1 = consPol 5 2 (consPol 3 (-1) polCero)
--    p2 = consPol 7 (-2) (consPol 5 1 (consPol 4 5 (consPol 2 1 polCero)))
--    p3 = polCero
--    p4 = consPol 4 (-1) (consPol 3 2 (consPol 1 1 polCero))
-- se tiene que
--    ghci> gradosCoeficientes [p1,p2,p3,p4]
--    [(1,{0,1}),(2,{0,1}),(3,{-1,0,2}),(4,{-1,0,5}),(5,{0,1,2}),
--     (6,{0}),(7,{-2,0})]
-- ---------------------------------------------------------------------

p1, p2, p3, p4 :: Polinomio Int
p1 = consPol 5 2 (consPol 3 (-1) polCero)
p2 = consPol 7 (-2) (consPol 5 1 (consPol 4 5 (consPol 2 1 polCero)))
p3 = polCero
p4 = consPol 4 (-1) (consPol 3 2 (consPol 1 1 polCero))

gradosCoeficientes :: (Ord a, Num a) => [Polinomio a] -> [(Int,Conj a)]
gradosCoeficientes ps = 
    [(k,foldr (inserta . coeficiente k) vacio ps) | k <- [1..m]]
    where m = maximum (map grado ps)

-- (coeficiente k p) es el coeficiente del término de grado k
-- del polinomio p. Por ejemplo,
--    let pol = consPol 5 2 (consPol 3 1 (consPol 0 (-1) polCero))
--    pol                ==  2*x^5 + x^3 + -1
--    coeficiente 5 pol  ==  2
--    coeficiente 6 pol  ==  0
--    coeficiente 4 pol  ==  0
--    coeficiente 3 pol  ==  1
coeficiente :: Num a => Int -> Polinomio a -> a
coeficiente k p | k > n     = 0
                | k == n    = c
                | otherwise = coeficiente k r
                where n = grado p
                      c = coefLider p
                      r = restoPol p
                      


