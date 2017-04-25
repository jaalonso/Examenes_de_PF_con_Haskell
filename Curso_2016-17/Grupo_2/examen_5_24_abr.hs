-- Informática (1º del Grado en Matemáticas - Grupo 2)
-- 5º examen de evaluación continua (24 de abril de 2017)               
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Array
import qualified Data.Matrix as M
import Data.Number.CReal
import I1M.PolOperaciones
import I1M.Grafo

-- ---------------------------------------------------------------------
-- Ejercicio 1. El cociente entre dos números se puede calcular con el
-- operador de división (/), obteniendo tantos decimales como nos
-- permita la representación de los números reales. Por ejemplo, 
--    123/11 = 11.181818181818182
--    123/13 = 9.461538461538462
--    123/15 = 8.2
--    123/17 = 7.235294117647059
--    123/19 = 6.473684210526316
--    
-- Definir la función:
--    division :: Integer -> Integer -> [Integer]
-- tal que (division n m) es la lista (posiblemente infinita) cuyo
-- primer elemento es el cociente entero de la división del número
-- natural n entre el número natural m; y los demás elementos son todas
-- las cifras de la parte decimal de la división de n entre m. Por
-- ejemplo,  
--    take 10 (division 123 11)  ==  [11,1,8,1,8,1,8,1,8,1]
--    take 10 (division 123 13)  ==  [9,4,6,1,5,3,8,4,6,1]
--    division 123 15            ==  [8,2]
--    take 30 (division 123 17)  ==
--       [7,2,3,5,2,9,4,1,1,7,6,4,7,0,5,8,8,2,3,5,2,9,4,1,1,7,6,4,7,0]
--    take 30 (division 123 19)  ==
--       [6,4,7,3,6,8,4,2,1,0,5,2,6,3,1,5,7,8,9,4,7,3,6,8,4,2,1,0,5,2]
-- ---------------------------------------------------------------------

division :: Integer -> Integer -> [Integer]
division x y 
  | r == 0    = [q]
  | otherwise = q : division (r*10) y
  where (q,r) = quotRem x y

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    mapPol :: (Num a, Eq a) => 
--              (a -> a) -> (Int -> Int) -> Polinomio a -> Polinomio a
-- tal que (mapPol f g p) es el polinomio construido a partir de los
-- términos del polinomio p, tomando como nuevos coeficientes el
-- resultado de evaluar la función f sobre los coeficientes de los
-- términos de p y tomando como nuevos grados el resultado de evaluar la
-- función g sobre los grados de los términos de p. Por ejemplo, si p1
-- es el polinomio definido por
--    p1 :: Polinomio Int
--    p1 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
-- entonces
--    p1                         =>  3*x^4 + -5*x^2 + 3
--    mapPol (+1) (+1) p1        =>  4*x^5 + -4*x^3 + 4*x
--    mapPol (+2) (*2) p1        =>  5*x^8 + -3*x^4 + 5
--    mapPol (+(-2)) (*2) p1     =>  x^8 + -7*x^4 + 1
--    mapPol (+(-2)) (*(-2)) p1  =>  -5
--    mapPol (+0) (*0) p1        =>  1
-- Nota: Si al aplicar una función al exponente el resultado es
-- negativo, se cambia el resultado por cero. 
-- ---------------------------------------------------------------------

p1 :: Polinomio Int
p1 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))

mapPol :: (Num a, Eq a) => 
          (a -> a) -> (Int -> Int) -> Polinomio a -> Polinomio a
mapPol f g p 
  | esPolCero p = polCero
  | otherwise   = consPol (max 0 (g n)) (f b) (mapPol f g r)
  where n = grado p
        b = coefLider p
        r = restoPol p

-- ---------------------------------------------------------------------
-- Ejercicio 3. Dado un grafo no dirigido G, decimos que tres vértices
-- (v1,v2,v3) de G forman un triángulo si hay en G una arista entre 
-- cada par de ellos; es decir, una arista de v1 a v2, otra de v2 a v3 y
-- una tercera de v3 a v1.
--
-- Definir la función
--    verticesSinTriangulo :: (Ix v,Num p) => Grafo v p -> [v]
-- tal que (verticesSinTriangulo g) es la lista de todos los vértices
-- del grafo g que no están en ningún triángulo. Por ejemplo, si g1 y g2
-- son los grafos definidos por
--    g1, g2 :: Grafo Int Int
--    g1 = creaGrafo ND (1,6) [(1,2,0),(1,3,0),(1,4,0),(3,6,0),
--                             (5,4,0),(6,2,0),(6,4,0),(6,5,0)]
--    g2 = creaGrafo ND (1,6) [(1,3,0),(1,5,0),(3,5,0),(5,6,0),
--                             (2,4,0),(2,6,0),(4,6,0)]
-- entonces,
--    verticesSinTriangulo g1  ==  [1,2,3]
--    verticesSinTriangulo g2  ==  []
-- ---------------------------------------------------------------------

g1, g2 :: Grafo Int Int
g1 = creaGrafo ND (1,6) [(1,2,0),(1,3,0),(1,4,0),(3,6,0),
                         (5,4,0),(6,2,0),(6,4,0),(6,5,0)]
g2 = creaGrafo ND (1,6) [(1,3,0),(1,5,0),(3,5,0),(5,6,0),
                         (2,4,0),(2,6,0),(4,6,0)]

verticesSinTriangulo :: (Ix v,Num p) => Grafo v p -> [v]
verticesSinTriangulo g =
  [x | x <- nodos g, sinTriangulo g x]

-- (sinTriangulo g x) se verifica si no hay ningún triángulo en el grafo
-- g que contenga al vértice x. Por ejemplo,
--    sinTriangulo g1 2  ==  True
--    sinTriangulo g1 5  ==  False
sinTriangulo :: (Ix v,Num p) => Grafo v p -> v -> Bool
sinTriangulo g x = null (triangulos g x)

-- (triangulos g x) es la lista de los pares de vértices (y,z) tales que
-- (x,y,z) es un triángulo en el grafo g. Por ejemplo,
--    triangulos g1 5  ==  [(6,4),(4,6)]
--    triangulos g1 2  ==  []
triangulos :: (Ix v,Num p) => Grafo v p -> v -> [(v,v)]
triangulos g x =
  [(y,z) | y <- ns
         , z <- ns
         , aristaEn g (y,z)]
  where ns = adyacentes g x

-- ---------------------------------------------------------------------
-- Ejercicio 4. El recorrido en ZigZag de una matriz consiste en pasar
-- de la primera fila hasta la última, de izquierda a derecha en las 
-- filas impares y de derecha a izquierda en las filas pares, como se
-- indica en la figura.
--
--          /             \
--          | 1 -> 2 -> 3 |
--          |           | |
--          |           v |
--          | 4 <- 5 <- 6 |   =>  Recorrido ZigZag: [1,2,3,6,5,4,7,8,9]
--          | |           |
--          | v           |
--          | 7 -> 8 -> 9 |
--          \             /
--
-- Definir la función
--    recorridoZigZag :: M.Matrix a -> [a]
-- tal que (recorridoZigZag m) es la lista con los elementos de la
-- matriz m cuando se recorre esta en ZigZag. Por ejemplo,
--    ghci> recorridoZigZag (M.fromLists [[1,2,3],[4,5,6],[7,8,9]])
--    [1,2,3,6,5,4,7,8,9]
--    ghci> recorridoZigZag (M.fromLists [[1,2],[3,4],[5,6],[7,8]])
--    [1,2,4,3,5,6,8,7]
--    ghci> recorridoZigZag (M.fromLists [[1,2,3,4],[5,6,7,8],[9,10,11,12]])
--    [1,2,3,4,8,7,6,5,9,10,11,12]
-- ---------------------------------------------------------------------

recorridoZigZag :: M.Matrix a -> [a]
recorridoZigZag m =
  concat [f xs | (f,xs) <- zip (cycle [id,reverse]) (M.toLists m)]
