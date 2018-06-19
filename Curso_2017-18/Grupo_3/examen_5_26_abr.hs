-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 5º examen de evaluación continua (26 de abril de 2018)               
-- =====================================================================

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Array
import Data.List
import I1M.Pol
import I1M.Grafo
-- import GrafoConMatrizDeAdyacencia

-- ---------------------------------------------------------------------
-- Ejercicio 1. La secuencia de Thue-Morse es una secuencia binaria
-- (formada por ceros y unos) infinita tal que el término n-ésimo se
-- define de forma recursiva de la siguiente forma:
--    d(0)    = 0
--    d(2n)   = d(n)
--    d(2n+1) = 1-d(n)
-- Por tanto, está secuencia comienza de la siguiente forma:
--    "0110100110010110100101100110100110010110..."
--
-- Definir la constante
--   thueMorse :: String
-- cuyo valor es la secuencia infinita de Thue-Morse. Por ejemplo,
--   take 10 thueMorse  ==  "0110100110"
--   take 40 thueMorse  ==  "0110100110010110100101100110100110010110"
-- ----------------------------------------------------------------------------

complementario :: Char -> Char
complementario '0' = '1'
complementario '1' = '0'

thueMorse :: String
thueMorse = map thueMorseAux [0..]

thueMorseAux :: Int -> Char
thueMorseAux 0 = '0'
thueMorseAux n
  | even n    = thueMorseAux (n `div` 2)
  | otherwise = complementario (thueMorseAux (n `div` 2))

-- ---------------------------------------------------------------------
-- Ejercicio 2. Los árboles genéricos se pueden representar con el
-- tipo de dato algebraico
--    data ArbolG t = NG t [ArbolG t]
--                    deriving (Show, Eq)
-- en la que un árbol se representa con el constructor NG seguido del
-- valor que tiene el nodo raíz del árbol y de la lista de las
-- representaciones de sus árboles hijos.
--
-- Por ejemplo, los árboles
--       3              3
--      /|\            /|\
--     / | \          / | \
--    2  4  2        2  4  3
--         / \         /|\ 
--        3   1       3 5 5
-- se representan por
--    a1, a2 :: ArbolG Int
--    a1 = NG 3 [NG 2 [], NG 4 [], NG 2 [NG 3 [], NG 1 []]]
--    a2 = NG 3 [NG 2 [], NG 4 [NG 3 [], NG 5 [], NG 5 []], NG 3 []]
--
-- En particular, en esta representación una hoja es un árbol genérico
-- con una lista vacía de hijos: NG x [].
--
-- Un árbol genérico es de salto fijo si el valor absoluto de la
-- diferencia de los elementos adyacentes (es decir, entre cualquier
-- nodo y cualquiera de sus hijos) es siempre la misma. Por ejemplo, el
-- árbol a1 es de salto fijo ya que el valor absoluto de sus pares de
-- elementos adyacentes son  
--    |3-2| = |3-4| = |3-2| = |2-3| = |2-1| = 1
-- En cambio, el árbol a2 no es de salto fijo ya que el nodo raíz tiene
-- dos hijos con los que la diferencia en valor absoluto no es la misma
--    |3-2| = 1 =/= 0 = |3-3|
--
-- Definir la función
--    esSaltoFijo :: (Num a, Eq a) => Arbol a -> Bool
-- tal que (esSaltoFijo a) se verifica si el árbol a es de salto fijo. Por 
-- ejemplo,
--    esSaltoFijo a1  ==  True
--    esSaltoFijo a2  ==  False
-- ----------------------------------------------------------------------------

data ArbolG t = NG t [ArbolG t]
  deriving (Show, Eq)

a1, a2 :: ArbolG Int
a1 = NG 3 [NG 2 [], NG 4 [], NG 2 [NG 3 [], NG 1 []]]
a2 = NG 3 [NG 2 [], NG 4 [NG 3 [], NG 5 [], NG 5 []], NG 3 []]

esSaltoFijo :: (Num a, Eq a) => ArbolG a -> Bool
esSaltoFijo a =
  all (==1) (listaDiferencias a)

listaDiferencias :: (Num a, Eq a) => ArbolG a -> [a]
listaDiferencias (NG v []) = []
listaDiferencias (NG v ns) =
  map (\ (NG w _) -> abs (v-w)) ns ++
  concatMap listaDiferencias ns

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Decimos que un polinomio P es regresivo si está formado
-- por un único monomio o todo monomio no nulo de P es un múltiplo
-- entero (por un monomio de coefiente entero y grado positivo) del
-- monomio no nulo de grado inmediatamente inferior. Por ejemplo, 
-- + El polinomio 3x^3 es regresivo.
-- + El polinomio 6x^2 + 3x + 1 es regresivo pues el monomio 6x² es un
--   múltiplo entero de 3x (6x^2 = 2x * 3x) y éste es un múltiplo entero
--   de 1 (3x = 3x * 1).
-- + El polinomio 6x^4 - 2x^2 - 2 es regresivo pues el monomio 6x^4 es
--   un múltiplo entero de -2x^2 (6x^4 = (-2x^2) * (-2x^2)) y éste es un
--   múltiplo entero de -2 (-2x^2 = x^2 * (-2)).
-- + El polinomio 6x^3 + 3x^2 + 2 no es regresivo pues el monomio 3x^2
--   no es un múltiplo entero de 2 (3x^2 = 3/2x^2 * 2).
-- 
-- Definir la función
--    polinomioRegresivo :: Polinomio Int -> Bool
-- tal que (polinomioRegresivo p) se cumple si el polinomio p es regresivo.
-- Por ejemplo:
--    polinomioRegresivo p1  ==  True
--    polinomioRegresivo p2  ==  True
--    polinomioRegresivo p3  ==  True
--    polinomioRegresivo p4  ==  False
-- ----------------------------------------------------------------------------

p1, p2, p3, p4 :: Polinomio Int
p1 = consPol 3 3 polCero
p2 = foldr (\ (g,c) p -> consPol g c p) polCero [(2,6),(1,3),(0,1)]
p3 = foldr (\ (g,c) p -> consPol g c p) polCero [(4,6),(2,-2),(0,-2)]
p4 = foldr (\ (g,c) p -> consPol g c p) polCero [(3,6),(2,3),(0,2)]

polinomioRegresivo :: Polinomio Int -> Bool
polinomioRegresivo p =
  esPolCero p
  || esPolCero q
  || coefLider p `mod` coefLider q == 0 && polinomioRegresivo q
  where q = restoPol p

-- ----------------------------------------------------------------------------
-- Ejercicio 3.2. La regresión de un polinomio es el polinomio que se
-- obtiene haciendo el cociente entre monomios no nulos consecutivos. Si
-- un polinomio de coeficientes enteros es regresivo, entonces su
-- regresión también será un polinomio de coeficientes enteros. La
-- regresión de un polinomio formado por un único monomio es el
-- polinomio nulo. 
--
-- Definir la función
--    regresionPolinomio :: Polinomio Int -> Polinomio Int
-- tal que (regresionPolinomio p) es la regresión del polinomio regresivo
-- p. Por ejemplo,
--    regresionPolinomio p1  =>  0
--    regresionPolinomio p2  =>  5*x
--    regresionPolinomio p3  =>  - 2*x^2
-- ----------------------------------------------------------------------------

regresionPolinomio :: Polinomio Int -> Polinomio Int
regresionPolinomio p
  | esPolCero p || esPolCero q = polCero
  | otherwise = consPol (grado p - grado q)
                        (coefLider p `div` coefLider q)
                        (regresionPolinomio q)
  where q = restoPol p

-- ---------------------------------------------------------------------
-- Ejercicio 4. Dado un grafo dirigido G, su grafo moral es un grafo no
-- dirigido construido con el mismo conjunto de nodos y cuyas aristas
-- son las del grafo original G (consideradas sin dirección), añadiendo
-- una arista entre cada dos nodos distintos que tengan un hijo en común
-- en G. Por ejemplo, si consideramos los siguientes grafos:
--    g1 = creaGrafo D (1,3) [(1,3,0),(2,3,0)]
--    g2 = creaGrafo D (1,4) [(1,2,0),(2,4,0),(1,3,0),(3,4,0)]
--    g3 = creaGrafo D (1,4) [(1,4,0),(2,4,0),(3,4,0)]
-- + El grafo moral de g1 tiene las aristas originales junto con una
--   arista nueva que une los nodos 1 y 2.
-- + El grafo moral de g2 tiene las aristas originales junto con una
--   arista nueva que une los nodos 2 y 3.
-- + El grafo moral de g3 tiene las aristas originales junto con aristas
--   nuevas que unen los nodos 1 y 2; 1 y 3; y 2 y 3.
--
-- Definir la función
--   grafoMoral :: (Ix v, Num p, Eq p) => Grafo v p -> Grafo v p
-- tal que (grafoMoral g) es el grafo moral del grafo g. Por ejemplo,
--    λ> grafoMoral g1
--    G ND (array (1,3) [(1,[(3,0),(2,0)]),
--                       (2,[(1,0),(3,0)]),
--                       (3,[(1,0),(2,0)])])
--    λ> [(x,adyacentes (grafoMoral g1) x) | x <- nodos g1] 
--    [(1,[3,2]),(2,[1,3]),(3,[1,2])]
--    λ> [(x,adyacentes (grafoMoral g2) x) | x <- nodos g2] 
--    [(1,[2,3]),(2,[1,4,3]),(3,[1,2,4]),(4,[2,3])]
--    λ> [(x,adyacentes (grafoMoral g3) x) | x <- nodos g3] 
--    [(1,[4,2,3]),(2,[1,4,3]),(3,[1,2,4]),(4,[1,2,3])]
-- ----------------------------------------------------------------------------

g1, g2, g3 :: Grafo Int Int
g1 = creaGrafo D (1,3) [(1,3,0),(2,3,0)]
g2 = creaGrafo D (1,4) [(1,2,0),(2,4,0),(1,3,0),(3,4,0)]
g3 = creaGrafo D (1,4) [(1,4,0),(2,4,0),(3,4,0)]

grafoMoral :: (Ix v, Num p, Eq p) => Grafo v p -> Grafo v p
grafoMoral g =
  creaGrafo ND
            (minimum xs,maximum xs)
            (sinAristasSimetricas (aristas g `union` aristasMorales g))
  where xs = nodos g

aristasMorales :: (Ix v, Num p) => Grafo v p -> [(v,v,p)]
aristasMorales g = [(x,y,0) | x <- xs
                            , y <- xs
                            , x /= y
                            , hijoComun g x y]
  where xs = nodos g

hijoComun :: (Ix v, Num p) => Grafo v p -> v -> v -> Bool
hijoComun g x y =
  not (null (adyacentes g x `intersect` adyacentes g y))

-- (sinAristasSimetricas as) es la lista de aristas obtenida eliminando
-- las simétricas de as. Por ejemplo,
--    λ> sinAristasSimetricas [(1,3,0),(2,3,0),(1,2,0),(2,1,0)]
--    [(1,3,0),(2,3,0),(1,2,0)]
sinAristasSimetricas :: (Eq v, Eq p) => [(v,v,p)] -> [(v,v,p)]
sinAristasSimetricas [] = []
sinAristasSimetricas ((x,y,p):as) =
  (x,y,p) : sinAristasSimetricas (as \\ [(y,x,p)])
