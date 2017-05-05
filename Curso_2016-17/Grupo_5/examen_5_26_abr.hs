-- Informática (1º del Grado en Matemáticas)
-- 5º examen de evaluación continua (26 de abril de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Matrix
import qualified Data.Vector as V
import I1M.Pila
import I1M.PolOperaciones

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Un número natural x es un cuadrado perfecto si x = b^2
-- para algún número natural b <= x. Por ejemplo, 25 es un cuadrado
-- perfecto y 24 no lo es.
-- 
-- Un número natural se dirá equilibrado si contiene una cantidad par de
-- cifras pares y una cantidad impar de cifras impares. Por ejemplo,
-- 124, 333 y 20179 son equilibrados, mientras que 1246, 2333 y 2017 no
-- lo son.  
-- 
-- Definir la lista infinita
--    equilibradosPerf :: [Integer]
-- de todos los cuadrados perfectos que son equilibrados. Por ejemplo
--    ghci> take 15 equilibradosPerf
--    [1,9,100,144,225,256,289,324,441,625,676,784,841,900,10000]
-- ---------------------------------------------------------------------

equilibradosPerf :: [Integer]
equilibradosPerf = filter esEquilibrado cuadrados

-- cuadrados es la lista de los cuadrados perfectos. Por ejemplo,
--    take 10 cuadrados  ==  [0,1,4,9,16,25,36,49,64,81]
cuadrados :: [Integer]
cuadrados = [x^2 | x <- [0..]]

-- (esEquilibrado x) se verifica si x es equilibrado. Por ejemplo,
--    esEquilibrado 124   ==  True
--    esEquilibrado 1246  ==  False
esEquilibrado :: Integer -> Bool
esEquilibrado x = even a && odd (n-a) where
  cs = show x
  n  = length cs
  a  = length (filter (`elem` "02468") cs) 

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Calcular el mayor cuadrado perfecto equilibrado de 9
-- cifras. 
-- ---------------------------------------------------------------------

-- El cálculo es
--    ghci> last (takeWhile (<=999999999) equilibradosPerf)
--    999950884

-- ---------------------------------------------------------------------
-- Ejercicio 2. Representamos los árboles binarios con elementos en las
-- hojas y en los nodos mediante el tipo de dato 
--    data Arbol a = H a
--                 | N a (Arbol a) (Arbol a)
--                 deriving Show
-- Por ejemplo,
--    ejArbol :: Arbol Int
--    ejArbol = N 5 (N 2 (H 1) (H 2)) (N 3 (H 4) (H 2))
-- 
-- Definir la función
--    todasHojas :: (a -> Bool)->  Arbol a -> Bool
-- tal que (todasHojas p a) se verifica si todas las hojas del árbol a 
-- satisfacen el predicado p.  Por ejemplo,
--    todasHojas even ejArbol  ==  False
--    todasHojas (<5) ejArbol  ==  True
-- ---------------------------------------------------------------------

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
             deriving Show

ejArbol :: Arbol Int
ejArbol = N 5 (N 2 (H 1) (H 2)) (N 3 (H 4) (H 2))

todasHojas :: (a -> Bool) -> Arbol a -> Bool
todasHojas p (H x)     = p x
todasHojas p (N _ i d) = todasHojas p i && todasHojas p d

-- ---------------------------------------------------------------------
-- Ejercicio 3. Representamos las pilas mediante el TAD definido en la
-- librería I1M.Pila. Por ejemplo, 
-- 
-- Definir la función
--    elemPila :: Int -> Pila a -> Maybe a
-- tal que (elemPila k p) es el k-ésimo elemento de la pila p, si un tal
-- elemento existe y Nothing, en caso contrario. Por ejemplo, para la
-- pila definida por
--    ejPila :: Pila Int
--    ejPila = foldr apila vacia [2,4,6,8,10]
-- se tiene
--    elemPila 2 ejPila  ==   Just 4
--    elemPila 5 ejPila  ==   Just 10
--    elemPila 7 ejPila  ==   Nothing
-- ---------------------------------------------------------------------

ejPila :: Pila Int
ejPila = foldr apila vacia [2,4,6,8,10]

elemPila :: Int -> Pila a -> Maybe a
elemPila k p
  | esVacia p = Nothing
  | k <= 0    = Nothing
  | k == 1    = Just (cima p)
  | otherwise = elemPila (k-1) (desapila p)

-- -----------------------------------------------------------------------
-- Ejercicio 4. Representamos las matrices mediante la librería
-- Data.Matrix. Por ejemplo, 
--    ejMat :: Matrix Int
--    ejMat = fromLists [[1,2,3,0],[4,2,6,7],[7,5,1,11]]
-- representa la matriz
--    |1 2 3 0 |
--    |4 2 6 7 |
--    |7 5 1 11|
-- 
-- Definir la función
--   ampliada :: Ord a => Matrix a -> Matrix a
-- tal que (ampliada p) es la matriz obtenida al ampliar cada fila de p 
-- con sus elementos mínimo y máximo, respectivamente. Por ejemplo,
--    > ampliada ejMat
--    ( 1 2 3  0 0  3 )
--    ( 4 2 6  7 2  7 )
--    ( 7 5 1 11 1 11 )
-- ---------------------------------------------------------------------

ejMat :: Matrix Int
ejMat = fromLists [[1,2,3,0],[4,2,6,7],[7,5,1,11]]

ampliada :: Ord a => Matrix a -> Matrix a
ampliada p = matrix n (m+2) f where
  n = nrows p
  m = ncols p
  f (i,j) | j <= m   = p ! (i,j)
          | j == m+1 = minimum (fila i p)
          | j == m+2 = maximum (fila i p)
  fila i p = [p ! (i,k) | k <- [1..m]] 

-- 2ª definición
-- =============

ampliada2 :: Ord a => Matrix a -> Matrix a
ampliada2 p = p <|> minMax p

-- (minMax p) es la matriz cuyas filas son los mínimos y los máximos de
-- la matriz p. Por ejemplo,
--    ghci> minMax ejMat
--    (  0  3 )
--    (  2  7 )
--    (  1 11 )
minMax :: Ord a => Matrix a -> Matrix a
minMax p =
  fromLists [[V.minimum f, V.maximum f] | i <- [1..nrows p]
                                        , let f = getRow i p]
-- --------------------------------------------------------------------
-- Ejercicio 5.1. Representamos los polinomios mediante el TAD definido
-- en la librería I1M.Pol. Por ejemplo,
--    ejPol :: Polinomio Int
--    ejPol = consPol 4 5 (consPol 3 6 (consPol 2 (-1) (consPol 0 7 polCero)))
-- 
-- Definir la función
--    partePar :: (Eq a,Num a) => Polinomio a -> Polinomio a
-- tal que (partePar p) es el polinomio formado por los términos de
-- grado par del polinomio p. Por ejemplo, 
--   ghci> partePar ejPol
--   5*x^4 + -1*x^2 + 7
-- ---------------------------------------------------------------------

partePar :: (Eq a,Num a) => Polinomio a -> Polinomio a
partePar p 
  | esPolCero p = polCero
  | even n      = consPol n a (partePar r)
  | otherwise   = partePar r
  where n = grado p
        a = coefLider p
        r = restoPol p    

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir la función
--    generaPol :: [Int] -> Polinomio Int
-- tal que (generaPol xs) es el polinomio de coeficientes enteros de
-- grado la longitud de xs y cuyas raíces son los elementos de xs. Por
-- ejemplo, 
--    ghci> generaPol [1,-1]
--    x^2 + -1
--    ghci> generaPol [0,1,1]
--    x^3 + -2*x^2 + 1*x
--    ghci> generaPol [1,1,3,-4,5]
--    x^5 + -6*x^4 + -8*x^3 + 90*x^2 + -137*x + 60
-- ---------------------------------------------------------------------

-- 1ª solución
generaPol ::  [Int] -> Polinomio Int 
generaPol []     = polUnidad
generaPol (r:rs) = multPol (creaFactor r) (generaPol rs)
  where creaFactor r = consPol 1 1 (consPol 0 (-r) polCero)

-- 2ª solución
generaPol2 ::  [Int] -> Polinomio Int 
generaPol2 xs =
  foldr multPol polUnidad ps
  where ps = [consPol 1 1 (consPol 0 (-x) polCero) | x <- xs]
