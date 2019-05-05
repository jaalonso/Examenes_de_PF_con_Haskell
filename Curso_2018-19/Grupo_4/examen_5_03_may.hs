-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 5º examen de evaluación continua (3 de mayo de 2019)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Array as A
import Data.Map as M
import Data.Set as S
import I1M.Grafo
import System.Timeout

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. [1 punto] Dado 4 puntos de un círculo se pueden
-- dibujar 2 cuerdas entre ellos de forma que no se corten. En efecto,
-- si se enumeran los puntos del 1 al 4 en sentido de las agujas del
-- reloj una forma tiene las cuerdas {1-2, 3-4} y la otra {1-4, 2-3}.
--
-- Definir la función
--    numeroFormas :: Integer -> Integer
-- tal que (numeroFormas n) es el número de formas que se pueden dibujar
-- n cuerdas entre 2xn puntos de un círculo sin que se corten. Por
-- ejemplo, 
--    numeroFormas 1   ==  1
--    numeroFormas 2   ==  2
--    numeroFormas 4   ==  14
-- ---------------------------------------------------------------------

-- 1ª definición
numeroFormas :: Integer -> Integer
numeroFormas 0 = 0
numeroFormas n = aux (2*n)
  where aux 0 = 1
        aux 2 = 1
        aux i = sum [aux j * aux (i-2-j) | j <- [0,2..i-1]]

-- 2ª definición
numeroFormas2 :: Integer -> Integer
numeroFormas2 0 = 0
numeroFormas2 n = v A.! (2*n)
  where v   = array (0,2*n) [(i, f i) | i <- [0..2*n]]
        f 0 = 1
        f 2 = 1
        f i = sum [v A.! j * v A.! (i-2-j) | j <- [0,2..i-1]]

-- Comparación de eficiencia
-- =========================

--    λ> numeroFormas 15
--    9694845
--    (28.49 secs, 4,293,435,552 bytes)
--    λ> numeroFormas2 15
--    9694845
--    (0.01 secs, 184,552 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 1.2 [1.5 puntos]. Con la definición del apartado anterior,
-- evaluar (en menos de 2 segundos), el número de dígitos de
-- (numeroFormas 700); es decir, evaluar la siguiente expresión para que
-- de un valor distinto de Nothing
--    timeout (2*10^6) (return $! (length (show (numeroFormas 700))))
-- ---------------------------------------------------------------------

-- El cálculo es
--    λ> timeout (2*10^6) (return $! (length (show (numeroFormas 700))))
--    Just 417
--    (1.65 secs, 230,243,040 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2. [2.5 puntos]. Definir la función
--    mayoritarios :: Ord a => Set (Set a) -> [a]
-- tal que (mayoritarios f) es la lista de elementos que pertenecen al
-- menos a la mitad de los conjuntos de la familia f. Por ejemplo,
--    λ> mayoritarios (S.fromList [S.empty, S.fromList [1,3], S.fromList [3,5]])
--    [3]
--    λ> mayoritarios (S.fromList [S.empty, S.fromList [1,3], S.fromList [4,5]])
--    []
--    λ> mayoritarios (S.fromList [S.fromList [1..n] | n <- [1..7]])
--    [1,2,3,4]
--    λ> mayoritarios (S.fromList [S.fromList [1..n] | n <- [1..8]])
--    [1,2,3,4,5]
-- ---------------------------------------------------------------------

mayoritarios :: Ord a => Set (Set a) -> [a]
mayoritarios f =
  [x | x <- S.toList (elementosFamilia f)
     , nOcurrencias f x >= n]
  where n = (1 + S.size f) `div` 2

-- (elementosFamilia f) es el conjunto de los elementos de los elementos
-- de la familia f. Por ejemplo, 
--    λ> elementosFamilia (S.fromList [S.fromList [1,2], S.fromList [2,5]])
--    fromList [1,2,5]
elementosFamilia :: Ord a => Set (Set a) -> Set a
elementosFamilia = S.unions . S.toList

-- (nOcurrencias f x) es el número de conjuntos de la familia f a los
-- que pertenece el elemento x. nOcurrencias :: Ord a => Set (Set a) -> a -> Int
nOcurrencias f x =
  length [c | c <- S.toList f, x `S.member` c]

-- ---------------------------------------------------------------------
-- Ejercicio 3 [2.5 puntos]. Los polinomios se pueden representar
-- mediante diccionarios con los exponentes como claves y los
-- coeficientes como valores. 
-- 
-- El tipo de los polinomios con coeficientes de tipo a se define por 
--    type Polinomio a = M.Map Int a
-- 
-- Dos ejemplos de polinomios (que usaremos en los ejemplos) son
--    3 + 7x - 5x^3
--    4 + 5x^3 + x^5
-- se definen por
--   ejPol1, ejPol2 :: Polinomio Int
--   ejPol1 = M.fromList [(0,3),(1,7),(3,-5)]
--   ejPol2 = M.fromList [(0,4),(3,5),(5,1)]
-- 
-- Definir la función
--    multPol :: (Eq a, Num a) => Polinomio a -> Polinomio a -> Polinomio a
-- tal que (multPol p q) es el producto de los polinomios p y q. Por ejemplo, 
--    ghci> multPol ejPol1 ejPol2
--    fromList [(0,12),(1,28),(3,-5),(4,35),(5,3),(6,-18),(8,-5)]
--    ghci> multPol ejPol1 ejPol1
--    fromList [(0,9),(1,42),(2,49),(3,-30),(4,-70),(6,25)]
--    ghci> multPol ejPol2 ejPol2
--    fromList [(0,16),(3,40),(5,8),(6,25),(8,10),(10,1)]
-- ---------------------------------------------------------------------

type Polinomio a = M.Map Int a 

ejPol1, ejPol2 :: Polinomio Int
ejPol1 = M.fromList [(0,3),(1,7),(3,-5)]
ejPol2 = M.fromList [(0,4),(3,5),(5,1)]

multPol :: (Eq a, Num a) => Polinomio a -> Polinomio a -> Polinomio a
multPol p q
  | M.null p  = M.empty
  | otherwise = sumaPol (multPorTerm t q) (multPol r q)
  where (t,r) = M.deleteFindMin p

-- (multPorTerm (n,a) p) es el producto del término ax^n por p. Por
-- ejemplo, 
--    ghci> multPorTerm (2,3) (M.fromList [(0,4),(2,1)])
--    fromList [(2,12),(4,3)]
multPorTerm :: Num a => (Int,a) -> Polinomio a -> Polinomio a
multPorTerm (n,a) p =
  M.map (*a) (M.mapKeys (+n) p)

-- (sumaPol p q) es la suma de los polinomios p y q. Por ejemplo,
--    ghci> sumaPol ejPol1 ejPol2
--    fromList [(0,7),(1,7),(5,1)]
--    ghci> sumaPol ejPol1 ejPol1
--    fromList [(0,6),(1,14),(3,-10)]
sumaPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
sumaPol p q = 
  M.filter (/=0) (M.unionWith (+) p q)

-- ---------------------------------------------------------------------
-- Ejercicio 4 [2.5 puntos] Definir las funciones
--    grafoD  :: [(Int,Int)] -> Grafo Int Int
--    grafoND :: Grafo Int Int -> Grafo Int Int
-- tales que
-- + (grafoD ps) es el grafo dirigido cuyas cuyos nodos son todos los
--   elementos comprendidos entre el menor y el mayor de las componentes
--   de los elementos de ps y las aristas son los elementos de ps
--   añadiéndole el peso cero. Por ejemplo,
--      λ> grafoD [(1,3),(1,4),(4,1)]
--      G D (array (1,4) [(1,[(3,0),(4,0)]),(2,[]),(3,[]),(4,[(1,0)])])
--      λ> grafoD [(1,1),(1,2),(2,2)]
--      G D (array (1,2) [(1,[(1,0),(2,0)]),(2,[(2,0)])])
-- + (grafoND g) es el grafo no dirigido correspondiente al grafo
--   dirigido g (en el que todos los pesos son 0); es decir, es un grafo
--   que tiene el mismo conjunto de nodos que g pero sus aristas son las
--   de g junto con sus inversas. Por ejemplo,
--      λ> grafoND (grafoD [(1,3),(1,4),(4,1)])
--      G ND (array (1,4) [(1,[(3,0),(4,0)]),(2,[]),(3,[(1,0)]),(4,[(1,0)])])
--      λ> grafoND (grafoD [(1,1),(1,2),(2,2)])
--      G ND (array (1,2) [(1,[(1,0),(2,0)]),(2,[(2,0),(1,0)])])
-- ---------------------------------------------------------------------

grafoD :: [(Int,Int)] -> Grafo Int Int
grafoD ps = creaGrafo D (1,n) [(x,y,0) | (x,y) <- ps]
  where n = maximum [max x y | (x,y) <- ps]

grafoND :: Grafo Int Int -> Grafo Int Int
grafoND g = creaGrafo ND (1,n) (nub [(x,y,0) | (x,y,_) <- as ++ bs])
  where n  = maximum (nodos g)
        as = aristas g
        bs = [(y,x,p) | (x,y,p) <- as]
  
