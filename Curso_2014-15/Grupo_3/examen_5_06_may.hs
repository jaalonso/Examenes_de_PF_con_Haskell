-- Informática (1º del Grado en Matemáticas)
-- 5º examen de evaluación continua (6 de mayo de 2015)
-- ---------------------------------------------------------------------

import Data.List
import I1M.Pol
import Data.Matrix 
import I1M.Grafo
import I1M.BusquedaEnEspaciosDeEstados
import qualified Debug.Trace as T

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    conUno :: [Int] -> [Int]
-- tal que (conUno xs) es la lista de los elementos de xs que empiezan
-- por 1. Por ejemplo,
--    conUno [123,51,11,711,52]  == [123,11]
-- ---------------------------------------------------------------------

conUno :: [Int] -> [Int]
conUno xs = [x | x <- xs, head (show x) == '1']

-- ---------------------------------------------------------------------
-- Ejercicio 2. Representamos los árboles binarios con elementos en las
-- hojas y en los nodos mediante el tipo de dato 
--    data Arbol a = H a | N a (Arbol a) (Arbol a) deriving Show
--
-- Definir la función
--    aplica :: (a -> a) -> (a -> a) -> Arbol a -> Arbol a
-- tal que (aplica f g a) devuelve el árbol obtenido al aplicar la
-- función f a las hojas del árbol a y la función g a los nodos
-- interiores. Por ejemplo,
--    ghci> aplica (+1)(*2) (N 5 (N 2 (H 1) (H 2)) (N 3 (H 4) (H 2)))
--    N 10 (N 4 (H 2) (H 3)) (N 6 (H 5) (H 3))
-- ---------------------------------------------------------------------

data Arbol a = H a | N a (Arbol a) (Arbol a) deriving Show

aplica :: (a -> a) -> (a -> a) -> Arbol a -> Arbol a
aplica f g (H x)     = H (f x)
aplica f g (N x i d) = N (g x) (aplica f g i) (aplica f g d)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Representamos los polinomios mediante el TAD de los
-- Polinomios (I1M.Pol). La parte par de un polinomio de coeficientes
-- enteros es el polinomio formado por sus monomios cuyos coeficientes
-- son números pares. Por ejemplo, la parte par de 4x^3+x^2-7x+6 es 
-- 4x^3+6. 
--
-- Definir la función 
--    partePar :: Integral a => Polinomio a -> Polinomio a
-- tal que (partePar p) es la parte par de p. Por ejemplo, 
--    ghci> partePar (consPol 3 4 (consPol 2 1 (consPol 0 6 polCero)))
--    4*x^3 + 6
-- ---------------------------------------------------------------------

partePar :: Integral a => Polinomio a -> Polinomio a
partePar p
    | esPolCero p = polCero
    | even b      = consPol n b (partePar r)
    | otherwise   = partePar r
    where n = grado p
          b = coefLider p
          r = restoPol p

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Representaremos las matrices mediante la librería de
-- Haskell Data.Matrix.  
-- 
-- Las posiciones frontera de una matriz de orden mxn son aquellas que
-- están en la fila 1 o la fila m o la columna 1 o la columna n. El
-- resto se dirán posiciones interiores. Observa que cada elemento en
-- una posición interior tiene exactamente 8 vecinos en la matriz.
-- 
-- Definir la función
--    marco :: Int -> Int -> Integer -> Matrix Integer
-- tal que (marco m n z) genera la matriz de dimensión mxn que
-- contiene el entero z en las posiciones frontera y 0 en las posiciones 
-- interiores. Por ejemplo,
--    ghci> marco 5 5 1
--    ( 1 1 1 1 1 )
--    ( 1 0 0 0 1 )
--    ( 1 0 0 0 1 )
--    ( 1 0 0 0 1 )
--    ( 1 1 1 1 1 )
-- ---------------------------------------------------------------------

marco :: Int -> Int -> Integer -> Matrix Integer
marco m n z  = matrix m n f
    where f (i,j) | frontera m n (i,j) = 1
                  | otherwise          = 0 

-- (frontera m n (i,j)) se verifica si (i,j) es una posición de la
-- frontera de las matrices de dimensión mxn.
frontera :: Int -> Int -> (Int,Int) -> Bool
frontera m n (i,j) = i == 1 || i == m || j == 1 || j == n

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Dada una matriz, un paso de transición genera una
-- nueva matriz de la misma dimensión pero en la que se ha sustituido
-- cada elemento interior por la suma de sus 8 vecinos. Los elementos
-- frontera no varían.   
--
-- Definir la función
--   paso :: Matrix Integer -> Matrix Integer
-- tal que (paso t) calcula la matriz generada tras aplicar un paso de
-- transición a la matriz t. Por ejemplo,
--    ghci> paso (marco 5 5 1)
--    ( 1 1 1 1 1 )
--    ( 1 5 3 5 1 )
--    ( 1 3 0 3 1 )
--    ( 1 5 3 5 1 )
--    ( 1 1 1 1 1 )
-- ---------------------------------------------------------------------

paso :: Matrix Integer -> Matrix Integer
paso p = matrix m n f where
    m = nrows p
    n = ncols p
    f (i,j) 
        | frontera m n (i,j) = 1
        | otherwise          = sum [p!(u,v) | (u,v) <- vecinos m n (i,j)]
    
-- (vecinos m n (i,j)) es la lista de las posiciones de los vecinos de
-- (i,j) en las matrices de dimensión mxn.
vecinos :: Int -> Int -> (Int,Int) -> [(Int,Int)]
vecinos m n (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)]
                           , b <- [max 1 (j-1)..min n (j+1)]
                           , (a,b) /= (i,j)]

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Definir la función 
--    itPasos :: Int -> Matrix Integer -> Matrix Integer
-- tal que (itPasos k t) es la matriz obtenida tras aplicar k pasos de
-- transición a partir de la matriz t. Por ejemplo, 
--    ghci> itPasos 10 (marco 5 5 1)
--    (       1       1       1       1       1 )
--    (       1 4156075 5878783 4156075       1 )
--    (       1 5878783 8315560 5878783       1 )
--    (       1 4156075 5878783 4156075       1 )
--    (       1       1       1       1       1 )
-- ---------------------------------------------------------------------

itPasos :: Int -> Matrix Integer -> Matrix Integer 
itPasos k t = (iterate paso t) !! k 

-- ---------------------------------------------------------------------
-- Ejercicio 4.4. Definir la función 
--    pasosHasta :: Integer -> Int
-- tal que (pasosHasta k) es el número de pasos de transición a partir
-- de la matriz (marco 5 5 1) necesarios para que en la matriz
-- resultante aparezca un elemento mayor que k. Por ejemplo,
--    pasosHasta 4         ==  1
--    pasosHasta 6         ==  2
--    pasosHasta (2^2015)  ==  887
-- ---------------------------------------------------------------------

pasosHasta :: Integer -> Int
pasosHasta k =
    length (takeWhile (\t -> menores t k) (iterate paso (marco 5 5 1)))

-- (menores p k) se verifica si los elementos de p son menores o
-- iguales que k. Por ejemplo, 
--    menores (itPasos 1 (marco 5 5 1)) 6  ==  True
--    menores (itPasos 1 (marco 5 5 1)) 4  ==  False
menores :: Matrix Integer -> Integer -> Bool
menores p k = and [p!(i,j) <= k | i <- [1..m], j <- [1..n]]
    where m = nrows p
          n = ncols p

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Representaremos los grafos mediante el TAD de los
-- grafos (I1M.Grafo). 
-- 
-- Dado un grafo G, un ciclo en G es una secuencia de nodos de G
-- [v(1),v(2),v(3),...,v(n)] tal que: 
--    1) (v(1),v(2)), (v(2),v(3)), (v(3),v(4)), ..., (v(n-1),v(n)) son
--       aristas de G, 
--    2) v(1) = v(n), y
--    3) salvo v(1) = v(n), todos los v(i) son distintos entre sí.
-- 
-- Definir la función
--    esCiclo :: [Int] -> Grafo Int Int -> Bool
-- tal que (esCiclo xs g) se verifica si xs es un ciclo de g. Por
-- ejemplo, si g1 es el grafo definido por
--    g1 :: Grafo Int Int
--    g1 = creaGrafo D (1,4) [(1,2,0),(2,3,0),(2,4,0),(4,1,0)]
-- entonces
--    esCiclo [1,2,4,1] g1  ==  True
--    esCiclo [1,2,3,1] g1  ==  False
--    esCiclo [1,2,3] g1    ==  False
--    esCiclo [1,2,1] g1    ==  False
-- ---------------------------------------------------------------------

g1 :: Grafo Int Int
g1 = creaGrafo D (1,4) [(1,2,0),(2,3,0),(2,4,0),(4,1,0)]

esCiclo :: [Int] -> Grafo Int Int -> Bool
esCiclo vs g = 
    all (aristaEn g) (zip vs (tail vs)) &&
    head vs == last vs &&
    length (nub vs) == length vs - 1    

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. El grafo rueda de orden k es un grafo no dirigido
-- formado por 
--    1) Un ciclo con k nodos [1,2,..,k], y
--    2) un nodo central k+1 unido con cada uno de los k nodos del
--       ciclo; 
--    3) y con peso 0 en todas sus aristas.
-- 
-- Definir la función
--    rueda :: Int -> Grafo Int Int
-- tal que (rueda k) es el grafo rueda de orden k. Por ejemplo,
--    ghci> rueda 3
--    G D (array (1,4) [(1,[(2,0)]),(2,[(3,0)]),(3,[(1,0)]),
--                      (4,[(1,0),(2,0),(3,0)])])
-- ---------------------------------------------------------------------

rueda :: Int -> Grafo Int Int
rueda k = creaGrafo D (1,k+1) ([(i,i+1,0) | i <- [1..k-1]] ++
                               [(k,1,0)] ++
                               [(k+1,i,0) | i <- [1..k]])

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Definir la función
--    contieneCiclo :: Grafo Int Int -> Int -> Bool
-- tal que (contieneCiclo g k) se verifica si el grafo g contiene algún
-- ciclo de orden k. Por ejemplo,
--    contieneCiclo g1 4         ==  True
--    contieneCiclo g1 3         ==  False
--    contieneCiclo (rueda 5) 6  ==  True
-- ---------------------------------------------------------------------

contieneCiclo :: Grafo Int Int -> Int -> Bool
contieneCiclo g k = not (null (ciclos g k)) 
 
-- (caminosDesde g k v) es la lista de los caminos en el grafo g, de
-- longitud k, a partir del vértice v. Por ejemplo
--    caminosDesde g1 3 1  ==  [[1,2,3],[1,2,4]]
caminosDesde1 :: Grafo Int Int -> Int -> Int -> [[Int]]
caminosDesde1 g k v = map reverse (aux [[v]])
    where aux [] = [] 
          aux ((x:vs):vss) 
              | length (x:vs) == k = (x:vs) : aux vss
              | length (x:vs) >  k = aux vss
              | otherwise      = aux ([y:x:vs | y <- adyacentes g x] ++ vss)

caminosDesde2 :: Grafo Int Int -> Int -> Int -> [[Int]]
caminosDesde2 g k v = map (reverse . snd) (aux [(1,[v])])
    where aux [] = [] 
          aux ((n,(x:vs)):vss) 
              | n == k = (n,(x:vs)) : aux vss
              | n >  k = aux vss
              | otherwise = aux ([(n+1,y:x:vs) | y <- adyacentes g x] ++ vss)

-- 3ª definición de caminosDesde (con búsqueda en espacio de estados):
caminosDesde3 :: Grafo Int Int -> Int -> Int -> [[Int]]
caminosDesde3 g k v = map reverse (buscaEE sucesores esFinal inicial)
    where inicial          = [v]
          esFinal vs       = length vs == k
          sucesores (x:xs) = [y:x:xs | y <- adyacentes g x]

-- 4ª definición de caminosDesde (con búsqueda en espacio de estados):
caminosDesde4 :: Grafo Int Int -> Int -> Int -> [[Int]]
caminosDesde4 g k v = map (reverse . snd) (buscaEE sucesores esFinal inicial)
    where inicial            = (1,[v])
          esFinal (n,_)      = n == k
          sucesores (n,x:xs) = [(n+1,y:x:xs) | y <- adyacentes g x]

-- Comparación
--    ghci> let n = 10000 in length (caminosDesde1 (rueda n) (n+1) 1)
--    1
--    (3.87 secs, 20713864 bytes)
--    ghci> let n = 10000 in length (caminosDesde2 (rueda n) (n+1) 1)
--    1
--    (0.10 secs, 18660696 bytes)
--    ghci> let n = 10000 in length (caminosDesde3 (rueda n) (n+1) 1)
--    1
--    (0.42 secs, 16611272 bytes)
--    ghci> let n = 10000 in length (caminosDesde4 (rueda n) (n+1) 1)
--    1
--    (0.10 secs, 20118376 bytes)

-- En lo sucesivo usamos la 4ª definición
caminosDesde :: Grafo Int Int -> Int -> Int -> [[Int]]
caminosDesde = caminosDesde4

-- (ciclosDesde g k v) es la lista de los ciclos en el grafo g de orden
-- k a partir del vértice v. Por ejemplo,
--    ciclosDesde g1 4 1  ==  [[1,2,4,1]]
ciclosDesde :: Grafo Int Int -> Int -> Int -> [[Int]]
ciclosDesde g k v = [xs | xs <- caminosDesde g k v
                        , esCiclo xs g]

-- (ciclos g k) es la lista de los ciclos en el grafo g de orden
-- k. Por ejemplo,
--    ciclos g1 4  ==  [[1,2,4,1],[2,4,1,2],[4,1,2,4]]
ciclos :: Grafo Int Int -> Int -> [[Int]]
ciclos g k = concat [ciclosDesde g k v | v <- nodos g]

caminosDesde5 :: Grafo Int Int -> Int -> [[Int]]
caminosDesde5 g v = map (reverse . fst) (buscaEE sucesores esFinal inicial)
    where inicial             = ([v],[v])
          esFinal (x:_,ys)    = all (`elem` ys) (adyacentes g x)
          sucesores (x:xs,ys) = [(z:x:xs,z:ys) | z <- adyacentes g x
                                               , z `notElem` ys]

--    caminos g1  ==  [[1,2,3],[1,2,4],[2,3],[2,4,1],[3],[4,1,2,3]]
caminos :: Grafo Int Int -> [[Int]]
caminos g = concatMap (caminosDesde5 g) (nodos g)

--    todosCiclos g1  ==  [[1,2,4,1],[2,4,1,2]]

todosCiclos :: Grafo Int Int -> [[Int]]
todosCiclos g = [ys | (x:xs) <- caminos g
                    , let ys = (x:xs) ++ [x]
                    , esCiclo ys g]
