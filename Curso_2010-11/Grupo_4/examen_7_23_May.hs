-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 7º examen de evaluación continua (23 de mayo de 2011)
-- ---------------------------------------------------------------------
 
import Data.Array
import GrafoConVectorDeAdyacencia
import RecorridoEnAnchura

-- ---------------------------------------------------------------------
-- Ejercicio 1. Se considera que los puntos del plano se representan por
-- pares de números como se indica a continuación 
--    type Punto = (Double,Double) 
-- 
-- Definir la función 
--    cercanos :: [Punto] -> [Punto] -> (Punto,Punto)
-- tal que (cercanos ps qs) es un par de puntos, el primero de ps y el
-- segundo de qs, que son los más cercanos (es decir, no hay otro par
-- (p',q') con p' en ps y q' en qs tales que la distancia entre p' y q'
-- sea menor que la que hay entre p y q). Por ejemplo,
--    ghci> cercanos [(2,5),(3,6)] [(4,3),(1,0),(7,9)]
--    ((2.0,5.0),(4.0,3.0))
-- ---------------------------------------------------------------------

type Punto = (Double,Double) 

cercanos :: [Punto] -> [Punto] -> (Punto,Punto)
cercanos ps qs = (p,q)
    where (d,p,q) = minimum [(distancia p q, p, q) | p <- ps, q <-qs]
          distancia (x,y) (u,v) = sqrt ((x-u)^2+(y-v)^2)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Las relaciones binarias pueden representarse mediante
-- conjuntos de pares de elementos. 
-- 
-- Definir la función 
--    simetrica :: Eq a => [(a,a)] -> Bool
-- tal que (simetrica r) se verifica si la relación binaria r es
-- simétrica. Por ejemplo,
--    simetrica [(1,3),(2,5),(3,1),(5,2)]  ==  True
--    simetrica [(1,3),(2,5),(3,1),(5,3)]  ==  False
-- ---------------------------------------------------------------------

simetrica :: Eq a => [(a,a)] -> Bool
simetrica [] = True
simetrica ((x,y):r) 
    | x == y    = True
    | otherwise = elem (y,x) r && simetrica (borra (y,x) r)

-- (borra x ys) es la lista obtenida borrando el elemento x en ys. Por
-- ejemplo, 
--    borra 2 [3,2,5,7,2,3]  ==  [3,5,7,3]
borra :: Eq a => a -> [a] -> [a]
borra x ys = [y | y <- ys, y /= x] 

-- ---------------------------------------------------------------------
-- Ejercicio 3. Un grafo no dirigido G se dice conexo, si para cualquier
-- par de vértices u y v en G, existe al menos una trayectoria (una
-- sucesión de vértices adyacentes) de u a v.
--
-- Definirla función 
--    conexo :: (Ix a, Num p) => Grafo a p -> Bool
-- tal que (conexo g) se verifica si el grafo g es conexo. Por ejemplo, 
--    conexo (creaGrafo False (1,3) [(1,2,0),(3,2,0)])  ==  True
--    conexo (creaGrafo False (1,4) [(1,2,0),(3,4,0)])  ==  False
-- ---------------------------------------------------------------------

conexo :: (Ix a, Num p) => Grafo a p -> Bool
conexo g = length (recorridoEnAnchura i g) == n
    where xs = nodos g
          i  = head xs
          n  = length xs

-- ---------------------------------------------------------------------
-- Ejercicio 4. Se consideran los tipos de los vectores y de las
-- matrices definidos por 
--    type Vector a = Array Int a
--    type Matriz a = Array (Int,Int) a
-- y, como ejemplo, la matriz q definida por
--    q :: Matriz Int
--    q = array ((1,1),(2,2)) [((1,1),1),((1,2),1),((2,1),1),((2,2),0)] 
-- 
-- Definir la función
--    potencia :: Num a => Matriz a -> Int -> Matriz a
-- tal que (potencia p n) es la potencia n-ésima de la matriz cuadrada
-- p. Por ejemplo,
--    ghci> potencia q 2
--    array ((1,1),(2,2)) [((1,1),2),((1,2),1),((2,1),1),((2,2),1)]
--    ghci> potencia q 3
--    array ((1,1),(2,2)) [((1,1),3),((1,2),2),((2,1),2),((2,2),1)]
--    ghci> potencia q 4
--    array ((1,1),(2,2)) [((1,1),5),((1,2),3),((2,1),3),((2,2),2)]
-- ¿Qué relación hay entre las potencias de la matriz q y la sucesión de
-- Fibonacci? 
-- ---------------------------------------------------------------------

type Vector a = Array Int a
type Matriz a = Array (Int,Int) a

q :: Matriz Int
q = array ((1,1),(2,2)) [((1,1),1),((1,2),1),((2,1),1),((2,2),0)] 

potencia :: Num a => Matriz a -> Int -> Matriz a
potencia p 0 = identidad (numFilas p)
potencia p (n+1) = prodMatrices p (potencia p n)

-- (identidad n) es la matriz identidad de orden n. Por ejemplo,
--    ghci> identidad 3
--    array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),0),
--                         ((2,1),0),((2,2),1),((2,3),0),
--                         ((3,1),0),((3,2),0),((3,3),1)]
identidad :: Num a => Int -> Matriz a
identidad n =     
    array ((1,1),(n,n))
          [((i,j),f i j) | i <- [1..n], j <- [1..n]]
    where f i j | i == j    = 1
                | otherwise = 0

-- (prodEscalar v1 v2) es el producto escalar de los vectores v1
-- y v2. Por ejemplo,
--    ghci> prodEscalar (listArray (1,3) [3,2,5]) (listArray (1,3) [4,1,2])
--    24
prodEscalar :: Num a => Vector a -> Vector a -> a
prodEscalar v1 v2 = 
    sum [i*j | (i,j) <- zip (elems v1) (elems v2)]

-- (filaMat i p) es el vector correspondiente a la fila i-ésima
-- de la matriz p. Por ejemplo,
--    filaMat 2 q  ==  array (1,2) [(1,1),(2,0)]
filaMat :: Num a => Int -> Matriz a -> Vector a
filaMat i p = array (1,n) [(j,p!(i,j)) | j <- [1..n]]
    where n = numColumnas p

-- (columnaMat j p) es el vector correspondiente a la columna
-- j-ésima de la matriz p. Por ejemplo,
--    columnaMat 2 q  ==  array (1,2) [(1,1),(2,0)]
columnaMat :: Num a => Int -> Matriz a -> Vector a
columnaMat j p = array (1,m) [(i,p!(i,j)) | i <- [1..m]]
    where m = numFilas p

-- (numFilas m) es el número de filas de la matriz m. Por ejemplo,
--    numFilas q  ==  2
numFilas :: Num a => Matriz a -> Int
numFilas = fst . snd . bounds

-- (numColumnas m) es el número de columnas de la matriz m. Por ejemplo, 
--    numColumnas q  ==  2
numColumnas:: Num a => Matriz a -> Int
numColumnas = snd . snd . bounds

-- (prodMatrices p q) es el producto de las matrices p y q. Por ejemplo,
--    ghci> prodMatrices q q
--    array ((1,1),(2,2)) [((1,1),2),((1,2),1),((2,1),1),((2,2),1)]
prodMatrices :: Num a => Matriz a -> Matriz a -> Matriz a
prodMatrices p q = 
    array ((1,1),(m,n))
          [((i,j), prodEscalar (filaMat i p) (columnaMat j q)) |
           i <- [1..m], j <- [1..n]]
    where m = numFilas p
          n = numColumnas q

-- Los sucesión de Fibonacci es 0,1,1,2,3,5,8,13,... Se observa que los
-- elementos de (potencia q n) son los términos de la sucesión en los
-- lugares n+1, n, n y n-1.
