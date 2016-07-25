-- Informática (1º del Grado en Matemáticas)
-- Examen de septiembre (4 de septiembre de 2015)
-- ---------------------------------------------------------------------

import Data.List
import Data.Array
import Test.QuickCheck
import I1M.PolOperaciones

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    numeroBloquesRepeticion :: Eq a => [a] -> Int
-- tal que (numeroBloquesRepeticion xs) es el número de bloques de
-- elementos consecutivos repetidos en 'xs'. Por ejemplo, 
--    numeroBloquesRepeticion [1,1,2,2,3,3]  ==  3
--    numeroBloquesRepeticion [1,1,1,2,3,3]  ==  2
--    numeroBloquesRepeticion [1,1,2,3]      ==  1
--    numeroBloquesRepeticion [1,2,3]        ==  0
-- ---------------------------------------------------------------------

-- 1ª definición
numeroBloquesRepeticion1 :: Eq a => [a] -> Int
numeroBloquesRepeticion1 xs =
    length (filter (\ys -> length ys > 1) (group xs))

-- 2ª definición (por recursión):
numeroBloquesRepeticion2 :: Eq a => [a] -> Int
numeroBloquesRepeticion2 (x:y:zs)
    | x == y    = 1 + numeroBloquesRepeticion2 (dropWhile (==x) zs)
    | otherwise = numeroBloquesRepeticion2 (y:zs)
numeroBloquesRepeticion2 _ = 0

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Los grafos se pueden representar mediante una lista de
-- pares donde las primeras componentes son los vértices y las segundas
-- la lista de los vértices conectados. Por ejemplo, el grafo 
--    1 ----- 2
--    | \     |
--    |  3    |
--    | /     |
--    4 ----- 5
-- se representa por
--    [(1,[2,3,4]),(2,[1,5]),(3,[1,4]),(4,[1,3,5]),(5,[2,4])]   
-- En Haskell se puede definir el tipo de los grafos por
--    type Grafo a = [(a,[a])]
-- y el ejemplo anterior se representa por
--    ejGrafo :: Grafo Int
--    ejGrafo = [(1,[2,3,4]),(2,[1,5]),(3,[1,4]),(4,[1,3]),(5,[2,4])]   
-- 
-- Definir la función
--    aristas :: Ord a => Grafo a -> [(a,a)]
-- tal que (aristas g) es la lista de aristas del grafo g. Por ejemplo, 
--    aristas ejGrafo  ==  [(1,2),(1,3),(1,4),(2,5),(3,4)]
-- ---------------------------------------------------------------------

type Grafo a = [(a,[a])]

ejGrafo :: Grafo Int
ejGrafo = [(1,[2,3,4]),(2,[1,5]),(3,[1,4]),(4,[1,3]),(5,[2,4])]   

aristas :: Ord a => Grafo a -> [(a,a)]
aristas g = [(x,y) | (x,ys) <- g, y <- ys, x < y]  

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. El grafo línea de un grafo G es el grafo L(G) tal que
-- + los vértices de L(G) son las aristas de G y
-- + dos vértices de L(G) son adyacentes si y sólo si sus aristas
--   correspondientes tienen un extremo común en G. 
--
-- Definir la función 
--    grafoLinea :: Ord a => Grafo a -> Grafo (a,a)
-- tal que (grafoLinea g) es el grafo línea de g. Por ejemplo
--    ghci> grafoLinea ejGrafo
--    [((1,2),[(1,3),(1,4),(2,5)]),
--     ((1,3),[(1,2),(1,4),(3,4)]),
--     ((1,4),[(1,2),(1,3),(3,4)]),
--     ((2,5),[(1,2)]),
--     ((3,4),[(1,3),(1,4)])]
-- ---------------------------------------------------------------------

grafoLinea :: Ord a => Grafo a -> Grafo (a,a)
grafoLinea g = 
    [(a1,[a2 | a2 <- as, conExtremoComun a1 a2, a1 /= a2]) | a1 <- as]
    where as = aristas g

conExtremoComun :: Eq a => (a,a) -> (a,a) -> Bool
conExtremoComun (x1,y1) (x2,y2) =
    not (null ([x1,y1] `intersect` [x2,y2]))

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. La sucesión de polinomios de Fibonacci se define por
--    p(0) = 0
--    p(1) = 1
--    p(n) = x*p(n-1) + p(n-2)
-- Los primeros términos de la sucesión son
--    p(2) = x
--    p(3) = x^2 + 1
--    p(4) = x^3 + 2*x
--    p(5) = x^4 + 3*x^2 + 1
--
-- Definir la lista
--    sucPolFib :: [Polinomio Integer]
-- tal que sus elementos son los polinomios de Fibonacci. Por ejemplo,
--    ghci> take 6 sucPolFib
--    [0,1,1*x,x^2 + 1,x^3 + 2*x,x^4 + 3*x^2 + 1]
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

sucPolFib :: [Polinomio Integer]
sucPolFib = [polFibR n | n <- [0..]]

polFibR :: Integer -> Polinomio Integer
polFibR 0 = polCero
polFibR 1 = polUnidad
polFibR n = 
    sumaPol (multPol (consPol 1 1 polCero) (polFibR (n-1)))
            (polFibR (n-2))

-- 2ª definición (dinámica)
-- ========================

sucPolFib2 :: [Polinomio Integer]
sucPolFib2 = 
    polCero : polUnidad : zipWith f (tail sucPolFib2) sucPolFib2
    where f p = sumaPol (multPol (consPol 1 1 polCero) p)

-- --------------------------------------------------------------------- 
-- Ejercicio 3.2. Comprobar con QuickCheck que el valor del n-ésimo
-- término de sucPolFib para x=1 es el n-ésimo término de la sucesión de
-- Fibonacci 0, 1, 1, 2, 3, 5, 8, ...
--
-- Nota. Limitar la búsqueda a ejemplos pequeños usando
--    quickCheckWith (stdArgs {maxSize=5}) prop_polFib
-- ---------------------------------------------------------------------

-- La propiedad es
prop_polFib :: Integer -> Property
prop_polFib n = 
    n >= 0 ==> valor (polFib n) 1 == fib n
    where polFib n = sucPolFib2 `genericIndex` n
          fib n    = fibs `genericIndex` n

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- La comprobación es
--    ghci> quickCheckWith (stdArgs {maxSize=5}) prop_polFib
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4. Los números triangulares se forman como sigue
-- 
--    *     *      * 
--         * *    * *
--               * * *
--    1     3      6
-- 
-- La sucesión de los números triangulares se obtiene sumando los
-- números naturales. Así, los 5 primeros números triangulares son
--     1 = 1
--     3 = 1+2
--     6 = 1+2+3
--    10 = 1+2+3+4
--    15 = 1+2+3+4+5
-- 
-- Definir la función
--    descomposicionesTriangulares :: Int -> [(Int, Int, Int)] 
-- tal que (descomposicionesTriangulares n) es la lista de las
-- ternas correspondientes a las descomposiciones de n en tres sumandos,
-- como máximo, formados por números triangulares. Por ejemplo,
--    ghci> descomposicionesTriangulares 6
--    [(0,0,6),(0,3,3)]
--    ghci> descomposicionesTriangulares 26
--    [(1,10,15),(6,10,10)]
--    ghci> descomposicionesTriangulares 96
--    [(3,15,78),(6,45,45),(15,15,66),(15,36,45)]
-- ---------------------------------------------------------------------

descomposicionesTriangulares :: Int -> [(Int, Int, Int)] 
descomposicionesTriangulares n =         
    [(x,y,n-x-y) | x <- xs, 
                   y <- dropWhile (<x) xs, 
                   n-x-y `elem` dropWhile (<y) xs]
    where xs = takeWhile (<=n) triangulares

-- triangulares es la lista de los números triangulares. Por ejemplo,
--    take 10 triangulares  ==  [0,1,3,6,10,15,21,28,36,45]
triangulares :: [Int]
triangulares = scanl (+) 0 [1..]

-- ---------------------------------------------------------------------
-- Ejercicio 5. En este problema se consideran matrices cuyos elementos
-- son 0 y 1.  Los valores 1 aparecen en forma de islas rectangulares
-- separadas por 0 de forma que como máximo las islas son diagonalmente
-- adyacentes. Por ejemplo, 
--    ej1, ej2 :: Array (Int,Int) Int
--    ej1 = listArray ((1,1),(6,3))
--                    [0,0,0,
--                     1,1,0,
--                     1,1,0,
--                     0,0,1,
--                     0,0,1,
--                     1,1,0]
--    ej2 = listArray ((1,1),(6,6))
--                    [1,0,0,0,0,0,
--                     1,0,1,1,1,1,
--                     0,0,0,0,0,0,
--                     1,1,1,0,1,1,
--                     1,1,1,0,1,1,
--                     0,0,0,0,1,1]
-- 
-- Definir la función
--    numeroDeIslas :: Array (Int,Int) Int -> Int
-- tal que (numeroDeIslas p) es el número de islas de la matriz p. Por
-- ejemplo, 
--    numeroDeIslas ej1  ==  3
--    numeroDeIslas ej2  ==  4
-- ---------------------------------------------------------------------

type Matriz = Array (Int,Int) Int

ej1, ej2 :: Array (Int,Int) Int
ej1 = listArray ((1,1),(6,3))
                [0,0,0,
                 1,1,0,
                 1,1,0,
                 0,0,1,
                 0,0,1,
                 1,1,0]
ej2 = listArray ((1,1),(6,6))
                [1,0,0,0,0,0,
                 1,0,1,1,1,1,
                 0,0,0,0,0,0,
                 1,1,1,0,1,1,
                 1,1,1,0,1,1,
                 0,0,0,0,1,1]

numeroDeIslas :: Array (Int,Int) Int -> Int
numeroDeIslas p = 
    length [(i,j) | (i,j) <- indices p, 
                     verticeSuperiorIzquierdo p (i,j)]

-- (verticeSuperiorIzquierdo p (i,j)) se verifica si (i,j) es el
-- vértice superior izquierdo de algunas de las islas de la matriz p,
-- Por ejemplo, 
--    ghci> [(i,j) | (i,j) <- indices ej1, verticeSuperiorIzquierdo ej1 (i,j)]
--    [(2,1),(4,3),(6,1)]
--    ghci> [(i,j) | (i,j) <- indices ej2, verticeSuperiorIzquierdo ej2 (i,j)]
--    [(1,1),(2,3),(4,1),(4,5)]
verticeSuperiorIzquierdo :: Matriz -> (Int,Int) -> Bool
verticeSuperiorIzquierdo p (i,j) =
    enLadoSuperior p (i,j) && enLadoIzquierdo p (i,j) 

-- (enLadoSuperior p (i,j)) se verifica si (i,j) está en el lado
-- superior de algunas de las islas de la matriz p, Por ejemplo,
--    ghci> [(i,j) | (i,j) <- indices ej1, enLadoSuperior ej1 (i,j)]
--    [(2,1),(2,2),(4,3),(6,1),(6,2)]
--    ghci> [(i,j) | (i,j) <- indices ej2, enLadoSuperior ej2 (i,j)]
--    [(1,1),(2,3),(2,4),(2,5),(2,6),(4,1),(4,2),(4,3),(4,5),(4,6)]
enLadoSuperior :: Matriz -> (Int,Int) -> Bool
enLadoSuperior p (1,j) = p!(1,j) == 1
enLadoSuperior p (i,j) = p!(i,j) == 1 && p!(i-1,j) == 0

-- (enLadoIzquierdo p (i,j)) se verifica si (i,j) está en el lado
-- izquierdo de algunas de las islas de la matriz p, Por ejemplo,
--    ghci> [(i,j) | (i,j) <- indices ej1, enLadoIzquierdo ej1 (i,j)]
--    [(2,1),(3,1),(4,3),(5,3),(6,1)]
--    ghci> [(i,j) | (i,j) <- indices ej2, enLadoIzquierdo ej2 (i,j)]
--    [(1,1),(2,1),(2,3),(4,1),(4,5),(5,1),(5,5),(6,5)]
enLadoIzquierdo :: Matriz -> (Int,Int) -> Bool
enLadoIzquierdo p (i,1) = p!(i,1) == 1
enLadoIzquierdo p (i,j) = p!(i,j) == 1 && p!(i,j-1) == 0

-- 2ª solución
-- ===========

numeroDeIslas2 :: Array (Int,Int) Int -> Int
numeroDeIslas2 p = 
    length [(i,j) | (i,j) <- indices p, 
                    p!(i,j) == 1,
                    i == 1 || p!(i-1,j) == 0,
                    j == 1 || p!(i,j-1) == 0]  

