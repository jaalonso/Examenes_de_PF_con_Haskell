-- Informática (1º del Grado en Matemáticas y en Matemáticas y Estadística)
-- 5º examen de evaluación continua (22 de mayo de 2014) 
-- ============================================================================

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Array
import GrafoConListas
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Una sucesión creciente es aquella en la que todo
-- elemento es estrictamente mayor que el anterior. Una sucesión
-- super-creciente es una sucesión creciente en la que la diferencia
-- entre un elemento y el siguiente es estrictamente mayor que la
-- diferencia entre dicho elemento y el anterior. Por ejemplo, [1,2,4,7]
-- es una sucesión super-creciente, pero [1,4,8,12] no. Otra
-- caracterización de las sucesiones super-crecientes es que la sucesión
-- de las diferencias entre elementos consecutivos es creciente. 
--
-- Definir, utilizando exclusivamente recursión en todas las
-- definiciones (incluyendo auxiliares), la función 
--    superCrecienteR :: (Num a, Ord a) => [a] -> Bool
-- tal que (superCrecienteR xs) se verifica si la secuencia xs es
-- super-creciente. Por ejemplo, 
--    superCrecienteR [1,2,4,7]   ==  True
--    superCrecienteR [1,4,8,12]  ==  False
-- ----------------------------------------------------------------------------

-- 1ª solución de superCrecienteR:
superCrecienteR :: (Num a, Ord a) => [a] -> Bool
superCrecienteR xs = crecienteR (diferenciasR xs)

crecienteR :: Ord a => [a] -> Bool
crecienteR (x1:x2:xs) = x1 < x2 && crecienteR (x2:xs)
crecienteR _          = True

diferenciasR :: Num a => [a] -> [a]
diferenciasR (x1:x2:xs) = x2-x1 : diferenciasR (x2:xs)
diferenciasR _          = []

-- 2ª solución de superCrecienteR:
superCrecienteR2 :: (Num a, Ord a) => [a] -> Bool
superCrecienteR2 [x1,x2]       = x1 < x2
superCrecienteR2 (x1:x2:x3:xs) = x1 < x2 && x2-x1 < x3-x2 && 
                                 superCrecienteR2 (x2:x3:xs)
superCrecienteR2 _             = True

-- ----------------------------------------------------------------------------
-- Ejercicio 2. Definir sin utilizar recursión en ninguna de las definiciones 
-- (incluyendo auxiliares), la función 
--    superCrecienteC :: (Num a, Ord a) => [a] -> Bool
-- tal que  (superCrecienteC xs) se verifica si la secuencia xs es 
-- super-creciente. Por ejemplo, 
--    superCrecienteC [1,2,4,7]   ==  True
--    superCrecienteC [1,4,8,12]  ==  False
-- ----------------------------------------------------------------------------

-- 1ª definición de superCrecienteC:
superCrecienteC :: (Num a, Ord a) => [a] -> Bool
superCrecienteC xs = crecienteC (diferenciasC xs)

crecienteC :: Ord a => [a] -> Bool
crecienteC xs = and [x1 < x2 | (x1,x2) <- zip xs (tail xs)]

diferenciasC :: Num t => [t] -> [t]
diferenciasC xs = zipWith (-) (tail xs) xs 

-- 2ª definición de superCrecienteC:
superCrecienteC2 :: (Num a, Ord a) => [a] -> Bool
superCrecienteC2 xs = 
    and [x1 < x2 && x2-x1 < x3-x2 | 
         (x1,x2,x3) <- zip3 xs (tail xs) (drop 2 xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Se considera la secuencia infinita de todos los números
-- naturales. 
--    [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,...]
-- Si todos los números de esta secuencia se descomponen en sus dígitos,
-- se obtiene la secuencia infinita:
--    [1,2,3,4,5,6,7,8,9,1,0,1,1,1,2,1,3,1,4,1,5,1,6,1,7,1,8,1,9,2,,...]
-- 
-- Definir la función 
--    secuenciaDigitosNaturales :: [Int]
-- tal que su valor es la secuencia infinita de los dígitos de todos los
-- elementos de la secuencia de números naturales. Por ejemplo,
--    take 11 secuenciaDigitosNaturales  ==  [1,2,3,4,5,6,7,8,9,1,0]
-- ---------------------------------------------------------------------

secuenciaDigitosNaturales :: [Int]
secuenciaDigitosNaturales = [read [c] | n <- [1..], c <- show n]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Consideremos las matrices representadas como tablas
-- cuyos índices son pares de números naturales. 
--    type Matriz a = Array (Int,Int) a
-- La dimensión de una matriz es el par formado por el número de filas y
-- el número de columnas:
--    dimension :: Num a => Matriz a -> (Int,Int)
--    dimension = snd . bounds
-- 
-- Una matriz tridiagonal es aquella en la que sólo hay elementos distintos de
-- 0 en la diagonal principal o en las diagonales por encima y por debajo de la
-- diagonal principal. Por ejemplo,
--    ( 1 2 0 0 0 0 )
--    ( 3 4 5 0 0 0 )
--    ( 0 6 7 8 0 0 )
--    ( 0 0 9 1 2 0 )
--    ( 0 0 0 3 4 5 )
--    ( 0 0 0 0 6 7 )
-- 
-- Definir la función 
--    creaTridiagonal :: Int -> Matriz Int
-- tal que (creaTridiagonal n) es la siguiente matriz tridiagonal
-- cuadrada con n filas y n columnas:
--    ( 1 1 0 0 0 0 ... 0  0  )
--    ( 1 2 2 0 0 0 ... 0  0  )
--    ( 0 2 3 3 0 0 ... 0  0  )
--    ( 0 0 3 4 4 0 ... 0  0  )
--    ( 0 0 0 4 5 5 ... 0  0  )
--    ( 0 0 0 0 5 6 ... 0  0  )
--    ( ..................... )
--    ( 0 0 0 0 0 0 ... n  n  )
--    ( 0 0 0 0 0 0 ... n n+1 )
-- Por ejemplo,
--    ghci> creaTridiagonal 4
--    array ((1,1),(4,4)) [((1,1),1),((1,2),1),((1,3),0),((1,4),0),
--                         ((2,1),1),((2,2),2),((2,3),2),((2,4),0),
--                         ((3,1),0),((3,2),2),((3,3),3),((3,4),3),
--                         ((4,1),0),((4,2),0),((4,3),3),((4,4),4)]
-- ----------------------------------------------------------------------------

type Matriz a = Array (Int,Int) a

dimension :: Num a => Matriz a -> (Int,Int)
dimension = snd . bounds

creaTridiagonal :: Int -> Matriz Int
creaTridiagonal n =
    array ((1,1),(n,n))
          [((i,j),valores i j) | i <- [1..n], j <- [1..n]]
    where valores i j | i == j     = i
                      | i == j+1   = j
                      | i+1 == j   = i
                      | otherwise  = 0

-- ----------------------------------------------------------------------------
-- Ejercicio 5. Definir la función 
--    esTridiagonal :: Matriz Int -> Bool
-- tal que (esTridiagonal m) se verifica si la matriz m es tridiagonal. Por 
-- ejemplo,
--    ghci> esTridiagonal (listArray ((1,1),(3,3)) [1..9])
--    False
--    ghci> esTridiagonal (creaTridiagonal 5)
--    True
-- ----------------------------------------------------------------------------

esTridiagonal :: Matriz Int -> Bool
esTridiagonal m =
    and [m!(i,j) == 0 | i <- [1..p], j <- [1..q], (j < i-1 || j > i+1)]
    where (p,q) = dimension m

-- ---------------------------------------------------------------------
-- Ejercicio 6. Consideremos una implementación del TAD de los grafos,
-- por ejemplo en la que los grafos se representan mediante listas. Un
-- ejemplo de grafo es el siguiente:
--    g0 :: Grafo Int Int
--    g0 = creaGrafo D (1,6) [(1,3,2),(1,5,4),(3,5,6),(5,1,8),(5,5,10),
--                            (2,4,1),(2,6,3),(4,6,5),(4,4,7),(6,4,9)]
-- 
-- Definir la función 
--    conectados :: Grafo Int Int -> Int -> Int -> Bool
-- tal que (conectados g v1 v2) se verifica si los vértices v1 y v2
-- están conectados en el grafo g. Por ejemplo,
--    conectados g0 1 3  ==  True
--    conectados g0 1 4  ==  False
--    conectados g0 6 2  ==  False
--    conectados g0 2 6  ==  True
-- ----------------------------------------------------------------------------

g0 :: Grafo Int Int
g0 = creaGrafo D (1,6) [(1,3,2),(1,5,4),(3,5,6),(5,1,8),(5,5,10),
                        (2,4,1),(2,6,3),(4,6,5),(4,4,7),(6,4,9)]

conectados :: Grafo Int Int -> Int -> Int -> Bool
conectados g v1 v2 = elem v2 (conectadosAux g [] [v1])

conectadosAux :: Grafo Int Int -> [Int] -> [Int] -> [Int]
conectadosAux g vs [] = vs
conectadosAux g vs (w:ws) 
    | elem w vs = conectadosAux g vs ws
    | otherwise = conectadosAux g (union [w] vs) (union ws (adyacentes g w))

