-- Informática (1º del Grado en Matemáticas, Grupo 2)
-- 4º examen de evaluación continua (15 de marzo de 2019)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Librerías                                                          --
-- ---------------------------------------------------------------------

import Data.List
import System.Timeout
import Data.Array
import qualified Data.Matrix as M
import qualified Data.Set as S

-- ---------------------------------------------------------------------
-- Ejercicio 1. Dada una relación de orden r, se dice que el elemento
-- e2 es mayor que el elemento e1 con respecto a r si se cumple 
-- (r e1 e2). 
--
-- Definir la función
--   subconjuntoMaximal :: S.Set a -> (a -> a -> Bool) -> S.Set a
-- tal que (subconjuntoMaximal s r) es el subconjunto del conjunto s
-- formado por todos aquellos elementos que no tienen en el conjunto s
-- ningún elemento mayor con respecto a la relación de orden r. Por
-- ejemplo,
--    λ> subconjuntoMaximal (S.fromList [1,2,3,4]) (<)
--    fromList [4]
--    λ> subconjuntoMaximal (S.fromList [1..4]) (\x y -> even (x+y) && x<y)
--    fromList [3,4]
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

subconjuntoMaximal :: S.Set a -> (a -> a -> Bool) -> S.Set a
subconjuntoMaximal s r =
  S.filter (\e1 -> S.null (S.filter (\e2 -> r e1 e2) s)) s

-- 2ª solución
-- ===========

subconjuntoMaximal2 :: S.Set a -> (a -> a -> Bool) -> S.Set a
subconjuntoMaximal2 s r =
  S.filter (\e1 -> S.null (S.filter (r e1) s)) s

-- 3ª solución
-- ===========

subconjuntoMaximal3 :: S.Set a -> (a -> a -> Bool) -> S.Set a
subconjuntoMaximal3 s r = S.filter (esMaximal s r) s

-- (esMaximal s r x) se verifica si x es maximal en s repecto de r.
esMaximal :: S.Set a -> (a -> a -> Bool) -> a -> Bool
esMaximal s r x = not (any (x `r`) s)
  
-- ---------------------------------------------------------------------
-- Ejercicio 2. La búsqueda dicotómica es una forma eficiente de buscar
-- un elemento en una tabla cuyos elementos están ordenados. Este
-- proceso busca un elemento en un trozo de la tabla delimitado entre
-- dos índices Min y Max y actúa de la siguiente forma: 
-- + Si Min y Max son iguales, entonces se comprueba si el elemento
--   buscado está en la posición Min de la tabla y se termina
-- + En caso contrario, se calcula el índice medio entre Min y Max:
--   Med = (Min+Max)/2 
-- + Si el elemento buscado está en la posición Med de la tabla,
--   entonces lo hemos encontrado 
-- + Si el elemento buscado es menor que el que se encuentra en la
--   posición Med de la tabla, entonces continuamos buscando en el trozo 
--   de la tabla delimitado por Min y la posición anterior a Med
-- + Si el elemento buscado es mayor que el que se encuentra en la
--   posición Med de la tabla, entonces continuamos buscando en el trozo
--   de la tabla delimitado por la posición siguiente a Med y Max
-- Si se llega a una situación en la que Max < Min, entonces el elemento
-- buscado no está en la tabla.
--
-- Para comenzar la búsqueda, el valor inicial de Min es el índice más
-- pequeño de la tabla y el valor inicial de Max es el índice más grande
-- de la tabla. 
--
-- Por ejemplo, para buscar el elemento 6 en una tabla de tamaño 10 que
-- contiene los números pares ordenados del 2 al 20 se procedería como
-- sigue: 
-- + Comenzamos la búsqueda con Min = 1 y Max = 10
-- + Se calcula el índice medio entre Min y Max: Med = 5
-- + El elemento de la tabla que está en la posición 5 es el 10 > 6
-- + Se continúa la búsqueda con Min = 1 y Max = 4
-- + Se calcula el índice medio entre Min y Max: Med = 2
-- + El elemento de la tabla que está en la posición 2 es el 4 < 6
-- + Se continúa la búsqueda con Min = 3 y Max = 4
-- + Se calcula el índice medio entre Min y Max: Med = 3
-- + El elemento de la tabla que está en la posición 3 es el 6
-- + La búsqueda termina con éxito.
--
-- Definir la función
--   busquedaDicotomica :: Ord a => Array Int a -> a -> Bool
-- tal que (busquedaDicotomica m v) realiza una búsqueda dicotómica del
-- elemento v en la tabla m cuyos elementos están ordenados en orden
-- creciente. Por ejemplo,
--   busquedadicotomica (listarray (1,10) [2,4..]) 8         ==  true
--   busquedaDicotomica (listArray (1,10) [2,4..]) 9         ==  False
--   busquedaDicotomica (listArray (1,10) [2,4..]) 0         ==  False
--   busquedaDicotomica (listArray (1,10) [2,4..]) 22        ==  False
--   busquedaDicotomica (listArray (1,10^6) [2,4..]) 123456  ==  True
--   busquedaDicotomica (listArray (1,10^6) [2,4..]) 234567  ==  False
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

busquedaDicotomica :: Ord a => Array Int a -> a -> Bool
busquedaDicotomica v x =
  busquedaDicotomicaAux v x min max
  where (min,max) = bounds v

busquedaDicotomicaAux :: Ord a => Array Int a -> a -> Int -> Int -> Bool
busquedaDicotomicaAux v x min max
  | max == min   = v ! min == x
  | max < min    = False
  | v ! med == x = True
  | x < v ! med  = busquedaDicotomicaAux v x min (med-1)
  | otherwise    = busquedaDicotomicaAux v x (med+1) max
  where med = (min+max) `div` 2

-- 2ª solución
-- ===========

busquedaDicotomica2 :: Ord a => Array Int a -> a -> Bool
busquedaDicotomica2 v x = max == min && v!min == x
  where (min,max) = until imposible reduce (bounds v)
        imposible (a,b) = b <= a
        reduce (a,b) | v!c == x  = (c,c)
                     | x < v!c   = (a,c-1)
                     | otherwise = (c+1, b)
          where c = (a+b) `div` 2

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--   subconjuntosDivisibles :: [Int] -> [[Int]]
-- tal que (subconjuntosDivisibles xs) es la lista de todos los
-- subconjuntos de xs en los que todos los elementos tienen un factor
-- común mayor que 1. Por ejemplo,
--   subconjuntosDivisibles []         ==  [[]]
--   subconjuntosDivisibles [1]        ==  [[]]
--   subconjuntosDivisibles [3]        ==  [[3],[]]
--   subconjuntosDivisibles [1,3]      ==  [[3],[]]
--   subconjuntosDivisibles [3,6]      ==  [[3,6],[3],[6],[]]
--   subconjuntosDivisibles [1,3,6]    ==  [[3,6],[3],[6],[]]
--   subconjuntosDivisibles [2,3,6]    ==  [[2,6],[2],[3,6],[3],[6],[]]
--   subconjuntosDivisibles [2,3,6,8]  ==
--     [[2,6,8],[2,6],[2,8],[2],[3,6],[3],[6,8],[6],[8],[]]
--   length (subconjuntosDivisibles [1..10])  ==  41
--   length (subconjuntosDivisibles [1..20])  ==  1097
--   length (subconjuntosDivisibles [1..30])  ==  33833
--   length (subconjuntosDivisibles [1..40])  ==  1056986
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

subconjuntosDivisibles :: [Int] -> [[Int]]
subconjuntosDivisibles xs = filter esDivisible (subsequences xs)

-- (esDivisible xs) se verifica si todos los elementos de xs tienen un
-- factor común mayor que 1. Por ejemplo,
--    esDivisible [6,10,22]  ==  True
--    esDivisible [6,10,23]  ==  False
esDivisible :: [Int] -> Bool
esDivisible [] = True
esDivisible xs = mcd xs > 1

-- (mcd xs) es el máximo común divisor de xs. Por ejemplo,
--    mcd [6,10,22]  ==  2
--    mcd [6,10,23]  ==  1
mcd :: [Int] -> Int
mcd = foldl1' gcd

-- 2ª solución
-- ===========

subconjuntosDivisibles2 :: [Int] -> [[Int]]
subconjuntosDivisibles2 []     = [[]]
subconjuntosDivisibles2 (x:xs) = [x:ys | ys <- yss, esDivisible (x:ys)] ++ yss
  where yss = subconjuntosDivisibles2 xs

-- 3ª solución
-- ===========

subconjuntosDivisibles3 :: [Int] -> [[Int]]
subconjuntosDivisibles3 []     = [[]]
subconjuntosDivisibles3 (x:xs) = filter esDivisible (map (x:) yss) ++ yss
  where yss = subconjuntosDivisibles3 xs

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> length (subconjuntosDivisibles [1..21])
--    1164
--    (3.83 secs, 5,750,416,768 bytes)
--    λ> length (subconjuntosDivisibles2 [1..21])
--    1164
--    (0.01 secs, 5,400,232 bytes)
--    λ> length (subconjuntosDivisibles3 [1..21])
--    1164
--    (0.01 secs, 5,264,928 bytes)
--    
--    λ> length (subconjuntosDivisibles2 [1..40])
--    1056986
--    (6.95 secs, 8,845,664,672 bytes)
--    λ> length (subconjuntosDivisibles3 [1..40])
--    1056986
--    (6.74 secs, 8,727,141,792 bytes)

-- ---------------------------------------------------------------------
-- Ejrcicio 4. Definir la función
--    matrizGirada180 :: M.Matrix a -> M.Matrix a
-- tal que (matrizGirada180 p) es la matriz obtenida girando 180 grados la
-- matriz p. Por ejemplo,
--    λ> M.fromList 4 3 [1..]
--    (  1  2  3 )
--    (  4  5  6 )
--    (  7  8  9 )
--    ( 10 11 12 )
-- 
--    λ> matrizGirada180 (M.fromList 4 3 [1..])
--    ( 12 11 10 )
--    (  9  8  7 )
--    (  6  5  4 )
--    (  3  2  1 )
-- 
--    λ> M.fromList 3 4 [1..]
--    (  1  2  3  4 )
--    (  5  6  7  8 )
--    (  9 10 11 12 )
-- 
--    λ> matrizGirada180 (M.fromList 3 4 [1..])
--    ( 12 11 10  9 )
--    (  8  7  6  5 )
--    (  4  3  2  1 )
-- ----------------------------------------------------------------------------

-- 1ª solución
matrizGirada180 :: M.Matrix a -> M.Matrix a
matrizGirada180 p = M.matrix m n f
  where m       = M.nrows p
        n       = M.ncols p
        f (i,j) = p M.! (m-i+1,n-j+1)

-- 2ª solución
matrizGirada180b :: M.Matrix a -> M.Matrix a
matrizGirada180b p =
  M.fromLists (reverse (map reverse (M.toLists p)))

-- 3ª solución
matrizGirada180c :: M.Matrix a -> M.Matrix a
matrizGirada180c =
  M.fromLists . reverse . map reverse . M.toLists
