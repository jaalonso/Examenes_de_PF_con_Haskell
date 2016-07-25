-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- Examen de la convocatoria de Septiembre (10 de septiembre de 2012)
-- ---------------------------------------------------------------------

import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1. [1.7 puntos] El enunciado de uno de los problemas de la
-- IMO de 1966 es
--    Calcular el número de maneras de obtener 500 como suma de números
--    naturales consecutivos.
-- Definir la función
--    sucesionesConSuma :: Int -> [(Int,Int)]
-- tal que (sucesionesConSuma n) es la lista de las sucesiones de
-- números naturales consecutivos con suma n. Por ejemplo,
--    sucesionesConSuma 15  == [(1,5),(4,6),(7,8),(15,15)]
-- ya que 15 = 1+2+3+4+5 = 4+5+6 = 7+8 = 15.
--
-- Calcular la solución del problema usando sucesionesConSuma. 
-- ---------------------------------------------------------------------

sucesionesConSuma :: Int -> [(Int,Int)]
sucesionesConSuma n =
    [(x,y) | y <- [1..n], x <- [1..y], sum [x..y] == n]

-- La solución del problema es
--    ghci> length (sucesionesConSuma 500)
--    4

-- Otra definción, usando la fórmula de la suma es
sucesionesConSuma2 :: Int -> [(Int,Int)]
sucesionesConSuma2 n = 
    [(x,y) | y <- [1..n], x <- [1..y], (x+y)*(y-x+1) == 2*n]

-- La 2ª definición es más eficiente
--    ghci> :set +s
--    ghci> sucesionesConSuma 500
--    [(8,32),(59,66),(98,102),(500,500)]
--    (1.47 secs, 1452551760 bytes)
--    ghci> sucesionesConSuma2 500
--    [(8,32),(59,66),(98,102),(500,500)]
--    (0.31 secs, 31791148 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2 [1.7 puntos] Definir la función
--    inversiones :: Ord a => [a] -> [(a,Int)]
-- tal que (inversiones xs) es la lista de pares formados por los
-- elementos x de xs junto con el número de elementos de xs que aparecen
-- a la derecha de x y son mayores que x. Por ejemplo,
--    inversiones [7,4,8,9,6]  == [(7,2),(4,3),(8,1),(9,0),(6,0)]
-- ---------------------------------------------------------------------

inversiones :: Ord a => [a] -> [(a,Int)]
inversiones []     = []
inversiones (x:xs) = (x,length (filter (>x) xs)) : inversiones xs

-- ---------------------------------------------------------------------
-- Ejercicio 3 [1.7 puntos] Se considera el siguiente procedimiento de
-- reducción de listas: Se busca un par de elementos consecutivos
-- iguales pero con signos opuestos, se eliminan dichos elementos y se
-- continúa el proceso hasta que no se encuentren pares de elementos
-- consecutivos iguales pero con signos opuestos. Por ejemplo, la
-- reducción de [-2,1,-1,2,3,4,-3] es
--    [-2,1,-1,2,3,4,-3]    (se elimina el par (1,-1))
--    -> [-2,2,3,4,-3]      (se elimina el par (-2,2))
--    -> [3,4,-3]           (el par (3,-3) no son consecutivos)
-- Definir la función
--    reducida :: [Int] -> [Int]
-- tal que (reducida xs) es la lista obtenida aplicando a xs el proceso
-- de eliminación de pares de elementos consecutivos opuestos. Por
-- ejemplo,
--    reducida [-2,1,-1,2,3,4,-3]           == [3,4,-3]
--    reducida [-2,1,-1,2,3,-4,4,-3]        == []
--    reducida [-2,1,-1,2,5,3,-4,4,-3]      == [5]
--    reducida [-2,1,-1,2,5,3,-4,4,-3,-5]   == []
-- ---------------------------------------------------------------------

paso :: [Int] -> [Int]
paso [] = []
paso [x] = [x]
paso (x:y:zs) | x == -y   = paso zs
              | otherwise = x : paso (y:zs)

reducida :: [Int] -> [Int]
reducida xs | xs == ys  = xs
            | otherwise = reducida ys
            where ys = paso xs

reducida2 :: [Int] -> [Int]
reducida2 xs = aux xs []
    where aux [] ys                   = reverse ys
          aux (x:xs) (y:ys) | x == -y = aux xs ys
          aux (x:xs) ys               = aux xs (x:ys)

-- ---------------------------------------------------------------------
-- Ejercicio 4. [1.7 puntos] Las variaciones con repetición de una lista
-- xs se puede ordenar por su longitud y las de la misma longitud
-- lexicográficamente. Por ejemplo, las variaciones con repetición de
-- "ab" son
--    "","a","b","aa","ab","ba","bb","aaa","aab","aba","abb","baa",...
-- y las de "abc" son
--    "","a","b","c","aa","ab","ac","ba","bb","bc","ca","cb",...
-- Definir la función 
--    posicion :: Eq a => [a] -> [a] -> Int
-- tal que (posicion xs ys) es posición de xs en la lista ordenada de
-- las variaciones con repetición de los elementos de ys. Por ejemplo,
--    posicion "ba" "ab"       == 5
--    posicion "ba" "abc"      == 7
--    posicion "abccba" "abc"  == 520
-- ---------------------------------------------------------------------

posicion :: Eq a => [a] -> [a] -> Int
posicion xs ys =
    length (takeWhile (/=xs) (variaciones ys))

variaciones :: [a] -> [[a]]
variaciones xs = concat aux  
    where aux = [[]] : [[x:ys | x <- xs, ys <- yss] | yss <- aux] 

-- ---------------------------------------------------------------------
-- Ejercicio 5. [1.6 puntos] Un árbol ordenado es un árbol binario tal
-- que para cada nodo, los elementos de su subárbol izquierdo son
-- menores y los de su subárbol derecho son mayores. Por ejemplo,
--         5
--        / \
--       /   \
--      3     7
--     / \   / \
--    1   4 6   9 
-- El tipo de los árboles binarios se define por
--    data Arbol = H Int
--               | N Int Arbol Arbol
-- con lo que el ejemplo anterior se define por
--    ejArbol = N 5 (N 3 (H 1) (H 4)) (N 7 (H 6) (H 9))
-- 
-- Definir la función
--    ancestroMasProximo :: Int -> Int -> Int
-- tal que (ancestroMasProximo x y a) es el ancestro más próximo de los
-- nodos x e y en el árbol a. Por ejemplo, 
--    ancestroMasProximo 4 1 ejArbol  == 3
--    ancestroMasProximo 4 6 ejArbol  == 5
-- ---------------------------------------------------------------------

data Arbol = H Int
           | N Int Arbol Arbol

ejArbol :: Arbol
ejArbol = N 5 (N 3 (H 1) (H 4)) (N 7 (H 6) (H 9))

ancestroMasProximo :: Int -> Int -> Arbol -> Int
ancestroMasProximo x y (N z i d)
    | x < z && y < z = ancestroMasProximo x y i
    | x > z && y > z = ancestroMasProximo x y d
    | otherwise      = z

-- ---------------------------------------------------------------------
-- Ejercicio 6. [1.6 puntos] Las matrices puede representarse mediante
-- tablas cuyos índices son pares de números naturales:   
--    type Matriz = Array (Int,Int) Int
-- Definir la función 
--    maximos :: Matriz -> [Int]
-- tal que (maximos p) es la lista de los máximos locales de la matriz
-- p; es decir de los elementos de p que son mayores que todos sus
-- vecinos. Por ejemplo,  
--    ghci> maximos (listArray ((1,1),(3,4)) [9,4,6,5,8,1,7,3,0,2,5,4])
--    [9,7]
-- ya que los máximos locales de la matriz
--    |9 4 6 5|
--    |8 1 7 3|
--    |0 2 5 4|
-- son 9 y 7.
-- ---------------------------------------------------------------------

type Matriz = Array (Int,Int) Int

maximos :: Matriz -> [Int]
maximos p = 
    [p!(i,j) | (i,j) <- indices p,
               and [p!(a,b) < p!(i,j) | (a,b) <- vecinos (i,j)]] 
    where (_,(m,n)) = bounds p
          vecinos (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)],
                                   b <- [max 1 (j-1)..min n (j+1)],
                                   (a,b) /= (i,j)]
