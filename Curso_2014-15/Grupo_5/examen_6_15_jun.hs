-- Informática (1º del Grado en Matemáticas)
-- 6º examen de evaluación continua (15 de junio de 2015)
-- ---------------------------------------------------------------------

import Data.List 
import qualified Data.Map as M 
import I1M.BusquedaEnEspaciosDeEstados
import I1M.Grafo

-- ---------------------------------------------------------------------
-- Ejercicio 1. Una inversión de una lista xs es un par de elementos
-- (x,y) de xs tal que y está a la derecha de x en xs y además y es
-- menor que x. Por ejemplo, en la lista [1,7,4,9,5] hay tres
-- inversiones: (7,4), (7,5) y (9,5). 
-- 
-- Definir la función 
--    inversiones :: Ord a -> [a] -> [(a,a)]
-- tal que (inversiones xs) es la lista de las inversiones de xs. Por
-- ejemplo, 
--    inversiones [1,7,4,9,5]  ==  [(7,4),(7,5),(9,5)]
--    inversiones "esto"       ==  [('s','o'),('t','o')]
-- ---------------------------------------------------------------------

inversiones :: Ord a => [a] -> [(a,a)]
inversiones []     = []
inversiones (x:xs) = [(x,y) | y <- xs, y < x] ++ inversiones xs

-- ---------------------------------------------------------------------
-- Ejercicio 2. Las expresiones aritméticas se pueden representar como
-- árboles con números en las hojas y operaciones en los nodos. Por
-- ejemplo, la expresión "9-2*4" se puede representar por el árbol
--      - 
--     / \
--    9   *
--       / \
--      2   4
-- 
-- Definiendo el tipo de dato Arbol por 
--    data Arbol = H Int | N (Int -> Int -> Int) Arbol Arbol
-- la representación del árbol anterior es
--    N (-) (H 9) (N (*) (H 2) (H 4))
--
-- Definir la función
--    valor :: Arbol -> Int
-- tal que (valor a) es el valor de la expresión aritmética
-- correspondiente al árbol a. Por ejemplo, 
--    valor (N (-) (H 9) (N (*) (H 2) (H 4)))    ==  1
--    valor (N (+) (H 9) (N (*) (H 2) (H 4)))    ==  17
--    valor (N (+) (H 9) (N (div) (H 4) (H 2)))  ==  11
--    valor (N (+) (H 9) (N (max) (H 4) (H 2)))  ==  13
-- ---------------------------------------------------------------------

data Arbol = H Int | N (Int -> Int -> Int) Arbol Arbol

valor :: Arbol -> Int
valor (H x)     = x
valor (N f i d) = f (valor i) (valor d)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    agrupa :: Ord c => (a -> c) -> [a] -> M.Map c [a]
-- tal que (agrupa f xs) es el diccionario obtenido agrupando los
-- elementos de xs según sus valores mediante la función f. Por ejemplo,
--    ghci> agrupa length ["hoy", "ayer", "ana", "cosa"]
--    fromList [(3,["hoy","ana"]),(4,["ayer","cosa"])]
--    ghci> agrupa head ["claro", "ayer", "ana", "cosa"]
--    fromList [('a',["ayer","ana"]),('c',["claro","cosa"])]
--    ghci> agrupa length (words "suerte en el examen")
--    fromList [(2,["en","el"]),(6,["suerte","examen"])]
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión)
agrupa1 :: Ord c => (a -> c) -> [a] -> M.Map c [a]
agrupa1 _ []     = M.empty
agrupa1 f (x:xs) = M.insertWith (++) (f x) [x] (agrupa1 f xs)

-- 2ª definición (por plegado)
agrupa2 :: Ord c => (a -> c) -> [a] -> M.Map c [a]
agrupa2 f = foldr (\x -> M.insertWith (++) (f x) [x]) M.empty

-- ---------------------------------------------------------------------
-- Ejercicio 4. Los primeros términos de la sucesión de Fibonacci son
--    0, 1, 1, 2, 3, 5, 8, 13, 21, 34
-- Se observa que el 6º término de la sucesión (comenzando a contar en
-- 0) es el número 8.
--
-- Definir la función
--    indiceFib :: Integer -> Maybe Integer
-- tal que (indiceFib x) es justo el número n si x es el n-ésimo
-- términos de la sucesión de Fibonacci o Nothing en el caso de que x no
-- pertenezca a la sucesión. Por ejemplo,
--    indiceFib 8        ==  Just 6
--    indiceFib 9        ==  Nothing
--    indiceFib 21       ==  Just 8
--    indiceFib 22       ==  Nothing
--    indiceFib 9227465  ==  Just 35
--    indiceFib 9227466  ==  Nothing
-- ---------------------------------------------------------------------

indiceFib :: Integer -> Maybe Integer
indiceFib x | y == x    = Just n
            | otherwise = Nothing
    where (y,n) = head (dropWhile (\(z,m) -> z < x) fibsNumerados)

-- fibs es la lista de los términos de la sucesión de Fibonacci. Por
-- ejemplo, 
--    take 10 fibs  ==  [0,1,1,2,3,5,8,13,21,34]
fibs :: [Integer]
fibs = 0 : 1 : [x+y | (x,y) <- zip fibs (tail fibs)]

-- fibsNumerados es la lista de los términos de la sucesión de Fibonacci
-- juntos con sus posiciones. Por ejemplo,
--    ghci> take 10 fibsNumerados
--    [(0,0),(1,1),(1,2),(2,3),(3,4),(5,5),(8,6),(13,7),(21,8),(34,9)]
fibsNumerados :: [(Integer,Integer)]
fibsNumerados = zip fibs [0..]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir las funciones
--    grafo   :: [(Int,Int)] -> Grafo Int Int
--    caminos :: Grafo Int Int -> Int -> Int -> [[Int]]
-- tales que 
-- + (grafo as) es el grafo no dirigido definido cuyas aristas son as. Por
--   ejemplo, 
--      ghci> grafo [(2,4),(4,5)]
--      G ND (array (2,5) [(2,[(4,0)]),(3,[]),(4,[(2,0),(5,0)]),(5,[(4,0)])])
-- + (caminos g a b) es la lista los caminos en el grafo g desde a hasta
--   b sin pasar dos veces por el mismo nodo. Por ejemplo,
--      ghci> sort (caminos (grafo [(1,3),(2,5),(3,5),(3,7),(5,7)]) 1 7)
--      [[1,3,5,7],[1,3,7]]
--      ghci> sort (caminos (grafo [(1,3),(2,5),(3,5),(3,7),(5,7)]) 2 7)
--      [[2,5,3,7],[2,5,7]]
--      ghci> sort (caminos (grafo [(1,3),(2,5),(3,5),(3,7),(5,7)]) 1 2)
--      [[1,3,5,2],[1,3,7,5,2]]
--      ghci> caminos (grafo [(1,3),(2,5),(3,5),(3,7),(5,7)]) 1 4
--      []
-- ---------------------------------------------------------------------

grafo :: [(Int,Int)] -> Grafo Int Int
grafo as = creaGrafo ND (m,n) [(x,y,0) | (x,y) <- as]
    where ns = map fst as ++ map snd as
          m  = minimum ns
          n  = maximum ns

-- 1ª solución (mediante espacio de estados)
caminos1 :: Grafo Int Int -> Int -> Int -> [[Int]]
caminos1 g a b = buscaEE sucesores esFinal inicial
    where inicial          = [b]
          sucesores (x:xs) = [z:x:xs | z <- adyacentes g x
                                     , z `notElem` (x:xs)] 
          esFinal (x:xs)   = x == a

-- 2ª solución (sin espacio de estados)
caminos2 :: Grafo Int Int -> Int -> Int -> [[Int]]
caminos2 g a b = aux [[b]] where 
    aux [] = []
    aux ((x:xs):yss)
        | x == a    = (x:xs) : aux yss
        | otherwise = aux ([z:x:xs | z <- adyacentes g x
                                   , z `notElem` (x:xs)] 
                           ++ yss) 

-- =====================================================================

-- Tipos de ejercicios
--    |--------------------------------+----+-----+----+----+----|
--    |                                | E1 | E2  | E3 | E4 | E5 |
--    |--------------------------------+----+-----+----+----+----|
--    | R: Recursión                   | R  | R   | R  | R  | R  |
--    | C: Comprensión                 | C  |     |    |    |    |
--    | TDA: Tipo de datos algebraicos |    | TDA |    |    |    |
--    | OS: Orden superior             |    |     | OS |    | OS |
--    | D: Diccionarios                |    |     | D  |    |    |
--    | P: Plegado                     |    |     | P  |    |    |
--    | M: Maybe                       |    |     |    | M  |    |
--    | LI: Listas infinitas           |    |     |    | LI |    |
--    | PD: Programación dinámica      |    |     |    | PD |    |
--    | E: Emparejamiento con zip      |    |     |    | E  |    |
--    | G: Grafos                      |    |     |    |    | G  |
--    | EE: Espacio de estados         |    |     |    |    | EE |
--    |--------------------------------+----+-----+----+----+----|

