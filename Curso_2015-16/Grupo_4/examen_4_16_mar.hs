-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 4º examen de evaluación continua (16 de marzo de 2016)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Array
import Data.Char
import Data.Numbers.Primes
import qualified Data.Matrix as M

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    siguiente :: Eq a => a -> [a] -> Maybe a
-- tal que (siguiente x ys) es justo el elemento siguiente a la primera 
-- ocurrencia de x en ys o Nothing si x no pertenece a ys. Por ejemplo,
--    siguiente 5 [3,5,2,5,7]                       ==  Just 2
--    siguiente 9 [3,5,2,5,7]                       ==  Nothing
--    siguiente 'd' "afdegdb"                       ==  Just 'e'
--    siguiente "todo" ["En","todo","la","medida"]  ==  Just "la"
--    siguiente "nada" ["En","todo","la","medida"]  ==  Nothing
--    siguiente 999999 [1..1000000]                 ==  Just 1000000
--    siguiente 1000000 [1..1000000]                ==  Nothing
-- ---------------------------------------------------------------------

-- 1ª solución (por recursión):
siguiente1 :: Eq a => a -> [a] -> Maybe a
siguiente1 x (y1:y2:ys) | x == y1   = Just y2
                        | otherwise = siguiente1 x (y2:ys)
siguiente1 x _ = Nothing

-- 2ª solución (por comprensión):
siguiente2 :: Eq a => a -> [a] -> Maybe a
siguiente2 x ys 
    | null zs   = Nothing
    | otherwise = Just (snd (head zs))
    where zs = [(u,v) | (u,v) <- zip ys (tail ys), u == x]  

-- 3ª solución (con dropWhile)
siguiente3 :: Eq a => a -> [a] -> Maybe a
siguiente3 x = aux . drop 1 . dropWhile (/=x)
    where aux []    = Nothing
          aux (y:_) = Just y

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    ghci> let n=10^6 in siguiente1 (n-1) [1..n]
--    Just 1000000
--    (1.34 secs, 277352616 bytes)
--    
--    ghci> let n=10^6 in siguiente2 (n-1) [1..n]
--    Just 1000000
--    (1.45 secs, 340836576 bytes)
--    
--    ghci> let n=10^6 in siguiente3 (n-1) [1..n]
--    Just 1000000
--    (0.26 secs, 84987544 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Un número n es k-belga si la sucesión cuyo primer
-- elemento es k y cuyos elementos se obtienen sumando reiteradamente
-- los dígitos de n contiene a n. Por ejemplo,
-- + El 18 es 0-belga, porque a partir del 0 vamos a ir sumando
--   sucesivamente 1, 8, 1, 8, ... hasta llegar o sobrepasar el 18: 0, 1,
--   9, 10, 18, ... Como se alcanza el 18, resulta que el 18 es 0-belga. 
-- + El 19 no es 1-belga, porque a partir del 1 vamos a ir sumando
--   sucesivamente 1, 9, 1, 9, ... hasta llegar o sobrepasar el 18: 1, 2,
--   11, 12, 21, 22, ... Como no se alcanza el 19, resulta que el 19 no es
--   1-belga. 
--
-- Definir la función 
--    esBelga :: Int -> Int -> Bool
-- tal que (esBelga k n)  se verifica si n es k-belga. Por ejemplo,
--    esBelga 0 18                              ==  True
--    esBelga 1 19                              ==  False
--    esBelga 0 2016                            ==  True
--    [x | x <- [0..30], esBelga 7 x]           ==  [7,10,11,21,27,29]
--    [x | x <- [0..30], esBelga 10 x]          ==  [10,11,20,21,22,24,26]
--    length [n | n <- [1..9000], esBelga 0 n]  ==  2857
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

esBelga1 :: Int -> Int -> Bool
esBelga1 k n =
    n == head (dropWhile (<n) (scanl (+) k (cycle (digitos n))))

digitos :: Int -> [Int]
digitos n = map digitToInt (show n)

-- 2ª solución
-- ===========

esBelga2 :: Int -> Int -> Bool
esBelga2 k n =
    k <= n && n == head (dropWhile (<n) (scanl (+) (k + q * s) ds))
    where ds = digitos n
          s  = sum ds
          q  = (n - k) `div` s

-- Comparación de eficiencia
-- =========================

--    ghci> length [n | n <- [1..9000], esBelga1 0 n]
--    2857
--    (2.95 secs, 1,115,026,728 bytes)
--    ghci> length [n | n <- [1..9000], esBelga2 0 n]
--    2857
--    (0.10 secs, 24,804,480 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Los árboles binarios con datos en los nodos y hojas se
-- definen por 
--    data Arbol a = H a
--                 | N a (Arbol a) (Arbol a) 
--                 deriving (Eq, Show)
-- Por ejemplo, el árbol 
--           3
--          / \
--         /   \
--        4     7
--       / \   / \
--      5   0 0   3
--     / \
--    2   0   
-- se representa por
--    ejArbol :: Arbol Integer
--    ejArbol = N 3 (N 4 (N 5 (H 2)(H 0)) (H 0)) (N 7 (H 0) (H 3))
--
-- Anotando cada elemento del árbol anterior con su profundidad, se
-- obtiene el árbol siguiente  
--           3-0
--           / \
--          /   \
--         /     \
--       4-1     7-1
--       / \     / \
--     5-2 0-2 0-2 3-2
--     / \
--   2-3 0-3   
--
-- Definir la función 
--    anotado :: Arbol a -> Arbol (a,Int)
-- tal que (anotado x) es el árbol obtenido anotando los elementos de x
-- con su profundidad. Por ejemplo,
--    ghci> anotado ejArbol
--    N (3,0) 
--      (N (4,1) 
--         (N (5,2) (H (2,3)) (H (0,3))) 
--         (H (0,2))) 
--      (N (7,1) (H (0,2)) (H (3,2)))
-- ---------------------------------------------------------------------

data Arbol a = H a 
             | N a (Arbol a) (Arbol a) 
             deriving (Eq, Show)

ejArbol :: Arbol Integer
ejArbol = N 3 (N 4 (N 5 (H 2)(H 0)) (H 0)) (N 7 (H 0) (H 3))

-- 1ª solución
-- ===========

anotado1 :: Arbol a -> Arbol (a,Int)
anotado1 (H x)     = H (x,0)
anotado1 (N x i d) = aux (N x i d) 0
    where aux (H x)     n = H (x,n)
          aux (N x i d) n = N (x,n) (aux i (n+1)) (aux d (n+1))  

-- 2ª solución
anotado2 :: Arbol a -> Arbol (a, Int)
anotado2 a = aux a [0..]
    where aux (H a)     (n:_ ) = H (a,n)
          aux (N a i d) (n:ns) = N (a,n) (aux i ns) (aux d ns)

-- ---------------------------------------------------------------------
-- Ejercicio 4. El pasado 11 de marzo se ha publicado el artículo
-- "Unexpected biases in the distribution of consecutive primes" en el
-- que muestra que los números primos repelen a otros primos que
-- terminan en el mismo dígito. 
--
-- La lista de los últimos dígitos de los 30 primeros números es
--    [2,3,5,7,1,3,7,9,3,9,1,7,1,3,7,3,9,1,7,1,3,9,3,9,7,1,3,7,9,3]
-- Se observa que hay 6 números que su último dígito es un 1 y de sus
-- consecutivos 4 terminan en 3 y 2 terminan en 7.
--
-- Definir la función 
--    distribucionUltimos :: Int -> M.Matrix Int
-- tal que (distribucionUltimos n) es la matriz cuyo elemento (i,j)
-- indica cuántos de los n primeros números primos terminan en i y su
-- siguiente número primo termina en j. Por ejemplo,
--    ghci> distribucionUltimos 30
--    ( 0 0 4 0 0 0 2 0 0 )
--    ( 0 0 1 0 0 0 0 0 0 )
--    ( 0 0 0 0 1 0 4 0 4 )
--    ( 0 0 0 0 0 0 0 0 0 )
--    ( 0 0 0 0 0 0 1 0 0 )
--    ( 0 0 0 0 0 0 0 0 0 )
--    ( 4 0 1 0 0 0 0 0 2 )
--    ( 0 0 0 0 0 0 0 0 0 )
--    ( 2 0 3 0 0 0 1 0 0 )
--    
--    ghci> distribucionUltimos (10^5)
--    ( 4104    0 7961    0    0    0 8297    0 4605 )
--    (    0    0    1    0    0    0    0    0    0 )
--    ( 5596    0 3604    0    1    0 7419    0 8387 )
--    (    0    0    0    0    0    0    0    0    0 )
--    (    0    0    0    0    0    0    1    0    0 )
--    (    0    0    0    0    0    0    0    0    0 )
--    ( 6438    0 6928    0    0    0 3627    0 8022 )
--    (    0    0    0    0    0    0    0    0    0 )
--    ( 8830    0 6513    0    0    0 5671    0 3995 )
--
-- Nota: Se observa cómo se "repelen" ya que en las filas del 1, 3, 7 y
-- 9 el menor elemento es el de la diagonal.
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

distribucionUltimos1 :: Int -> M.Matrix Int
distribucionUltimos1 n =
    M.matrix 9 9 
             (\(i,j) -> length (filter (==(i,j)) (take n ultimosConsecutivos)))

-- (ultimo n) es el último dígito de n.    
ultimo :: Int -> Int
ultimo n = n `mod` 10

-- ultimos es la lista de el último dígito de los primos.
--    ghci> take 20 ultimos
--    [2,3,5,7,1,3,7,9,3,9,1,7,1,3,7,3,9,1,7,1]
ultimos :: [Int]
ultimos = map ultimo primes

-- ultimosConsecutivos es la lista de los últimos dígitos de los primos
-- consecutivos. 
--    ghci> take 10 ultimosConsecutivos
--    [(2,3),(3,5),(5,7),(7,1),(1,3),(3,7),(7,9),(9,3),(3,9),(9,1)]
ultimosConsecutivos :: [(Int,Int)]
ultimosConsecutivos = zip ultimos (tail ultimos)

-- 2ª solución
-- ===========

distribucionUltimos2 :: Int -> M.Matrix Int
distribucionUltimos2 n =
    M.fromList 9 9 
               (elems (histograma ((1,1),(9,9)) (take n ultimosConsecutivos)))

-- (histograma r is) es el vector formado contando cuantas veces
-- aparecen los elementos del rango r en la lista de índices is. Por
-- ejemplo, 
--    ghci> histograma (0,5) [3,1,4,1,5,4,2,7]
--    array (0,5) [(0,0),(1,2),(2,1),(3,1),(4,2),(5,1)]
histograma :: (Ix a, Num b) => (a,a) -> [a] -> Array a b
histograma r is = 
    accumArray (+) 0 r [(i,1) | i <- is, inRange r i]

-- 3ª definición
-- =============

distribucionUltimos3 :: Int -> M.Matrix Int
distribucionUltimos3 n 
    | n < 4 = distribucionUltimos1 n
    | otherwise =  M.matrix 9 9 (\(i,j) -> f i j)
    where f i j | elem (i,j) [(2,3),(3,5),(5,7)] = 1
                | even i || even j = 0
                | otherwise = length (filter (==(i,j))
                                             (take n ultimosConsecutivos))

-- Comparación de eficiencia
-- =========================

--    ghci> distribucionUltimos1 (10^5)
--    ( 4104    0 7961    0    0    0 8297    0 4605 )
--    (    0    0    1    0    0    0    0    0    0 )
--    ( 5596    0 3604    0    1    0 7419    0 8387 )
--    (    0    0    0    0    0    0    0    0    0 )
--    (    0    0    0    0    0    0    1    0    0 )
--    (    0    0    0    0    0    0    0    0    0 )
--    ( 6438    0 6928    0    0    0 3627    0 8022 )
--    (    0    0    0    0    0    0    0    0    0 )
--    ( 8830    0 6513    0    0    0 5671    0 3995 )
--    
--    (3.51 secs, 941,474,520 bytes)
--    ghci> distribucionUltimos2 (10^5)
--    ( 4104    0 7961    0    0    0 8297    0 4605 )
--    (    0    0    1    0    0    0    0    0    0 )
--    ( 5596    0 3604    0    1    0 7419    0 8387 )
--    (    0    0    0    0    0    0    0    0    0 )
--    (    0    0    0    0    0    0    1    0    0 )
--    (    0    0    0    0    0    0    0    0    0 )
--    ( 6438    0 6928    0    0    0 3627    0 8022 )
--    (    0    0    0    0    0    0    0    0    0 )
--    ( 8830    0 6513    0    0    0 5671    0 3995 )
--    
--    (1.75 secs, 560,891,792 bytes)
--    ghci> distribucionUltimos3 (10^5)
--    ( 4104    0 7961    0    0    0 8297    0 4605 )
--    (    0    0    1    0    0    0    0    0    0 )
--    ( 5596    0 3604    0    1    0 7419    0 8387 )
--    (    0    0    0    0    0    0    0    0    0 )
--    (    0    0    0    0    0    0    1    0    0 )
--    (    0    0    0    0    0    0    0    0    0 )
--    ( 6438    0 6928    0    0    0 3627    0 8022 )
--    (    0    0    0    0    0    0    0    0    0 )
--    ( 8830    0 6513    0    0    0 5671    0 3995 )
--    
--    (1.70 secs, 623,371,360 bytes)

