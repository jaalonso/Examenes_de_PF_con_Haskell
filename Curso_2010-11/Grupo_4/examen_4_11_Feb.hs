-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 1º examen de evaluación continua (11 de febrero de 2011)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. (Problema 303 del proyecto Euler)
-- Definir la función
--    multiplosRestringidos :: Int -> (Int -> Bool) -> [Int]
-- tal que (multiplosRestringidos n x) es la lista de los múltiplos de n
-- cuyas cifras verifican la propiedad p. Por ejemplo, 
--    take 4 (multiplosRestringidos 5 (<=3))  ==  [10,20,30,100]
--    take 5 (multiplosRestringidos 3 (<=4))  ==  [3,12,21,24,30]
--    take 5 (multiplosRestringidos 3 even)   ==  [6,24,42,48,60]
-- ---------------------------------------------------------------------

multiplosRestringidos :: Int -> (Int -> Bool) -> [Int]
multiplosRestringidos n p = 
    [y | y <- [n,2*n..], and [p x | x <- cifras y]]

-- (cifras n) es la lista de las cifras de n, Por ejemplo, 
--    cifras 327  ==  [3,2,7]
cifras :: Int -> [Int]
cifras n = [read [x] | x <- show n]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    sumaDeDosPrimos :: Int -> [(Int,Int)]
-- tal que (sumaDeDosPrimos n) es la lista de las distintas
-- descomposiciones de n como suma de dos nuúmeros primos. Por ejemplo, 
--    sumaDeDosPrimos 30  ==  [(7,23),(11,19),(13,17)]
-- Calcular, usando la función sumaDeDosPrimos, el menor número que
-- puede escribirse de 10 formas distintas como suma de dos primos.
-- ---------------------------------------------------------------------

sumaDeDosPrimos :: Int -> [(Int,Int)]
sumaDeDosPrimos n = 
    [(x,n-x) | x <- primosN, x < n-x, n-x `elem` primosN]
    where primosN = takeWhile (<=n) primos

-- primos es la lista de los números primos
primos :: [Int]
primos = criba [2..]
    where criba []     = []
          criba (n:ns) = n : criba (elimina n ns)
          elimina n xs = [x | x <- xs, x `mod` n /= 0]

-- El cálculo es
--    ghci> head [x | x <- [1..], length (sumaDeDosPrimos x) == 10]
--    114

-- ---------------------------------------------------------------------
-- Ejercicio 3. Se consideran los árboles binarios
-- definidos por 
--    data Arbol = H Int 
--               | N Arbol Int Arbol
--               deriving (Show, Eq)
-- Por ejemplo, el árbol
--         5 
--        / \
--       /   \
--      9     7
--     / \   / \  
--    1   4 6   8  
-- se representa por
--    N (N (H 1) 9 (H 4)) 5 (N (H 6) 7 (H 8))
-- Definir la función
--    maximoArbol :: Arbol -> Int
-- tal que (maximoArbol a) es el máximo valor en el árbol a. Por
-- ejemplo, 
--    maximoArbol (N (N (H 1) 9 (H 4)) 5 (N (H 6) 7 (H 8)))  ==  9
-- ---------------------------------------------------------------------

data Arbol = H Int 
           | N Arbol Int Arbol
           deriving (Show, Eq)

maximoArbol :: Arbol -> Int
maximoArbol (H x) = x
maximoArbol (N i x d) = maximum [x, maximoArbol i, maximoArbol d]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    segmentos :: (a -> Bool) -> [a] -> [a]
-- tal que (segmentos p xs) es la lista de los segmentos de xs de cuyos
-- elementos verifican la propiedad p. Por ejemplo,
--    segmentos even [1,2,0,4,5,6,48,7,2]  ==  [[],[2,0,4],[6,48],[2]]
-- ---------------------------------------------------------------------

segmentos _ [] = []
segmentos p xs = 
    takeWhile p xs : segmentos p (dropWhile (not.p) (dropWhile p xs))
