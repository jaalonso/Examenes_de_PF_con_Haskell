-- Informática (1º del Grado en Matemáticas)
-- 3º examen de evaluación continua (25 de enero de 2016)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Librerías auxiliares
-- ---------------------------------------------------------------------

import Data.Char
import Data.List
import Data.Numbers.Primes

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Un número n es autodescriptivo cuando para cada posición
-- k de n (empezando a contar las posiciones a partir de 0), el dígito
-- en la  posición k es igual al número de veces que ocurre k en n. Por
-- ejemplo, 1210 es autodescriptivo porque tiene 1 dígito igual a "0", 2
-- dígitos iguales a "1", 1 dígito igual a "2" y ningún dígito igual a
-- "3". 
--
-- Definir la función
--    autodescriptivo :: Integer -> Bool
-- tal que (autodescriptivo n) se verifica si n es autodescriptivo. Por
-- ejemplo, 
--    autodescriptivo 1210                      == True
--    [x | x <- [1..100000], autodescriptivo x] == [1210,2020,21200]
--    autodescriptivo 9210000001000             == True
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

autodescriptivo1 :: Integer -> Bool
autodescriptivo1 n = autodescriptiva (digitos n)

digitos :: Integer -> [Integer]
digitos n = [read [d] | d <- show n]

autodescriptiva :: [Integer] -> Bool
autodescriptiva ns = 
    and [x == ocurrencias k ns | (k,x) <- zip [0..] ns]

ocurrencias :: Integer -> [Integer] -> Integer
ocurrencias x ys = genericLength (filter (==x) ys)

-- 2ª solución
-- ===========

autodescriptivo2 :: Integer -> Bool
autodescriptivo2 n =
    and [length [y | y <- xs, read [y] == k] == read [x] | 
         (x,k) <- zip xs [0..]]
    where xs = show n

-- Comparación de eficiencia
-- =========================

--    ghci> [x | x <- [1..100000], autodescriptivo1 x]
--    [1210,2020,21200]
--    (6.94 secs, 3,380,620,264 bytes)
--    ghci> [x | x <- [1..100000], autodescriptivo2 x]
--    [1210,2020,21200]
--    (7.91 secs, 4,190,791,608 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir el procedimiento
--    autodescriptivos :: IO ()
-- que pregunta por un número n y escribe la lista de los n primeros
-- números autodescriptivos. Por ejemplo,
--    ghci> autodescriptivos
--    Escribe un numero: 2
--    Los 2 primeros numeros autodescriptivos son [1210,2020]
--    ghci> autodescriptivos
--    Escribe un numero: 3
--    Los 3 primeros numeros autodescriptivos son [1210,2020,21200]
-- ---------------------------------------------------------------------

autodescriptivos :: IO ()
autodescriptivos = do
  putStr "Escribe un numero: "
  xs <- getLine
  putStr ("Los " ++ xs ++ " primeros numeros autodescriptivos son ")
  putStrLn (show (take (read xs) [a | a <- [1..], autodescriptivo1 a]))

-- ---------------------------------------------------------------------
-- Ejercicio 2. Dos enteros positivos a y b se dirán relacionados si
-- poseen, exactamente, un factor primo en común. Por ejemplo, 12 y 20
-- están relacionados, pero 6 y 30 no lo están.  
-- 
-- Definir la lista infinita
--    paresRel :: [(Int,Int)]
-- tal que paresRel enumera todos los pares (a,b), con 1 <= a < b, 
-- tal que a y b están relacionados. Por ejemplo,
--    ghci> take 10 paresRel
--    [(2,4),(2,6),(3,6),(4,6),(2,8),(4,8),(6,8),(3,9),(6,9),(2,10)]
-- 
-- ¿Qué lugar ocupa el par (51,111) en la lista infinita paresRel?
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

paresRel1 :: [(Int,Int)]
paresRel1 = [(a,b) | b <- [1..], a <- [1..b-1], relacionados a b]

relacionados :: Int -> Int -> Bool
relacionados a b = 
    length (nub (primeFactors a `intersect` primeFactors b)) == 1

-- El cálculo es
--    ghci> 1 + length (takeWhile (/=(51,111)) paresRel1)
--    2016

-- 2ª solución
-- ===========

paresRel2 :: [(Int,Int)]
paresRel2 = [(x,y) | y <- [4..], x <- [2..y-2], rel x y]
    where rel x y = m /= 1 && all (== head ps) ps
              where m  = gcd x y
                    ps = primeFactors m

-- 3ª solución
-- ===========

paresRel3 :: [(Int,Int)]
paresRel3 =
    [(x,y) | y <- [2..], x <- [2..y-1], relacionados3 x y]

relacionados3 :: Int -> Int -> Bool
relacionados3 x y =
    length (group (primeFactors (gcd x y))) == 1

-- Comparación de eficiencia
--    ghci> paresRel1 !! 40000
--    (216,489)
--    (3.19 secs, 1,825,423,056 bytes)
--    ghci> paresRel2 !! 40000
--    (216,489)
--    (0.96 secs, 287,174,864 bytes)
--    ghci> paresRel3 !! 40000
--    (216,489)
--    (0.70 secs, 264,137,928 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    agrupa :: (a -> Bool) -> [a] -> [a]
-- tal que (agrupa p xs) es la lista obtenida separando los elementos
-- consecutivos de xs que verifican la propiedad p de los que no la
-- verifican. Por ejemplo, 
--    agrupa odd  [1,2,0,4,9,6,4,5,7,2]  ==  [[1],[2,0,4],[9],[6,4],[5,7],[2]]
--    agrupa even [1,2,0,4,9,6,4,5,7,2]  ==  [[],[1],[2,0,4],[9],[6,4],[5,7],[2]]
--    agrupa (>4) [1,2,0,4,9,6,4,5,7,2]  ==  [[],[1,2,0,4],[9,6],[4],[5,7],[2]]
--    agrupa (<4) [1,2,0,4,9,6,4,5,7,2]  ==  [[1,2,0],[4,9,6,4,5,7],[2]]
-- ---------------------------------------------------------------------

agrupa :: (a -> Bool) -> [a] -> [[a]] 
agrupa p [] = []
agrupa p xs = takeWhile p xs : agrupa (not . p) (dropWhile p xs)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Los árboles binarios con datos en nodos y hojas se
-- definen por
--    data Arbol a = H a | N a (Arbol a) (Arbol a) deriving Show
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
-- Definir la función 
--    sucesores :: Arbol a -> [(a,[a])]
-- tal que (sucesores t) es la lista de los pares formados por los
-- elementos del árbol t junto con sus sucesores. Por ejemplo,
--    ghci> sucesores ejArbol
--    [(3,[4,7]),(4,[5,0]),(5,[2,0]),(2,[]),(0,[]),(0,[]),
--     (7,[0,3]),(0,[]),(3,[])]
-- ---------------------------------------------------------------------

data Arbol a = H a | N a (Arbol a) (Arbol a) deriving Show

ejArbol :: Arbol Integer
ejArbol = N 3 (N 4 (N 5 (H 2)(H 0)) (H 0)) (N 7 (H 0) (H 3))

sucesores :: Arbol a -> [(a,[a])]
sucesores (H x)     = [(x,[])]
sucesores (N x i d) = (x, [raiz i, raiz d]) : sucesores i ++ sucesores d

raiz :: Arbol a -> a
raiz (H x)      = x
raiz (N x _ _ ) = x 
