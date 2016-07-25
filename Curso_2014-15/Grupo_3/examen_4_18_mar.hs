-- Informática (1º del Grado en Matemáticas)
-- 4º examen de evaluación continua (18 de marzo de 2015)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Array
import I1M.Pol
import Data.Numbers.Primes

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función
--    agrupa :: Eq a => [a] -> [(a,Int)]
-- tal que (agrupa xs) es la lista obtenida agrupando las ocurrencias
-- consecutivas de elementos de xs junto con el número de dichas
-- ocurrencias. Por ejemplo: 
--    agrupa "aaabzzaa" == [('a',3),('b',1),('z',2),('a',2)] 
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión)
agrupa :: Eq a => [a] -> [(a,Int)]
agrupa xs = aux xs 1
    where aux (x:y:zs) n | x == y    = aux (y:zs) (n+1)
                         | otherwise = (x,n) : aux (y:zs) 1
          aux [x]      n             = [(x,n)]

-- 2ª definición (por recursión usando takeWhile):
agrupa2 :: Eq a => [a] -> [(a,Int)]
agrupa2 [] = []
agrupa2 (x:xs) = 
    (x,1 + length (takeWhile (==x) xs)) : agrupa2 (dropWhile (==x) xs)

-- 3ª definición (por comprensión usando group):
agrupa3 :: Eq a => [a] -> [(a,Int)]
agrupa3 xs = [(head ys,length ys) | ys <- group xs]

-- 4ª definición (usando map y group):
agrupa4 :: Eq a => [a] -> [(a,Int)]
agrupa4 = map (\xs -> (head xs, length xs)) . group

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función expande
--    expande :: [(a,Int)] -> [a]
-- tal que (expande xs) es la lista expandida correspondiente a ps (es
-- decir, es la lista xs tal que la comprimida de xs es ps. Por ejemplo, 
--    expande [('a',2),('b',3),('a',1)] == "aabbba"
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión)
expande :: [(a,Int)] -> [a]
expande ps = concat [replicate k x | (x,k) <- ps]

-- 2ª definición (por concatMap)
expande2 :: [(a,Int)] -> [a]
expande2 = concatMap (\(x,k) -> replicate k x) 

-- 3ª definición (por recursión)
expande3 :: [(a,Int)] -> [a]
expande3 [] = []
expande3 ((x,n):ps) = replicate n x ++ expande3 ps

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Dos enteros positivos a y b se dirán relacionados 
-- si poseen, exactamente, un factor primo en común. Por ejemplo, 12 y
-- 14 están relacionados pero 6 y 30 no lo están. 
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

paresRel :: [(Int,Int)]
paresRel = [(a,b) | b <- [1..], a <- [1..b-1], relacionados a b]

relacionados :: Int -> Int -> Bool
relacionados a b = 
    length (nub (primeFactors a `intersect` primeFactors b)) == 1

-- El cálculo es
--    ghci> 1 + length (takeWhile (/=(51,111)) paresRel)
--    2016

-- ---------------------------------------------------------------------
-- Ejercicio 3.  Representamos árboles binarios con elementos en las
-- hojas y en los nodos mediante el tipo de dato 
--    data Arbol a = H a | N a (Arbol a) (Arbol a) deriving Show
-- Por ejemplo,
--    ej1 :: Arbol Int
--    ej1 = N 5 (N 2 (H 1) (H 2)) (N 3 (H 4) (H 2))
-- 
-- Definir la función
--    ramasCon :: Eq a => Arbol a -> a -> [[a]]
-- tal que (ramasCon a x) es la lista de las ramas del árbol a en las
-- que aparece el elemento x. Por ejemplo,
--   ramasCon ej1 2 ==  [[5,2,1],[5,2,2],[5,3,2]]
-- ---------------------------------------------------------------------

data Arbol a = H a | N a (Arbol a) (Arbol a) deriving Show

ej1 :: Arbol Int
ej1 = N 5 (N 2 (H 1) (H 2)) (N 3 (H 4) (H 2))

-- 1ª definición
-- =============

ramasCon :: Eq a => Arbol a -> a -> [[a]]
ramasCon a x = [ys | ys <- ramas a, x `elem` ys]

ramas :: Arbol a -> [[a]]
ramas (H x)     = [[x]]
ramas (N x i d) = [x:ys | ys <- ramas i ++ ramas d]

-- 2ª definición
-- =============

ramasCon2 :: Eq a => Arbol a -> a -> [[a]]
ramasCon2 a x = filter (x `elem`) (ramas2 a)

ramas2 :: Arbol a -> [[a]]
ramas2 (H x)     = [[x]]
ramas2 (N x i d) = map (x:) (ramas2 i ++ ramas2 d)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Representamos matrices mediante el tipo de dato
--    type Matriz a = Array (Int,Int) a
-- Por ejemplo,
--    ejM :: Matriz Int
--    ejM = listArray ((1,1),(2,4)) [1,2,3,0,4,5,6,7]
-- representa la matriz
--    |1 2 3 0|
--    |4 5 6 7|
-- 
-- Definir la función 
--    ampliada :: Num a => Matriz a -> Matriz a
-- tal que (ampliada p) es la matriz obtenida al añadir una nueva fila 
-- a p cuyo elemento i-ésimo es la suma de la columna i-ésima de p. 
-- Por ejemplo,
--    |1 2 3 0|        |1 2 3 0|
--    |4 5 6 7| ==>    |4 5 6 7| 
--                     |5 7 9 7|
-- En Haskell,
--    ghci> ampliada ejM
--    array ((1,1),(3,4)) [((1,1),1),((1,2),2),((1,3),3),((1,4),0),
--                         ((2,1),4),((2,2),5),((2,3),6),((2,4),7),
--                         ((3,1),5),((3,2),7),((3,3),9),((3,4),7)]
-- ---------------------------------------------------------------------

type Matriz a = Array (Int,Int) a

ejM :: Matriz Int
ejM = listArray ((1,1),(2,4)) [1,2,3,0,4,5,6,7]

ampliada :: Num a => Matriz a -> Matriz a
ampliada p = 
    array ((1,1),(m+1,n)) [((i,j),f i j) | i <- [1..m+1], j <- [1..n]]
    where (_,(m,n)) = bounds p
          f i j | i <= m    = p!(i,j)
                | otherwise = sum [p!(i,j) | i <- [1..m]] 

-- ---------------------------------------------------------------------
-- Ejercicio 5. Un polinomio de coeficientes enteros se dirá par si
-- todos sus coeficientes son números pares. Por ejemplo, el polinomio
-- 2*x^3 - 4*x^2 + 8 es par y el x^2 + 2*x + 10 no lo es.

-- Definir el predicado
--    parPol :: Integral a => Polinomio a -> Bool
-- tal que (parPol p) se verifica si p es un polinomio par. Por ejemplo, 
--    ghci> parPol (consPol 3 2 (consPol 2 (-4) (consPol 0 8 polCero)))
--    True
--    ghci> parPol (consPol 2 1 (consPol 1 2 (consPol 0 10 polCero)))
--    False
-- ---------------------------------------------------------------------

parPol :: Integral a => Polinomio a -> Bool
parPol p = esPolCero p || (even (coefLider p) && parPol (restoPol p))
