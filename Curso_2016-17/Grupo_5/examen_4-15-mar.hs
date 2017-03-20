-- Informática (1º del Grado en Matemáticas)
-- 4º examen de evaluación continua (15 de marzo de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Array
import Data.Maybe

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir el predicado
--    relacionados :: Int -> Int -> Bool
-- tal que (relacionados x y) se verifica si los enteros positivos x e y
-- contienen los mismos dígitos (sin importar el orden o la repetición
-- de los mismos). Por ejemplo:
--    relacionados 12 1121   == True
--    relacionados 12 123    == False
-- ---------------------------------------------------------------------

relacionados :: Int -> Int -> Bool
relacionados x y =
  sort (nub (show x)) == sort (nub (show y))

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la lista infinita
--    paresRel :: [(Int,Int)]
-- cuyo elementos son los pares de enteros positivos (a,b), con
-- 1 <= a < b, tales que a y b están relacionados. Por ejemplo,
--    ghci> take 8 paresRel
--    [(1,11),(12,21),(2,22),(13,31),(23,32),(3,33),(14,41),(24,42)]
-- ---------------------------------------------------------------------

paresRel :: [(Int,Int)]
paresRel = [(a,b) | b <- [2..]
                  , a <- [1..b-1]
                  , relacionados a b]

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la función
--    lugar :: Int -> Int -> Maybe Int
-- tal que (lugar x y) es el lugar que ocupa el par (x,y) en la lista
-- infinita paresRel o bien Nothing si dicho par no está en la lista.   
-- Por ejemplo,
--    lugar 4 44  == Just 10
--    lugar 5 115 == Nothing
-- ---------------------------------------------------------------------

lugar :: Int -> Int -> Maybe Int
lugar x y | relacionados x y = Just z
          | otherwise        = Nothing
   where z = 1 + length (takeWhile (/=(x,y)) paresRel)          

-- ---------------------------------------------------------------------
-- Ejercicio 2. Representamos los árboles binarios con elementos en las
-- hojas y en los nodos mediante el tipo de dato  
--    data Arbol a = H a
--                 | N a (Arbol a) (Arbol a)
--      deriving Show
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

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
  deriving Show

ej1 :: Arbol Int
ej1 = N 5 (N 2 (H 1) (H 2)) (N 3 (H 4) (H 2))

ramasCon :: Eq a => Arbol a -> a -> [[a]]
ramasCon a x = filter (x `elem`) (ramas a)

ramas :: Arbol a -> [[a]]
ramas (H x)     = [[x]]
ramas (N x i d) = map (x:) (ramas i) ++ map (x:) (ramas d)

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Representamos las matrices mediante el tipo de dato 
--    type Matriz a = Array (Int,Int) a
-- Por ejemplo,
--    ejm :: matriz int
--    ejm = listarray ((1,1),(3,4)) [1,2,3,0,4,5,6,7,7,5,1,11]
-- representa la matriz
--    |1 2 3 0 |
--    |4 5 6 7 |
--    |7 5 1 11|
--
-- definir la función
--   cruz :: (eq a,num a) => matriz a -> int -> int -> matriz a
-- tal que (cruz p i j) es la matriz obtenida anulando todas las 
-- posiciones de p excepto las de la fila i y la columna j. por ejmplo,
--    ghci> cruz ejM 2 3
--    array ((1,1),(3,4)) [((1,1),0),((1,2),0),((1,3),3),((1,4),0),
--                         ((2,1),4),((2,2),5),((2,3),6),((2,4),7),
--                         ((3,1),0),((3,2),0),((3,3),1),((3,4),0)]
--    ghci> elems (cruz ejM 2 3)
--    [0,0,3,0,
--     4,5,6,7,
--     0,0,1,0]
-- ---------------------------------------------------------------------

type Matriz a = Array (Int,Int) a

ejM :: Matriz Int
ejM = listArray ((1,1),(3,4)) [1,2,3,0,4,5,6,7,7,5,1,11]

cruz :: Num a => Matriz a -> Int -> Int -> Matriz a
cruz p fil col =
  array (bounds p) [((i,j), f i j) | (i,j) <- indices p]
  where f i j | i == fil || j == col = p!(i,j)
              | otherwise = 0   

-- 2ª definición
cruz2 :: Num a => Matriz a -> Int -> Int -> Matriz a
cruz2 p fil col =
  p // [((i,j),0) | (i,j) <- indices p, i /= fil, j /= col]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Una matriz está ordenada por columnas si cada una de
-- sus columnas está ordenada en orden creciente.
-- 
-- Definir la función
--    ordenadaCol :: Ord a => Matriz a -> Bool
-- tal que (ordenadaCol p) se verifica si la matriz p está ordenada por
-- columnas. Por ejemplo,
--    ghci> ordenadaCol (listArray ((1,1),(3,4)) [1..12])
--    True
--    ghci> ordenadaCol (listArray ((1,1),(3,4)) [1,2,3,0,4,5,6,7,7,5,1,11])
--    False
-- ---------------------------------------------------------------------

ordenadaCol :: Ord a => Matriz a -> Bool
ordenadaCol p = and [xs == sort xs | xs <- columnas p]

columnas :: Matriz a -> [[a]]
columnas p = [[p!(i,j) | i <- [1..n]] | j <- [1..m]]       
   where (n,m) = snd (bounds p)

-- --------------------------------------------------------------------
-- Ejercicio 4.1. Representamos los pesos de un conjunto de objetos
-- mediante una lista de asociación (objeto,peso). Por ejemplo,
--    ejLista :: [(Char,Int)]
--    ejLista = [('a',10),('e',5),('i',7),('l',2),('s',1),('v',4)]
--
-- Definir la función
--    peso :: Eq a => [(a,Int)] -> [a] -> Int
-- tal que (peso xs as) es el peso del conjunto xs de acuerdo con la
-- lista de asociación as. Por ejemplo:
--    peso ejLista "sevilla" == 31
-- ---------------------------------------------------------------------

ejLista :: [(Char,Int)]
ejLista = [('a',10),('e',5),('i',7),('l',2),('s',1),('v',4)]

peso :: Eq a => [(a,Int)] -> [a] -> Int
peso as xs = sum [busca x as | x <- xs]       
  where busca x as = head [z | (y,z) <- as, y == x]

-- 2ª definición
peso2 :: Eq a => [(a,Int)] -> [a] -> Int
peso2 as xs = sum [fromJust (lookup x as) | x <- xs]       

-- 3ª definición
peso3 :: Eq a => [(a,Int)] -> [a] -> Int
peso3 as = sum . map (fromJust . (`lookup` as)) 

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función 
--    conPeso :: Eq a => Int -> [(a,Int)] -> [a] -> [[a]]
-- tal que (conPeso x as xs) es la lista de los subconjuntos de xs con
-- peso x de acuerdo con la asignación as. Por ejemplo:
--    conPeso 10 ejLista "sevilla" ==  ["sev","sell","sil","sil","a"]
-- ---------------------------------------------------------------------

conPeso :: Eq a => Int -> [(a,Int)] -> [a] -> [[a]]
conPeso x as xs =
  [ys | ys <- subsequences xs, peso as ys == x]
