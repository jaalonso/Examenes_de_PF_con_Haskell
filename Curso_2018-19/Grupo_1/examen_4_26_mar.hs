-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 4º examen de evaluación continua (26 de marzo de 2019)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Array
import Data.Maybe
import I1M.Cola

-- --------------------------------------------------------------------
-- Ejercicio 1.1. Se considera el tipo de dato de las matrices:
--    type Matriz a = Array (Int,Int) a
--
-- Dada una posicion (i,j) en una matriz, los vecinos a distancia d de la
-- posicion son aquellos valores que están a d pasos de la posicion (i,j)
-- contando los pasos en vertical y/o horizontal.
-- 
-- Definir la función
--    vecinosD :: (Int,Int) -> Matriz Integer -> Int -> [Integer]
-- tal que (vecinosD p a d) es la lista de los vecinos en la matriz a
-- de la posición p que están a distancia d de p. Por ejemplo, sea ejM la
-- definida por  
--    ejM1 = listArray ((1,1),(5,5)) [1..25]
--    ejM2 = listArray ((1,1),(3,4)) ['a'..]
-- entonces,
--    vecinosD (1,1) ejM1 1  ==  [2,6]
--    vecinosD (1,1) ejM1 2  ==  [3,7,11]
--    vecinosD (1,1) ejM1 3  ==  [4,8,12,16]
--    vecinosD (5,5) ejM1 1  ==  [20,24]
--    vecinosD (5,4) ejM1 1  ==  [19,23,25]
--    vecinosD (5,4) ejM1 3  ==  [9,13,15,17,21]
--    vecinosD (2,2) ejM1 6  ==  [25]
--    vecinosD (2,2) ejM1 7  ==  []
--    vecinosD (2,3) ejM2 2  ==  "bdejl"
-- ---------------------------------------------------------------------

type Matriz a = Array (Int,Int) a

ejM1 :: Matriz Int
ejM1 = listArray ((1,1),(5,5)) [1..25]

ejM2 :: Matriz Char
ejM2 = listArray ((1,1),(3,4)) ['a'..]

vecinosD :: (Int,Int) -> Matriz a -> Int -> [a]
vecinosD (i,j) a d =
  [a!(k,l) | (k,l) <- indices a
           ,  abs(i-k) + abs(j-l) == d]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función
--    circuloDeVecinos :: (Int,Int) -> Matriz Integer -> [[Integer]]
-- tal que (circuloDeVecinos p a) es la lista de listas de los vecinos
-- en a de p que están a distancia 1, 2, ... mientras tenga sentido. 
-- Por ejemplo,
--   λ> circuloDeVecinos (2,3) ejM1
--   [[3,7,9,13],[2,4,6,10,12,14,18],[1,5,11,15,17,19,23],[16,20,22,24],[21,25]]
--   λ> circuloDeVecinos (2,3) ejM2
--   ["cfhk","bdejl","ai"]
-- ---------------------------------------------------------------------

circuloDeVecinos :: (Int,Int) -> Matriz a -> [[a]]
circuloDeVecinos p a = 
  takeWhile (not . null) [vecinosD p a d | d <- [1..]] 

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la función
--    minimasDistancias :: Eq a => Matriz a -> a -> Matriz Int 
-- tal que, (minimasDistancias a x) es la matriz de las distancias de
-- cada elemento al elemento x más cercano, donde x es un elemento de la
-- matriz a. Por ejemplo, 
--    λ> elems (minimasDistancias ejM1 7)
--    [2,1,2,3,4,
--     1,0,1,2,3,
--     2,1,2,3,4,
--     3,2,3,4,5,
--     4,3,4,5,6]
--    λ> elems (minimasDistancias ejM1 12)
--    [3,2,3,4,5,
--     2,1,2,3,4,
--     1,0,1,2,3,
--     2,1,2,3,4,
--     3,2,3,4,5]
--    λ> elems (minimasDistancias ejM2 'c')
--    [2,1,0,1,
--     3,2,1,2,
--     4,3,2,3]
-- ---------------------------------------------------------------------

minimasDistancias :: Eq a => Matriz a -> a -> Matriz Int 
minimasDistancias a x =
  array (bounds a) [((i,j), f i j) | (i,j) <- indices a]
    where f i j | a!(i,j) == x = 0
                | otherwise    = minimaDistancia (i,j) a x

minimaDistancia :: Eq a => (Int,Int) -> Matriz a -> a -> Int 
minimaDistancia p a x =
  head [d | (d,vs) <- zip [1..] (circuloDeVecinos p a)
          , x `elem` vs]

-- --------------------------------------------------------------------
-- Ejercicio 2.1. Se considera el siguiente tipo de dato de los árboles
-- binarios: 
--    data Arbol a = H a
--                 | N a (Arbol a) (Arbol a)
--      deriving (Show, Eq)
-- 
-- Definir la función 
--    existenciaYunicidad :: Arbol a -> (a -> Bool) -> Bool
-- tal que (existenciaYunicidad a p) se verifica si el árbol a tiene
-- exactamente  un elemento que cumple la propiedad p. Por ejemplo,
--   existenciaYunicidad (N 2 (N 4 (H 1)(H 2))(N 3 (H 1) (H 0))) (>3) == True
--   existenciaYunicidad (N 2 (N 4 (H 1)(H 2))(N 6 (H 1) (H 0))) (>3) == False
--   existenciaYunicidad (N 2 (N 4 (H 1)(H 2))(N 6 (H 1) (H 0))) (>7) == False
-- ---------------------------------------------------------------------

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
  deriving (Show, Eq)

existenciaYunicidad :: Arbol a -> (a -> Bool) -> Bool
existenciaYunicidad a p = length (cumplidores a p) == 1 

cumplidores :: Arbol a -> (a -> Bool) -> [a]
cumplidores (H x) p | p x       = [x]
                    | otherwise = []
cumplidores (N x i d) p | p x       = x : cumplidores i p ++ cumplidores d p
                        | otherwise = cumplidores i p ++ cumplidores d p

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función
--    unico :: Arbol a -> (a -> Bool) -> Maybe a
-- tal que (unico a p) es el único elemento del árbol a que cumple la
-- propiedad p, si existe un único elemento que la cumple y Nothing, en
-- caso contrario. Por ejemplo,
--    unico (N 2 (N 4 (H 1)(H 2))(N 3 (H 1) (H 0))) (>3)  == Just 4
--    unico (N 2 (N 4 (H 1)(H 2))(N 3 (H 1) (H 0))) (<1)  == Just 0
--    unico (N 2 (N 4 (H 1)(H 2))(N 3 (H 1) (H 0))) (<2)  == Nothing
--    unico (N 2 (N 4 (H 1)(H 2))(N 3 (H 1) (H 0))) (>4)  == Nothing
-- ---------------------------------------------------------------------

unico :: Arbol a -> (a -> Bool) -> Maybe a
unico a p | existenciaYunicidad a p = aux a p
          | otherwise               = Nothing
  where aux (H x) p | p x       = Just x
                    | otherwise = Nothing
        aux (N x i d) p | p x       = Just x
                        | isJust r  = r
                        | otherwise = aux d p
          where r = aux i p

-- --------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    intercambiaPrimero :: [a] -> [[a]]
-- tal que (intercambiaPrimero xs) es la lista de las listas obtenidas
-- intercambiando el primer elemento de xs por cada uno de los
-- demás. Por ejemplo, 
-- intercambiaPrimero [1,2,3,4]  == [[2,1,3,4],[2,3,1,4],[2,3,4,1]]
-- intercambiaPrimero [3]        == [[3]]
-- ---------------------------------------------------------------------

intercambiaPrimero :: [a] -> [[a]]
intercambiaPrimero []     = []
intercambiaPrimero [x]    = [[x]]
intercambiaPrimero (x:xs) = [coloca x n xs | n <- [1.. length xs]]

coloca :: a -> Int -> [a] -> [a]
coloca x n xs = ys ++ x : zs
  where (ys,zs) = splitAt n xs

-- --------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    sustituyeEnCola :: Eq a => Cola a -> a -> a -> Cola a
-- tal que (sustituyeEnCola c x y) es la cola obtenida sustiyendo x por
-- y en c. Por ejemplo,
--    λ> c = foldr inserta vacia [1,0,2,0,3,0,4,0,5,0]
--    λ> c
--    C [0,5,0,4,0,3,0,2,0,1]
--    λ> sustituyeEnCola c 0 8
--    C [8,5,8,4,8,3,8,2,8,1]
--    λ> sustituyeEnCola c 3 7
--    C [0,5,0,4,0,7,0,2,0,1]
--    λ> sustituyeEnCola c 6 7
--    C [0,5,0,4,0,3,0,2,0,1]
-- ---------------------------------------------------------------------

sustituyeEnCola :: Eq a => Cola a -> a -> a -> Cola a
sustituyeEnCola c x y = aux c vacia
  where aux c' r 
          | esVacia c' = r
          | pc' == x   = aux rc' (inserta y r)
          | otherwise  = aux rc' (inserta pc' r)
          where pc' = primero c'
                rc' = resto c'
