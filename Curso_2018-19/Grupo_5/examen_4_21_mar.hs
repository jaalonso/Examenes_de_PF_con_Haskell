-- Informática (1º del Grado en Matemáticas, Grupo 5)
-- 4º examen de evaluación continua (21 de marzo de 2019)
-- ------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- § Librerías                                                        --
-- ---------------------------------------------------------------------

import Data.Numbers.Primes
import Data.List
import Data.Array
import Data.Maybe
import I1M.Pila

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Dos números primos a y b se dirán relacionados si
-- tienen el mismo número de cifras y se diferencian en, exactamente,
-- una de ellas. Por ejemplo, 3169 y 3119 están relacionados.
-- 
-- Definir la lista
--    primosRelacionados :: [(Integer,Integer)]
-- cuyos elementos son los pares (a,b), con 2 <= b < a, tales que a y b
--    λ> take 17 primosRelacionados
--    [(3,2),(5,2),(5,3),(7,2),(7,3),(7,5),(13,11),(17,11),(17,13),
--     (19,11),(19,13),(19,17),(23,13),(29,19),(29,23),(31,11),(37,17)]
-- ---------------------------------------------------------------------

primosRelacionados :: [(Integer,Integer)]
primosRelacionados =
  [(a,b) | a <- primes
         , b <- takeWhile (<a) primes
         , relacionados a b]

relacionados :: Integer -> Integer -> Bool
relacionados a b =
  length as == length bs &&
  length [(x,y) | (x,y) <- zip as bs, x /= y] == 1
  where as = show a
        bs = show b

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Calcular en qué posición aparece el par (3169,3119) en
-- la lista infinta primosRelacionados.
-- ---------------------------------------------------------------------

-- El cálculo es
--    λ> length (takeWhile(/=(3169,3119)) primosRelacionados)
--    1475
--
-- Otra forma de calcularlo es
--    λ> fromJust (elemIndex (3169,3119) primosRelacionados)
--    1475

-- ---------------------------------------------------------------------
-- Ejercicio 2. Representamos los árboles binarios mediante el tipo de
-- dato 
--    data Arbol a = H a
--                 | N a (Arbol a) (Arbol a)
--      deriving Show
-- 
-- Define la función
--    ramaIgual :: Eq a => Arbol a -> Bool
-- tal que (ramaIgual x) se verifica si x tiene alguna rama con todos
-- los elementos de la rama iguales entre sí. Por ejemplo,
--    ramaIgual (N 2 (H 7) (N 2 (N 2 (H 0) (H 2)) (H 5)))  ==  True
--    ramaIgual (N 2 (H 7) (N 2 (N 3 (H 0) (H 2)) (H 5)))  == False
-- ---------------------------------------------------------------------

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
  deriving Show

ramaIgual :: Eq a => Arbol a -> Bool
ramaIgual  = any todosIguales . ramas

-- (ramas a) es la lista de las ramas del árbol a. Por ejemplo,
--    λ> ramas (N 2 (H 7) (N 2 (N 2 (H 0) (H 2)) (H 5)))
--    [[2,7],[2,2,2,0],[2,2,2,2],[2,2,5]]
ramas :: Arbol a -> [[a]]
ramas (H x)     = [[x]]
ramas (N x i d) = map (x:) (ramas i ++ ramas d) 

-- (todosIguales xs) se verifica si todos los elementos de xs son
-- iguales. Por ejemplo,
--    todosIguales [3,3,3,3]  ==  True
--    todosIguales [3,3,2,3]  ==  False
todosIguales :: Eq a => [a] -> Bool
todosIguales []     = True
todosIguales (x:xs) = all (==x) xs

-- ---------------------------------------------------------------------
-- Ejercicio 3. Representamos la matrices mediante el tipo de dato
--    type Matriz a = Array (Int,Int) a
-- 
-- Un elemento de una matriz se dirá un mínimo local si es menor
-- estricto que todos sus vecinos. Por ejemplo, la matriz
--    ( 2,5,1,0 )
--    ( 1,7,4,8 )
--    ( 3,3,2,5 )
-- tiene tres mínimos locales que se encuentran en las posiciones
-- (1,4), (2,1) y (3,3).
--
-- Definir la función
--    posicionesMinimos :: Ord a => Matriz a -> [(Int,Int)]
-- tal que (posicionesMinimos p) es la lista de las posiciones de la
-- matriz p en la que p tiene un mínimo local. Por ejemplo,
--    λ> posicionesMinimos (listArray ((1,1),(3,4)) [2,5,1,0,1,7,4,8,3,3,2,5])
--    [(1,4),(2,1),(3,3)]
-- ---------------------------------------------------------------------

type Matriz a = Array (Int,Int) a

posicionesMinimos :: Ord a => Matriz a -> [(Int,Int)]
posicionesMinimos p = 
    [(i,j) | (i,j) <- indices p,
             and [p!(i,j) < p!(a,b) | (a,b) <- vecinos (i,j)]] 
    where (_,(m,n)) = bounds p
          vecinos (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)],
                                   b <- [max 1 (j-1)..min n (j+1)],
                                   (a,b) /= (i,j)]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    mayorSeg :: Eq a => Pila a -> [a]
-- tal que (mayorSeg q) es el mayor segmento inicial de la pila q que no
-- contiene ningún elemento repetido. Por ejemplo, 
--    mayorSeg (foldr apila vacia [2,3,5,5,6])  ==  [2,3,5]
--    mayorSeg (foldr apila vacia [2,3,2,5,6])  ==  [2,3]
--    mayorSeg (foldr apila vacia [2,3,4,5,6])  ==  [2,3,4,5,6]
-- ---------------------------------------------------------------------

mayorSeg :: Eq a => Pila a -> [a]
mayorSeg = aux []
  where aux xs p
          | esVacia p  = reverse xs
          | elem cp xs = reverse xs
          | otherwise  = aux (cp:xs) dp
          where cp = cima p
                dp = desapila p

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Una sucesión tipo Fibonacci de parámetros una lista de
-- números naturales ns y un número natural k >= 2, se define como
-- sigue:
--   a) para i entre 1 y k, a(i) es el i-ésimo elemento de ns
--      (completando con ceros si fuese necesario);
--   b) para cada i > k, a(i) el la suma de los k anteriores términos de
--      la sucesión.
--
-- Definir la función
--    fibTipo :: [Integer] -> Integer -> Integer -> Integer
-- tal que (fibTipo ns k x) devuelve el x-ésimo termino de la sucesión
-- tipo Fibonacci de parámetros ns y k. Por ejemplo:
--    fibTipo [1,1] 2 10 == 55
--    fibTipo [1] 4 10   == 15
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

fibTipo1 :: [Integer] -> Integer -> Integer -> Integer
fibTipo1 ns k x
  | x <= k    = (ns ++ repeat 0) `genericIndex` (x-1)
  | otherwise = sum [fibTipo1 ns k y | y <- [x-k..x-1]]

-- 2ª definición
-- =============

fibTipo2 :: [Integer] -> Integer -> Integer -> Integer
fibTipo2 ns k x = fibTipoVector ns k x ! x 

fibTipoVector :: [Integer] -> Integer -> Integer -> Array Integer Integer
fibTipoVector ns k x = v
  where
    v = array (1,x) [(i,f i) | i <- [1..x]]
    f i | i <= k    = (ns ++ repeat 0) `genericIndex` (i-1)  
        | otherwise = sum [v!j | j <- [i-k..i-1]]

-- Comparación de eficiencia
-- =========================

--    λ> fibTipo1 [2,1] 3 26
--    1455549
--    (2.74 secs, 2,239,559,112 bytes)
--    λ> fibTipo2 [2,1] 3 26
--    1455549
--    (0.01 secs, 173,056 bytes)

-- En lo que sigue usaremos la 2ª definición
fibTipo :: [Integer] -> Integer -> Integer -> Integer
fibTipo = fibTipo2

-- ---------------------------------------------------------------------        
-- Ejercicio 5.2. Calcular el valor de (fibTipo [2,1] 3 99).
-- ---------------------------------------------------------------------

-- El cálculo es
--    λ> fibTipo [2,1] 3 99
--    30369399521566076939980432
--    (0.01 secs, 320,768 bytes)

