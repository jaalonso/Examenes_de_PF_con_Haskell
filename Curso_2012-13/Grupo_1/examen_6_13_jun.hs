-- Informática (1º del Grado en Matemáticas, Grupos 1 y 4)
-- 6º examen de evaluación continua (13 de junio de 2013)
-- ---------------------------------------------------------------------

import Data.Array
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Un número es alternado si cifras son par/impar
-- alternativamente. Por ejemplo, 123456 y 2785410 son alternados.
-- 
-- Definir la función
--    numerosAlternados :: [Integer] -> [Integer]
-- tal que (numerosAlternados xs) es la lista de los números alternados
-- de xs. Por ejemplo, 
--    ghci> numerosAlternados [21..50]
--    [21,23,25,27,29,30,32,34,36,38,41,43,45,47,49,50]
-- Usando la definición de numerosAlternados calcular la cantidad de
-- números alternados de 3 cifras.
-- ---------------------------------------------------------------------

-- 1ª definición (por comprension):
numerosAlternados :: [Integer] -> [Integer]
numerosAlternados xs = [n | n <- xs, esAlternado (cifras n)]

-- (esAlternado xs) se verifica si los elementos de xs son par/impar 
-- alternativamente. Por ejemplo,
--   esAlternado [1,2,3,4,5,6]    ==  True
--   esAlternado [2,7,8,5,4,1,0]  ==  True
esAlternado :: [Integer] -> Bool
esAlternado [_] = True
esAlternado xs = and [odd (x+y) | (x,y) <- zip xs (tail xs)]

-- (cifras x) es la lista de las cifras del n?mero x. Por ejemplo,
--    cifras 325  ==  [3,2,5]
cifras :: Integer -> [Integer]
cifras x = [read [d] | d <- show x]         

-- El cálculo es
--    ghci> length (numerosAlternados [100..999])
--    225

-- 2ª definición (por filtrado):  
numerosAlternados2 :: [Integer] -> [Integer]
numerosAlternados2 = filter (\n -> esAlternado (cifras n))

-- la definición anterior se puede simplificar:
numerosAlternados2' :: [Integer] -> [Integer]
numerosAlternados2' = filter (esAlternado . cifras)

-- 3ª definición (por recursion):
numerosAlternados3 :: [Integer] -> [Integer]
numerosAlternados3 [] = []
numerosAlternados3 (n:ns)
  | esAlternado (cifras n) = n : numerosAlternados3 ns
  | otherwise              = numerosAlternados3 ns

-- 4ª definición (por plegado):
numerosAlternados4 :: [Integer] -> [Integer]
numerosAlternados4 = foldr f []
  where f n ns | esAlternado (cifras n) = n : ns
               | otherwise              = ns
          
-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    borraSublista :: Eq a => [a] -> [a] -> [a]
-- tal que (borraSublista xs ys) es la lista que resulta de borrar la
-- primera ocurrencia de la sublista xs en ys. Por ejemplo, 
--    borraSublista [2,3] [1,4,2,3,4,5]      == [1,4,4,5] 
--    borraSublista [2,4] [1,4,2,3,4,5]      == [1,4,2,3,4,5]
--    borraSublista [2,3] [1,4,2,3,4,5,2,3]  ==  [1,4,4,5,2,3]
-- ---------------------------------------------------------------------

borraSublista :: Eq a => [a] -> [a] -> [a]
borraSublista [] ys = ys
borraSublista _ []  = []
borraSublista (x:xs) (y:ys) 
    | esPrefijo (x:xs) (y:ys) = drop (length xs) ys
    | otherwise               = y : borraSublista (x:xs) ys

-- (esPrefijo xs ys) se verifica si xs es un prefijo de ys. Por ejemplo,
--    esPrefijo [2,5] [2,5,7,9]  ==  True
--    esPrefijo [2,5] [2,7,5,9]  ==  False
--    esPrefijo [2,5] [7,2,5,9]  ==  False
esPrefijo :: Eq a => [a] -> [a] -> Bool
esPrefijo [] ys         = True
esPrefijo _ []          = False
esPrefijo (x:xs) (y:ys) = x==y && esPrefijo xs ys

-- .....................................................................
-- Ejercicio 3. Dos números enteros positivos a y b se dicen "parientes"
-- si la suma de sus divisores coincide. Por ejemplo, 16 y 25 son
-- parientes ya que sus divisores son [1,2,4,8,16] y [1,5,25],
-- respectivamente, y 1+2+4+8+16 = 1+5+25.
-- 
-- Definir la lista infinita
--     parientes :: [(Int,Int)]
-- que contiene los pares (a,b) de números parientes tales que 
-- 1 <= a < b. Por ejemplo, 
--    take 5 parientes  ==  [(6,11),(14,15),(10,17),(14,23),(15,23)]
-- ---------------------------------------------------------------------

parientes :: [(Int,Int)] 
parientes = [(a,b) | b <- [1..], a <- [1..b-1], sonParientes a b]

-- (sonParientes a b) se verifica si a y b son parientes. Por ejemplo,
--    sonParientes 16 25  ==  True
sonParientes :: Int -> Int -> Bool
sonParientes a b = sum (divisores a) == sum (divisores b)

-- (divisores a) es la lista de los divisores de a. Por ejemplo,
--    divisores 16  ==  [1,2,4,8,16]
--    divisores 25  ==  [1,5,25]
divisores :: Int -> [Int]
divisores a = [x | x <- [1..a], rem a x == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Los árboles binarios se pueden representar con el de
-- dato algebraico 
--    data Arbol a = H a   
--                 | N a (Arbol a) (Arbol a)  
-- Por ejemplo, los árboles 
--         9                9                
--        / \              / \    
--       /   \            /   \   
--      8     6          7     9 
--     / \   / \        / \   / \ 
--    3   2 4   5      3   2 9   7
-- se pueden representar por
--    ej1, ej2:: Arbol Int
--    ej1 = N 9 (N 8 (H 3) (H 2)) (N 6 (H 4) (H 5))
--    ej2 = N 9 (N 7 (H 3) (H 2)) (N 9 (H 9) (H 7))
-- 
-- Definir la función 
--    nodosInternos :: Arbol t -> [t]
-- tal que (nodosInternos a) es la lista de los nodos internos del 
-- árbol a. Por ejemplo,
--    nodosInternos ej1  ==  [9,8,6]
--    nodosInternos ej2  ==  [9,7,9]
-- .....................................................................

data Arbol a = H a   
             | N a (Arbol a) (Arbol a)  

ej1, ej2:: Arbol Int
ej1 = N 9 (N 8 (H 3) (H 2)) (N 6 (H 4) (H 5))
ej2 = N 9 (N 7 (H 3) (H 2)) (N 9 (H 9) (H 7))

nodosInternos (H _)     = []
nodosInternos (N x i d) = x : (nodosInternos i ++ nodosInternos d)

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función
--    ramaIguales :: Eq t => Arbol t -> Bool
-- tal que (ramaIguales a) se verifica si el árbol a contiene al menos
-- una rama tal que todos sus elementos son iguales. Por ejemplo,
--    ramaIguales ej1  ==  False
--    ramaIguales ej2  ==  True
-- ---------------------------------------------------------------------

-- 1ª definición:
ramaIguales :: Eq a => Arbol a -> Bool
ramaIguales (H _)     = True
ramaIguales (N x i d) = aux x i || aux x d
    where aux x (H y)     = x == y
          aux x (N y i d) = x == y && (aux x i || aux x d)

-- 2ª definición:
ramaIguales2 :: Eq a => Arbol a -> Bool
ramaIguales2 a = or [iguales xs | xs <- ramas a]

-- (ramas a) es la lista de las ramas del árbol a. Por ejemplo,
--    ramas ej1  ==  [[9,8,3],[9,8,2],[9,6,4],[9,6,5]]
--    ramas ej2  ==  [[9,7,3],[9,7,2],[9,9,9],[9,9,7]]
ramas :: Arbol a -> [[a]]
ramas (H x)     = [[x]]
ramas (N x i d) = map (x:) (ramas i) ++ map (x:) (ramas d)

-- (iguales xs) se verifica si todos los elementos de xs son
-- iguales. Por ejemplo,
--    iguales [5,5,5]  ==  True
--    iguales [5,2,5]  ==  False
iguales :: Eq a => [a] -> Bool
iguales (x:y:xs) = x == y && iguales (y:xs)
iguales _        = True

-- Otra definición de iguales, por comprensión, es
iguales2 :: Eq a => [a] -> Bool
iguales2 [] = True
iguales2 (x:xs) = and [x == y | y <- xs]

-- Otra, usando nub, es
iguales3 :: Eq a => [a] -> Bool
iguales3 xs = length (nub xs) <= 1

-- 3ª solución:
ramaIguales3 :: Eq a => Arbol a -> Bool
ramaIguales3  = any iguales . ramas

-- ------------------------------------------------------------------
-- Ejercicio 5. Las matrices enteras se pueden representar mediante
-- tablas con índices enteros: 
--    type Matriz = Array (Int,Int) Int
-- Por ejemplo, la matriz
--    |0 1 3|
--    |1 2 0|
--    |0 5 7|
-- se puede definir por
--    m :: Matriz
--    m = listArray ((1,1),(3,3)) [0,1,3, 1,2,0, 0,5,7]		
-- 
-- Definir la función
--    sumaVecinos :: Matriz -> Matriz
-- tal que (sumaVecinos p) es la matriz obtenida al escribir en la 
-- posicion (i,j) la suma de los todos vecinos del elemento que ocupa 
-- el lugar (i,j) en la matriz p. Por ejemplo,
--    ghci> sumaVecinos m
--    array ((1,1),(3,3)) [((1,1),4),((1,2), 6),((1,3), 3),
--                         ((2,1),8),((2,2),17),((2,3),18),
--                         ((3,1),8),((3,2),10),((3,3), 7)]
-- ------------------------------------------------------------------

type Matriz = Array (Int,Int) Int

m :: Matriz
m = listArray ((1,1),(3,3)) [0,1,3, 1,2,0, 0,5,7]		

sumaVecinos :: Matriz -> Matriz
sumaVecinos p =  
    array ((1,1),(m,n)) 
          [((i,j), f i j) | i <- [1..m], j <- [1..n]] 
    where (_,(m,n)) = bounds p
          f i j = sum [p!(i+a,j+b) | a <- [-1..1], b <- [-1..1], 
                                     a /= 0 || b /= 0,
                                     inRange (bounds p) (i+a,j+b)]


 
