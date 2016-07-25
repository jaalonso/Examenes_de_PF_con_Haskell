-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 4º examen de evaluación continua (18 de marzo de 2013)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por comprensión, la función 
--    filtraAplicaC :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaC f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo, 
--    filtraAplicaC (4+) (< 3) [1..7] == [5,6]
-- ---------------------------------------------------------------------

filtraAplicaC :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaC f p xs = [f x | x <- xs, p x]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, usando map y filter, la función 
--    filtraAplicaMF :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaMF f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo, 
--    filtraAplicaMF (4+) (< 3) [1..7] == [5,6]
-- ---------------------------------------------------------------------

filtraAplicaMF :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaMF f p  = (map f) . (filter p)

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir, por recursión, la función 
--    filtraAplicaR :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaR f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo, 
--    filtraAplicaR (4+) (< 3) [1..7] == [5,6]
-- ---------------------------------------------------------------------

filtraAplicaR :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaR _ _ [] = []
filtraAplicaR f p (x:xs) | p x       = f x : filtraAplicaR f p xs
                         | otherwise = filtraAplicaR f p xs
			   
-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Definir, por plegado, la función 
--    filtraAplicaP :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplicaP f p xs) es la lista obtenida aplicándole a los
-- elementos de xs que cumplen el predicado p la función f. Por ejemplo, 
--    filtraAplicaP (4+) (< 3) [1..7] == [5,6]
-- ---------------------------------------------------------------------

filtraAplicaP :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaP f p = foldr g []
    where g x y | p x       = f x : y
                | otherwise = y

-- Se puede usar lambda en lugar de la función auxiliar
filtraAplicaP' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filtraAplicaP' f p = foldr (\x y -> if p x then f x : y else y) []

-- ---------------------------------------------------------------------
-- Ejercicio 2. Los árboles binarios se pueden representar con el de
-- tipo de dato algebraico 
--    data Arbol a = H a   
--                 | N a (Arbol a) (Arbol a)  
-- Por ejemplo, los árboles 
--         9                9                
--        / \              / \    
--       /   \            /   \   
--      8     8          4     8 
--     / \   / \        / \   / \ 
--    3   2 4   5      3   2 5   7
-- se pueden representar por
--    ej1, ej2:: Arbol Int
--    ej1 = N 9 (N 8 (H 3) (H 2)) (N 8 (H 4) (H 5))
--    ej2 = N 9 (N 4 (H 3) (H 2)) (N 8 (H 5) (H 7))
-- 
-- Se considera la definición de tipo de dato:
--
-- Definir el predicado
--    contenido :: Eq a => Arbol a -> Arbol a -> Bool
-- tal que (contenido a1 a2) es verdadero si todos los elementos que
-- aparecen en el árbol a1 también aparecen en el árbol a2. Por ejemplo, 
--    contenido ej1 ej2  ==  True
--    contenido ej2 ej1  ==  False
-- ---------------------------------------------------------------------

data Arbol a = H a 
             | N a (Arbol a) (Arbol a)

ej1, ej2:: Arbol Int
ej1 = N 9 (N 8 (H 3) (H 2)) (N 8 (H 4) (H 5))
ej2 = N 9 (N 4 (H 3) (H 2)) (N 8 (H 5) (H 7))

contenido :: Eq a => Arbol a -> Arbol a -> Bool
contenido (H x) a     = pertenece x a
contenido (N x i d) a = pertenece x a && contenido i a && contenido d a 

-- (pertenece x a) se verifica si x pertenece al árbol a. Por ejemplo,
--    pertenece 8 ej1  ==  True
--    pertenece 7 ej1  ==  False
pertenece x (H y)     = x == y
pertenece x (N y i d) = x == y || pertenece x i || pertenece x d

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función
--    esCubo :: Int -> Bool 
-- tal que (esCubo x) se verifica si el entero x es un cubo
-- perfecto. Por ejemplo,
--    esCubo 27  ==  True
--    esCubo 50  ==  False
-- ---------------------------------------------------------------------

-- 1ª definición:
esCubo :: Int -> Bool 
esCubo x = y^3 == x
    where y = ceiling ((fromIntegral x)**(1/3))

-- 2ª definición:
esCubo2 :: Int -> Bool 
esCubo2 x = elem x (takeWhile (<=x) [i^3 | i <- [1..]])

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la lista (infinita) 
--    soluciones :: [Int] 
-- cuyos elementos son los números naturales que pueden escribirse como
-- suma de dos cubos perfectos, al menos, de dos maneras distintas. Por
-- ejemplo, 
--    take 3 soluciones  ==  [1729,4104,13832]
-- ---------------------------------------------------------------------

soluciones :: [Int]
soluciones = [x | x <- [1..], length (sumas x) >= 2]

-- (sumas x) es la lista de pares de cubos cuya suma es x. Por ejemplo, 
--    sumas 1729  ==  [(1,1728),(729,1000)]
sumas :: Int -> [(Int,Int)]
sumas x = [(a^3,x-a^3) | a <- [1..cota], a^3 <= x-a^3,  esCubo (x-a^3)]
    where cota = floor ((fromIntegral x)**(1/3))

-- La definición anterior se pued simplificar:
sumas2 :: Int -> [(Int,Int)]
sumas2 x = [(a^3,x-a^3) | a <- [1..cota],  esCubo (x-a^3)]
    where cota = floor ((fromIntegral x / 2)**(1/3))

-- ---------------------------------------------------------------------
-- Ejercicio 4. Disponemos de una mochila que tiene una capacidad
-- limitada de c kilos. Nos encontramos con una serie de objetos cada
-- uno con un valor v y un peso p. El problema de la mochila consiste en
-- escoger subconjuntos de objetos tal que la suma de sus valores sea
-- máxima y la suma de sus pesos no rebase la capacidad de la mochila. 
-- 
-- Se definen los tipos sinónimos:
--    type Peso a   = [(a,Int)]
--    type Valor a  = [(a,Int)]
-- para asignar a cada objeto, respectivamente, su peso o valor. 
-- 
-- Definir la función:
--    mochila :: Eq a => [a] -> Int -> Peso a -> Valor a -> [[a]]
-- tal que (mochila xs c ps vs) devuelve todos los subconjuntos de xs
-- tal que la suma de sus valores sea máxima y la suma de sus pesos sea
-- menor o igua que cota c. Por ejemplo, 
--    ghci> :{
--    *Main| mochila ["linterna", "oro", "bocadillo", "apuntes"] 10
--    *Main|         [("oro",7),("bocadillo",1),("linterna",2),("apuntes",5)]
--    *Main|         [("apuntes",8),("linterna",1),("oro",100),("bocadillo",10)]
--    *Main| :}
-- ---------------------------------------------------------------------

type Peso a  = [(a,Int)]
type Valor a = [(a,Int)]

mochila :: Eq a => [a] -> Int -> Peso a -> Valor a -> [[a]]
mochila xs c ps vs = [ys | ys <- rellenos, pesoTotal ys vs == maximo]
    where rellenos = posibles xs c ps
	  maximo   = maximum [pesoTotal ys vs | ys <- rellenos]

-- (posibles xs c ps) es la lista de objetos de xs cuyo peso es menor o
-- igual que c y sus peso están indicada por ps. Por ejemplo,
--    ghci> posibles ["a","b","c"] 9 [("a",3),("b",7),("c",2)]
--    [[],["c"],["b"],["b","c"],["a"],["a","c"]]
posibles :: Eq a => [a] -> Int -> Peso a -> [[a]]
posibles xs c ps = [ys | ys <- subconjuntos xs, pesoTotal ys ps <= c] 

-- (subconjuntos xs) es la lista de los subconjuntos de xs. Por ejemplo, 
--    subconjuntos [2,5,3]  ==  [[],[3],[5],[5,3],[2],[2,3],[2,5],[2,5,3]]
subconjuntos :: [a] -> [[a]]
subconjuntos []     = [[]]
subconjuntos (x:xs) = subconjuntos xs ++ [x:ys | ys <- subconjuntos xs]

-- (pesoTotal xs ps) es el peso de todos los objetos de xs tales que los
-- pesos de cada uno están indicado por ps. Por ejemplo,
--    pesoTotal ["a","b","c"] [("a",3),("b",7),("c",2)]  ==  12
pesoTotal :: Eq a => [a] -> Peso a -> Int
pesoTotal xs ps = sum [peso x ps | x <- xs]

-- (peso x ps) es el peso de x en la lista de pesos ps. Por ejemplo,
--    peso "b" [("a",3),("b",7),("c",2)]  ==  7
peso :: Eq a => a -> [(a,b)]  -> b
peso x ps = head [b | (a,b) <- ps, a ==x]

