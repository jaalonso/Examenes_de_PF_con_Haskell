-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 6º examen de evaluación continua (13 de junio de 2013)
-- ---------------------------------------------------------------------

import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1. Un número es creciente si cada una de sus cifras es
-- mayor o igual que su anterior. 
-- 
-- Definir la función 
--    numerosCrecientes :: [Integer] -> [Integer]
-- tal que (numerosCrecientes xs) es la lista de los números crecientes
-- de xs. Por ejemplo,
--    ghci> numerosCrecientes [21..50]
--    [22,23,24,25,26,27,28,29,33,34,35,36,37,38,39,44,45,46,47,48,49]
-- Usando la definición de numerosCrecientes calcular la cantidad de
-- números crecientes de 3 cifras.
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
numerosCrecientes :: [Integer] -> [Integer]
numerosCrecientes xs = [n | n <- xs, esCreciente (cifras n)]

-- (esCreciente xs) se verifica si xs es una sucesión cerciente. Por
-- ejemplo, 
--    esCreciente [3,5,5,12]  ==  True
--    esCreciente [3,5,4,12]  ==  False
esCreciente :: Ord a => [a] -> Bool
esCreciente (x:y:zs) = x <= y && esCreciente (y:zs) 
esCreciente _        = True

-- (cifras x) es la lista de las cifras del número x. Por ejemplo,
--    cifras 325  ==  [3,2,5]
cifras :: Integer -> [Integer]
cifras x = [read [d] | d <- show x]         

-- El cálculo es
--    ghci> length (numerosCrecientes [100..999])
--    165

-- 2ª definición (por filtrado):  
numerosCrecientes2 :: [Integer] -> [Integer]
numerosCrecientes2 = filter (\n -> esCreciente (cifras n))

-- 3ª definición (por recursión):
numerosCrecientes3 :: [Integer] -> [Integer]
numerosCrecientes3 [] = []
numerosCrecientes3 (n:ns)
  | esCreciente (cifras n) = n : numerosCrecientes3 ns
  | otherwise              = numerosCrecientes3 ns

-- 4ª definición (por plegado):
numerosCrecientes4 :: [Integer] -> [Integer]
numerosCrecientes4 = foldr f []
  where f n ns | esCreciente (cifras n) = n : ns
               | otherwise              = ns
          
-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    sublistasIguales :: Eq a => [a] -> [[a]]
-- tal que (sublistasIguales xs) es la listas de elementos consecutivos
-- de xs que son iguales. Por ejemplo,
--    ghci> sublistasIguales [1,5,5,10,7,7,7,2,3,7] 
--    [[1],[5,5],[10],[7,7,7],[2],[3],[7]]
-- ---------------------------------------------------------------------

-- 1ª definición:
sublistasIguales :: Eq a => [a] -> [[a]]
sublistasIguales [] = []
sublistasIguales (x:xs) =
  (x : takeWhile (==x) xs) : sublistasIguales (dropWhile (==x) xs)

-- 2ª definición:
sublistasIguales2 :: Eq a => [a] -> [[a]]
sublistasIguales2 []     = []
sublistasIguales2 [x]    = [[x]]
sublistasIguales2 (x:y:zs)
  | x == u    = (x:u:us):vss
  | otherwise = [x]:((u:us):vss)           
  where ((u:us):vss) = sublistasIguales2 (y:zs)

-- ----------------------------------------------------------------------
-- Ejercicio 3. Los árboles binarios se pueden representar con el de
-- dato algebraico 
--    data Arbol a = H
--                 | N a (Arbol a) (Arbol a)
--                 deriving Show
-- Por ejemplo, los árboles 
--         9                9                
--        / \              / \    
--       /   \            /   \   
--      8     6          8     6  
--     / \   / \        / \   / \ 
--    3   2 4   5      3   2 4   7
-- se pueden representar por
--    ej1, ej2:: Arbol Int
--    ej1 = N 9 (N 8 (N 3 H H) (N 2 H H)) (N 6 (N 4 H H) (N 5 H H))
--    ej2 = N 9 (N 8 (N 3 H H) (N 2 H H)) (N 6 (N 4 H H) (N 7 H H))
-- Un árbol binario ordenado es un árbol binario (ABO) en el que los
-- valores de cada nodo es mayor o igual que los valores de sus
-- hijos. Por ejemplo, ej1 es un ABO, pero ej2 no lo es. 
-- 
-- Definir la función esABO
--    esABO :: Ord t => Arbol t -> Bool
-- tal que (esABO a) se verifica si a es un árbol binario ordenado. Por
-- ejemplo. 
--    esABO ej1 == True
--    esABO ej2 == False    
-- ---------------------------------------------------------------------

data Arbol a = H
             | N a (Arbol a) (Arbol a)
             deriving Show

ej1, ej2 :: Arbol Int
ej1 = N 9 (N 8 (N 3 H H) (N 2 H H))
          (N 6 (N 4 H H) (N 5 H H))

ej2 = N 9 (N 8 (N 3 H H) (N 2 H H))
          (N 6 (N 4 H H) (N 7 H H))

-- 1ª definición
esABO :: Ord a => Arbol a -> Bool
esABO H                       = True
esABO (N x H H)               = True
esABO (N x m1@(N x1 a1 b1) H) = x >= x1 && esABO m1
esABO (N x H m2@(N x2 a2 b2)) = x >= x2 && esABO m2
esABO (N x m1@(N x1 a1 b1) m2@(N x2 a2 b2)) = 
      x >= x1 && esABO m1 && x >= x2 && esABO m2

-- 2ª definición
esABO2 :: Ord a => Arbol a -> Bool
esABO2 H         = True
esABO2 (N x i d) = mayor x i && mayor x d && esABO2 i && esABO2 d 
       where  mayor x H         = True
              mayor x (N y _ _) = x >= y
    
-- ----------------------------------------------------------------------
-- Ejercicio 4. Definir la función 
--    paresEspecialesDePrimos :: Integer -> [(Integer,Integer)]
-- tal que (paresEspecialesDePrimos n) es la lista de los pares de
-- primos (p,q) tales que p < q y q-p es divisible por n. Por ejemplo,
--    ghci> take 9 (paresEspecialesDePrimos 2)
--    [(3,5),(3,7),(5,7),(3,11),(5,11),(7,11),(3,13),(5,13),(7,13)]
--    ghci> take 9 (paresEspecialesDePrimos 3)
--    [(2,5),(2,11),(5,11),(7,13),(2,17),(5,17),(11,17),(7,19),(13,19)]
-- ---------------------------------------------------------------------

paresEspecialesDePrimos :: Integer -> [(Integer,Integer)]
paresEspecialesDePrimos n =
  [(p,q) | (p,q) <- paresPrimos, rem (q-p) n == 0]

-- paresPrimos es la lista de los pares de primos (p,q) tales que p < q. 
-- Por ejemplo,
--    ghci> take 9 paresPrimos
--    [(2,3),(2,5),(3,5),(2,7),(3,7),(5,7),(2,11),(3,11),(5,11)]
paresPrimos :: [(Integer,Integer)]
paresPrimos = [(p,q) | q <- primos, p <- takeWhile (<q) primos]

-- primos es la lista de primos. Por ejemplo,
--    take 9 primos  ==  [2,3,5,7,11,13,17,19,23]
primos :: [Integer]
primos = 2 : [n | n <- [3,5..], esPrimo n]

-- (esPrimo n) se verifica si n es primo. Por ejemplo,
--    esPrimo 7  ==  True
--    esPrimo 9  ==  False
esPrimo :: Integer -> Bool
esPrimo n = [x | x <- [1..n], rem n x == 0] == [1,n]

-- ----------------------------------------------------------------------
-- Ejercicio 5. Las matrices enteras se pueden representar mediante
-- tablas con índices enteros:
--    type Matriz = Array (Int,Int) Int
-- 
-- Definir la función
--    ampliaColumnas :: Matriz -> Matriz -> Matriz
-- tal que (ampliaColumnas p q) es la matriz construida añadiendo las
-- columnas de la matriz q a continuación de las de p (se supone que
-- tienen el mismo número de filas). Por ejemplo, si p y q representa
-- las dos primeras matrices, entonces (ampliaColumnas p q) es la
-- tercera  
--    |0 1|    |4 5 6|    |0 1 4 5 6| 
--    |2 3|    |7 8 9|    |2 3 7 8 9|
-- En Haskell,
--    ghci> :{
--    *Main| ampliaColumnas (listArray ((1,1),(2,2)) [0..3]) 
--    *Main|                (listArray ((1,1),(2,3)) [4..9])
--    *Main| :}
--    array ((1,1),(2,5)) 
--          [((1,1),0),((1,2),1),((1,3),4),((1,4),5),((1,5),6),
--           ((2,1),2),((2,2),3),((2,3),7),((2,4),8),((2,5),9)]
-- --------------------------------------------------------------------- 

type Matriz = Array (Int,Int) Int

ampliaColumnas :: Matriz -> Matriz -> Matriz
ampliaColumnas p1 p2 =
  array ((1,1),(m,n1+n2)) [((i,j), f i j) | i <- [1..m], j <- [1..n1+n2]]
    where ((_,_),(m,n1)) = bounds p1
          ((_,_),(_,n2)) = bounds p2
          f i j | j <= n1   = p1!(i,j)
                | otherwise = p2!(i,j-n1) 
