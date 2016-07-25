-- Informática (1º Grado Matemáticas y doble Grado Matemáticas y Física)
-- 5º examen de evaluación continua (21 de mayo de 2014)
-- =====================================================================

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función
--    subcadena :: String -> String -> Int
-- tal que (subcadena xs ys) es el número de veces que aparece la cadena
-- xs dentro de la cadena ys. Por ejemplo, 
--    subcadena "abc" "abc1ab1c1abc1" == 2
--    subcadena "abc" "a1b1bc"        == 0
-- ---------------------------------------------------------------------

subcadena :: String -> String -> Int
subcadena _ [] = 0
subcadena xs cs@(y:ys) 
    | xs == take n cs = 1 + subcadena xs (drop n cs)
    | otherwise       = subcadena xs ys
    where n = length xs

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función 
--    ocurrencias :: Int -> (Integer -> Integer) -> Integer -> Int
-- tal que (ocurrencias n f x) es el número de veces que aparecen las
-- cifras de n como subcadena de las cifras del número f(x). Por
-- ejemplo, 
--    ocurrencias 0 (1+) 399          == 2
--    ocurrencias 837 (2^) 1000       == 3
-- ---------------------------------------------------------------------

ocurrencias :: Int -> (Integer -> Integer) -> Integer -> Int
ocurrencias n f x = subcadena (show n) (show (f x))

-- ----------------------------------------------------------------------
-- Ejercicio 2.1. Representamos árboles binarios mediante el tipo de
-- dato 
--    data Arbol a = Hoja a | Nodo a (Arbol a) (Arbol a)
-- Por ejemplo, los árboles
--        5           5   
--       / \         / \  
--      5   3       5   3 
--     / \         / \    
--    2   5       2   5   
-- se representan por
--    arbol1, arbol2 :: Arbol Int
--    arbol1 = (Nodo 5 (Nodo 5 (Hoja 2) (Hoja 5)) (Hoja 3))
--    arbol2 = (Nodo 5 (Nodo 5 (Hoja 2) (Hoja 1)) (Hoja 3))
-- 
-- Definir el predicado
--    ramaIgual :: Eq a => Arbol a -> Bool
-- tal que (ramaIgual t) se verifica si el árbol t contiene, al menos,
-- una rama con todos sus elementos iguales. Por ejemplo,
--    ramaIgual arbol1 == True
--    ramaIgual arbol2 == False
-- ---------------------------------------------------------------------

data Arbol a = Hoja a | Nodo a (Arbol a) (Arbol a)

arbol1, arbol2 :: Arbol Int
arbol1 = (Nodo 5 (Nodo 5 (Hoja 2) (Hoja 5)) (Hoja 3))
arbol2 = (Nodo 5 (Nodo 5 (Hoja 2) (Hoja 1)) (Hoja 3))

ramaIgual :: Eq a => Arbol a -> Bool
ramaIgual t = or [todosI xs | xs <- ramas t]

-- (ramas t) es la lista de las ramas del árbol t. Por ejemplo,
--    ramas arbol1  ==  [[5,5,2],[5,5,5],[5,3]]
--    ramas arbol2  ==  [[5,5,2],[5,5,1],[5,3]]
ramas (Hoja x)     = [[x]]
ramas (Nodo x i d) = map (x:) (ramas i ++ ramas d)

-- (todosI xs) se verifica si todos los elementos de xs son iguales. Por
-- ejemplo, 
--    todosI [6,6,6]  ==  True
--    todosI [6,9,6]  ==  False
todosI :: Eq a => [a] -> Bool
todosI (x:y:xs) = x == y && todosI (y:xs)
todosI _        = True

-- ---------------------------------------------------------------------
-- Ejercicio 2.2 Definir el predicado
--    ramasDis :: Eq a => Arbol a -> Bool
-- tal que (ramasDis t) se verifica si todas las ramas del árbol t
-- contienen, al menos, dos elementos distintos. Por ejemplo:
--    ramasDis arbol2 == True
--    ramasDis arbol1  ==  False
-- ---------------------------------------------------------------------

ramasDis :: Eq a => Arbol a -> Bool
ramasDis = not . ramaIgual

-- ----------------------------------------------------------------------      
-- Ejercicio 3. Representamos polinomios en una variable mediante el
-- tipo de dato 
--    data Pol a = PolCero | Cons Int a (Pol a) deriving Show
-- Por ejemplo, el polinomio 3x^5 - x^3 + 7x^2 se representa por
--    pol :: Pol Int
--    pol = Cons 5 3 (Cons 3 (-1) (Cons 2 7 PolCero))
-- 
-- Definir la función
--    derivadaN :: Num a => Int -> Pol a -> Pol a
-- tal que (derivadaN n p) es la derivada n-ésima del polinomio p. Por
-- ejemplo,
--    derivadaN 1 pol  ==  Cons 4 15 (Cons 2 (-3) (Cons 1 14 PolCero))
--    derivadaN 2 pol  ==  Cons 3 60 (Cons 1 (-6) (Cons 0 14 PolCero))
--    derivadaN 3 pol  ==  Cons 2 180 (Cons 0 (-6) PolCero)
--    derivadaN 1 (Cons 5 3.2 PolCero)  ==  Cons 4 16.0 PolCero
-- ---------------------------------------------------------------------

data Pol a = PolCero | Cons Int a (Pol a) deriving Show

pol :: Pol Int
pol = Cons 5 3 (Cons 3 (-1) (Cons 2 7 PolCero))

derivadaN :: Num a => Int -> Pol a -> Pol a
derivadaN n p = (iterate derivada p)!!n

derivada :: Num a => Pol a -> Pol a
derivada PolCero      = PolCero
derivada (Cons 0 x p) = PolCero
derivada (Cons n x p) = Cons (n-1) (x * fromIntegral n) (derivada p)

-- ----------------------------------------------------------------------
-- Ejercicio 4. El algoritmo de Jacobi se utiliza para calcular el
-- gradiente de temperatura en una malla de cuerpos dispuestos en dos
-- dimensiones. Se emplea para ello una matriz con el siguiente contenido:
--    a) Se define una frontera, que son los elementos de la primera fila,
--       primera columna, última fila y última columna. Estos elementos
--       indican la temperatura exterior, y su valor es siempre constante.
--    b) Los elementos del interior indican la temperatura de cada
--       cuerpo.
-- En cada iteración del algoritmo la matriz p se transforma en otra q,
-- de la misma dimensión, cuyos elementos son: 
--    a) Elementos de la frontera: 
--          q(i,j)=p(i,j).
--    b) Elementos del interior:
--          q(i,j)=0.2*(p(i,j)+p(i+1,j)+p(i-1,j)+p(i,j+1)+p(i,j-1))
-- Por ejemplo, la transformada de la matriz de la izquierda es la de la
-- derecha  
--    |2, 2, 2, 2, 2|          |2.0, 2.0, 2.0, 2.0, 2.0|
--    |2, 0, 0, 0, 2|          |2.0, 0.8, 0.4, 0.8, 2.0|
--    |2, 0, 0, 0, 2|          |2.0, 0.4, 0.0, 0.4, 2.0|
--    |2, 0, 0, 0, 2|          |2.0, 0.8, 0.4, 0.8, 2.0|
--    |2, 2, 2, 2, 2|          |2.0, 2.0, 2.0, 2.0, 2.0|
-- 
-- Representaremos la matriz con el tipo de dato:
--    type Matriz = Array (Int,Int) Float
-- La dos matrices anteriores se representan por
--    matriz1, matriz2 :: Matriz
--    matriz1 = listArray ((1,1),(5,5)) ([2, 2, 2, 2, 2, 
--                                        2, 0, 0, 0, 2, 
--                                        2, 0, 0, 0, 2, 
--                                        2, 0, 0, 0, 2, 
--                                        2, 2, 2, 2, 2])       
--    matriz2 = listArray ((1,1),(5,5)) ([2.0, 2.0, 2.0, 2.0, 2.0, 
--                                        2.0, 0.8, 0.4, 0.8, 2.0, 
--                                        2.0, 0.4, 0.0, 0.4, 2.0, 
--                                        2.0, 0.8, 0.4, 0.8, 2.0, 
--                                        2.0, 2.0, 2.0, 2.0, 2.0])
-- 
-- Definir la función 
--    iteracion_jacobi:: Matriz -> Matriz
-- tal que (iteracion_jacobi p) es la matriz obtenida aplicándole una
-- transformación de Jacobi a la matriz p. Por ejemplo,
--    iteracion_jacobi matriz1  ==  matriz2
-- ---------------------------------------------------------------------

type Matriz = Array (Int,Int) Float

matriz1 :: Matriz
matriz1 = listArray ((1,1),(5,5)) ([2, 2, 2, 2, 2, 
                                    2, 0, 0, 0, 2, 
                                    2, 0, 0, 0, 2, 
                                    2, 0, 0, 0, 2, 
                                    2, 2, 2, 2, 2])       

matriz2 :: Matriz                                 
matriz2 = listArray ((1,1),(5,5)) ([2.0, 2.0, 2.0, 2.0, 2.0, 
                                    2.0, 0.8, 0.4, 0.8, 2.0, 
                                    2.0, 0.4, 0.0, 0.4, 2.0, 
                                    2.0, 0.8, 0.4, 0.8, 2.0, 
                                    2.0, 2.0, 2.0, 2.0, 2.0])

-- 1ª definición:
iteracion_jacobi :: Matriz -> Matriz
iteracion_jacobi p = array ((1,1),(n,m)) [((i,j), f i j) | i <- [1..n], j<-[1..m]]
    where (_,(n,m)) = bounds p
          f i j | frontera (i,j) = p!(i,j)
                | otherwise      = 0.2*(p!(i,j)+p!(i+1,j)+p!(i-1,j)+p!(i,j+1)+p!(i,j-1))
          frontera (i,j) = i == 1 || i == n || j == 1 || j == m

-- 2ª definición:
iteracion_jacobi2 :: Matriz -> Matriz              
iteracion_jacobi2 p = 
    array ((1,1),(n,m)) 
          ([((i,j), 0.2*(p!(i,j)+p!(i+1,j)+p!(i-1,j)+p!(i,j+1)+p!(i,j-1))) | 
            i <- [2..n-1], j <- [2..m-1]] ++
           [((i,j),p!(i,j)) | i <- [1,n],  j <- [1..m]]++ 
           [((i,j),p!(i,j)) | i <- [1..n], j <- [1,m]])  
    where (_,(n,m)) = bounds p

