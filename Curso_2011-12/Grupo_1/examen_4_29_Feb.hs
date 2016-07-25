-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 4º examen de evaluación continua (29 de febrero de 2012)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. [2.5 puntos] En el enunciado de uno de los problemas de
-- las Olimpiadas matemáticas de Brasil se define el primitivo de un
-- número como sigue:
--    Dado un número natural N, multiplicamos todos sus dígitos,
--    repetimos este procedimiento hasta que quede un solo dígito al
--    cual llamamos primitivo de N. Por ejemplo para 327: 3x2x7 = 42 y 
--    4x2 = 8. Por lo tanto, el primitivo de 327 es 8.
--
-- Definir la función 
--    primitivo :: Integer -> Integer
-- tal que (primitivo n) es el primitivo de n. Por ejemplo.
--    primitivo 327  ==  8
-- ---------------------------------------------------------------------

primitivo :: Integer -> Integer
primitivo n | n < 10    = n
            | otherwise = primitivo (producto n)

-- (producto n) es el producto de las cifras de n. Por ejemplo,
--    producto 327  ==  42
producto :: Integer -> Integer
producto = product . cifras

-- (cifras n) es la lista de las cifras de n. Por ejemplo,
--    cifras 327  ==  [3,2,7]
cifras :: Integer -> [Integer]
cifras n = [read [y] | y <- show n]

-- ---------------------------------------------------------------------
-- Ejercicio 2. [2.5 puntos] Definir la función
--    sumas :: Int -> [Int] -> [Int]
-- tal que (sumas n xs) es la lista de los números que se pueden obtener
-- como suma de n, o menos, elementos de xs. Por ejemplo,
--    sumas 0 [2,5]    ==  [0]
--    sumas 1 [2,5]    ==  [2,5,0]
--    sumas 2 [2,5]    ==  [4,7,2,10,5,0]
--    sumas 3 [2,5]    ==  [6,9,4,12,7,2,15,10,5,0]
--    sumas 2 [2,3,5]  ==  [4,5,7,2,6,8,3,10,5,0]
-- ---------------------------------------------------------------------

sumas :: Int -> [Int] -> [Int]
sumas 0 _  = [0]
sumas _ [] = [0]  
sumas n (x:xs) = [x+y | y <- sumas (n-1) (x:xs)] ++ sumas n xs 

-- ---------------------------------------------------------------------
-- Ejercicio 3. [2.5 puntos] Los árboles binarios se pueden representar
-- mediante el siguiente tipo de datos
--    data Arbol = H  
--               | N Int Arbol Arbol
-- Por ejemplo, el árbol
--            9
--           / \ 
--          /   \ 
--         3     7   
--        / \   / \
--       /   \ H   H
--      2     4   
--     / \   / \  
--    H   H H   H
-- se representa por
--    N 9 (N 3 (N 2 H H) (N 4 H H)) (N 7 H H)
-- Definir la función
--    ramaIzquierda :: Arbol -> [Int]
-- tal que (ramaIzquierda a) es la lista de los valores de los nodos de
-- la rama izquierda del árbol a. Por ejemplo,
--    ghci> ramaIzquierda (N 9 (N 3 (N 2 H H) (N 4 H H)) (N 7 H H))
--    [9,3,2]
-- ---------------------------------------------------------------------

data Arbol = H  
           | N Int Arbol Arbol

ramaIzquierda :: Arbol -> [Int]
ramaIzquierda H         = []
ramaIzquierda (N x i d) = x : ramaIzquierda i

-- ---------------------------------------------------------------------
-- Ejercicio 4. [2.5 puntos] Un primo permutable es un número primo tal
-- que todos los números obtenidos permutando sus cifras son primos. Por
-- ejemplo, 337 es un primo permutable ya que 337, 373 y 733 son
-- primos. 
-- 
-- Definir la función  
--    primoPermutable :: Integer -> Bool
-- tal que (primoPermutable x) se verifica si x es un primo
-- permutable. Por ejemplo, 
--    primoPermutable 17  ==  True
--    primoPermutable 19  ==  False
-- ---------------------------------------------------------------------

primoPermutable :: Integer -> Bool
primoPermutable x = and [primo y | y <- permutacionesN x]

-- (permutacionesN x) es la lista de los números obtenidos permutando
-- las cifras de x. Por ejemplo,
permutacionesN :: Integer -> [Integer]
permutacionesN x = [read ys | ys <- permutaciones (show x)]

-- (intercala x ys) es la lista de las listas obtenidas intercalando x
-- entre los elementos de ys. Por ejemplo, 
--    intercala 1 [2,3]  ==  [[1,2,3],[2,1,3],[2,3,1]]
intercala :: a -> [a] -> [[a]]
intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys) : [y:zs | zs <- intercala x ys]

-- (permutaciones xs) es la lista de las permutaciones de la lista
-- xs. Por ejemplo, 
--    permutaciones "bc"   ==  ["bc","cb"]
--    permutaciones "abc"  ==  ["abc","bac","bca","acb","cab","cba"]
permutaciones :: [a] -> [[a]]
permutaciones []     = [[]]
permutaciones (x:xs) = 
    concat [intercala x ys | ys <- permutaciones xs]

-- (primo x) se verifica si x es primo.
primo :: Integer -> Bool
primo x = x == head (dropWhile (<x) primos)

-- primos es la lista de los números primos. 
primos :: [Integer ]
primos = criba [2..]
    where criba :: [Integer] -> [Integer]
          criba (p:xs) = p : criba [x | x <- xs, x `mod` p /= 0]
