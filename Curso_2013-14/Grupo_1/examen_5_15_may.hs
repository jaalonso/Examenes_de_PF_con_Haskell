-- Informática (1º del Grado en Matemáticas)
-- 5º examen de evaluación continua (15 de mayo de 2014)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--     separados :: Eq a => a -> a -> [a] -> Bool
-- tal que (separados x y xs) se verifica si a y b no son elementos
-- consecutivos en xs. Por ejemplo,
--    separados 4 6 [1..20]      == True
--    separados 4 6 [2,4..20]    == False
--    separados 'd' 'a' "damas"  == False
--    separados 'd' 'a' "ademas" == False
--    separados 'd' 'm' "ademas" == True
-- ---------------------------------------------------------------------

-- 1ª solución
separados :: Eq a => a -> a -> [a] -> Bool
separados a b zs = and [(x,y) /= (a,b) && (x,y) /= (b,a) |
                        (x,y) <- zip zs (tail zs)]

-- 2ª solución
separados2 :: Eq a => a -> a -> [a] -> Bool
separados2 a b zs = 
    (a,b) `notElem` consecutivos && (b,a) `notElem` consecutivos
    where consecutivos = zip zs (tail zs)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función 
--     subcadenasNoVacias :: [a] -> [[a]]
-- tal que (subcadenasNoVacias xs) es la lista de las subcadenas no
-- nulas de xs. Por ejemplo, 
--     ghci> subcadenasNoVacias "Hola"
--     ["H","Ho","Hol","Hola","o","ol","ola","l","la","a"]
-- ---------------------------------------------------------------------

-- 1ª solución
subcadenasNoVacias :: [a] -> [[a]]
subcadenasNoVacias []     = []
subcadenasNoVacias (x:xs) = tail (inits (x:xs)) ++ subcadenasNoVacias xs

-- 2ª solución
subcadenasNoVacias2 :: [a] -> [[a]]
subcadenasNoVacias2 xs = 
    [take i (drop j xs) | j <- [0..n], i <- [1..n-j]]
    where n = length xs

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Partiendo de un número d, se construye la sucesión que
-- empieza en d y cada término se obtiene sumándole al anterior el
-- producto de sus dígitos no nulos. Por ejemplo:
-- * Si empieza en 1, la sucesión es 1,2,4,8,16,22,26,38,62,74,...
-- * Si empieza en 30, la sucesión es 30,33,42,50,55,80,88,152,162,174,...
-- 
-- Definir la función
--     sucesion :: Integer -> [Integer]
-- tal que (sucesion d) es la sucesión que empieza en d. Por ejemplo,
--     ghci> take 10 (sucesion 1)
--     [1,2,4,8,16,22,26,38,62,74]
--     ghci> take 10 (sucesion 3)
--     [3,6,12,14,18,26,38,62,74,102]
--     ghci> take 10 (sucesion 30)
--     [30,33,42,50,55,80,88,152,162,174]
--     ghci> take 10 (sucesion 10)
--     [10,11,12,14,18,26,38,62,74,102]
-- ---------------------------------------------------------------------

-- 1ª definición
sucesion :: Integer -> [Integer]
sucesion d = iterate f d
    where f x = x + productoDigitosNN x 

-- (productoDigitosNN x) es el producto de los dígitos no nulos de
-- x. Por ejemplo, 
--    productoDigitosNN 306  ==  18
productoDigitosNN :: Integer -> Integer
productoDigitosNN = product . digitosNoNulos

-- (digitosNoNulos x) es la lista de los dígitos no nulos de x. Por
-- ejemplo, 
--    digitosNoNulos 306  ==  [3,6]
digitosNoNulos:: Integer -> [Integer]
digitosNoNulos n = [read [x] | x <- show n, x /= '0']

-- 2ª definición
sucesion2 :: Integer -> [Integer]
sucesion2 d = [aux n | n <- [0..]]
    where aux 0 = d
          aux n = x + productoDigitosNN x 
              where x = aux (n-1) 

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Las sucesiones así construidas tienen un elemento
-- común, a partir del cual los términos coinciden. Por ejemplo,
--    take 7 (sucesion 3)  ==  [3,6,    12,14,18,26,38]
--    take 7 (sucesion 5)  ==  [5,10,11,12,14,18,26]
-- se observa que las sucesiones que empiezan en 3 y 5, respectivamente,
-- coinciden a partir del término 12.
-- 
-- Definir la función 
--     comun :: Integer -> Integer -> Integer
-- tal que (comun x y) es el primer elemento común de las sucesiones que
-- empiezan en x e y, respectivamente. Por ejemplo,
--     comun 3 5     == 12
--     comun 3 4     == 26
--     comun 3 8     == 26
--     comun 3 20    == 26
--     comun 3 34    == 126
--     comun 234 567 == 1474
-- ---------------------------------------------------------------------

comun :: Integer -> Integer -> Integer
comun x y = 
    head [n | n <- sucesion x, n `elem` takeWhile (<=n) (sucesion y)]

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir la función
--     indicesComun :: Integer -> Integer -> (Integer, Integer)
-- tal que (indicesComun x y) calcula los índices a partir de los cuales
-- las sucesiones con valores iniciales x e y coinciden. Por ejemplo,
--     indicesComun 3 4     == (6,5)
--     indicesComun 3 5     == (3,4)
--     indicesComun 3 8     == (6,4)
--     indicesComun 3 20    == (6,3)
--     indicesComun 3 34    == (15,5)
--     indicesComun 234 567 == (16,19)
-- ---------------------------------------------------------------------

indicesComun :: Integer -> Integer -> (Integer, Integer)
indicesComun x y = (i,j)
    where z = comun x y
          i = head [k | (a,k) <- zip (sucesion x) [1..], a == z]
          j = head [k | (a,k) <- zip (sucesion y) [1..], a == z]


-- ---------------------------------------------------------------------
-- Ejercicio 4. Se consideran los árboles binarios definidos por 
--    data Arbol a = Hoja | Nodo a (Arbol a) (Arbol a)
--                   deriving Show
--    
-- Por ejemplo, el árbol
--            5 
--           / \
--          /   \
--         4     7
--        / \   / \  
--       1         8  
--     /  \       /  \
-- se representa por
--    arbol1 = Nodo 5 (Nodo 4 (Nodo 1 Hoja Hoja) Hoja )
--                    (Nodo 7 Hoja (Nodo 8 Hoja Hoja))
--
-- Definir la función 
--    takeArbolWhile :: (a -> Bool) -> Arbol a -> Arbol a
-- tal que (takeArbolWhile p ar) es el subárbol de ar empezando desde la
-- raiz mientras se verifique p. Por ejemplo,
--   takeArbolWhile odd arbol1   == Nodo 5 Hoja (Nodo 7 Hoja Hoja)
--   takeArbolWhile even arbol1  == Hoja
--   takeArbolWhile (< 6) arbol1 == Nodo 5 (Nodo 4 (Nodo 1 Hoja Hoja) Hoja) Hoja
-- ---------------------------------------------------------------------

data Arbol a = Hoja | Nodo a (Arbol a) (Arbol a)
               deriving Show

arbol1 = Nodo 5 (Nodo 4 (Nodo 1 Hoja Hoja) Hoja )
                (Nodo 7 Hoja (Nodo 8 Hoja Hoja))

takeArbolWhile :: (a -> Bool) -> Arbol a -> Arbol a
takeArbolWhile p Hoja = Hoja
takeArbolWhile p (Nodo a x y) 
    | p a       = Nodo a (takeArbolWhile p x) (takeArbolWhile p y)
    | otherwise = Hoja

-- ---------------------------------------------------------------------
-- Ejercicio 5. Los vectores son tablas cuyos índices son números
-- naturales. 
--    type Vector a = Array Int a
-- Las matrices son tablas cuyos índices son pares de números naturales. 
--    type Matriz a = Array (Int,Int) a
-- Por ejemplo,
--    c1, c2:: Matriz Double
--    c1 = listArray ((1,1),(4,4)) [1,3,0,0,
--                                  -1, 1,-1, 1,
--                                  1,-1, 1,-1,
--                                  1, 1,-1, 1]
--    
--    c2 = listArray ((1,1),(2,2)) [1,1,1,-1]
-- 
-- Definir la función
--     determinante:: Matriz Double -> Double
-- tal que (determinante p) es el determinante de la matriz p,
-- desarrollándolo por los elementos de una fila. Por ejemplo,
--     determinante c1 == 0.0
--     determinante c2 == -2.0
-- ---------------------------------------------------------------------

type Vector a = Array Int a
type Matriz a = Array (Int,Int) a

c1, c2:: Matriz Double
c1 = listArray ((1,1),(4,4)) [1,3,0,0,
                              -1, 1,-1, 1,
                              1,-1, 1,-1,
                              1, 1,-1, 1]

c2 = listArray ((1,1),(2,2)) [1,1,1,-1]

determinante:: Matriz Double -> Double
determinante p
    | (m,n) == (1,1) = p!(1,1)
    | otherwise =
        sum [((-1)^(i+1))*(p!(i,1))*determinante (submatriz i 1 p)
                 | i <- [1..m]]
            where (_,(m,n)) = bounds p


-- (submatriz i j p) es la submatriz de p obtenida eliminado la fila i y
-- la columna j. Por ejemplo,
--    submatriz 2 3 (listArray ((1,1),(3,3)) [2,1,5,1,2,3,5,4,2])
--    array ((1,1),(2,2)) [((1,1),2),((1,2),1),((2,1),5),((2,2),4)]
--    submatriz 2 3 (listArray ((1,1),(3,3)) [1..9])
--    array ((1,1),(2,2)) [((1,1),1),((1,2),2),((2,1),7),((2,2),8)]
submatriz :: Num a => Int -> Int -> Matriz a -> Matriz a
submatriz i j p = array ((1,1), (m-1,n -1))
                  [((k,l), p ! f k l) | k <- [1..m-1], l <- [1..n-1]]
                      where (_,(m,n)) = bounds p
                            f k l | k <  i && l <  j = (k,l)
                                  | k >= i && l <  j = (k+1,l)
                                  | k <  i && l >= j = (k,l+1)
                                  | otherwise = (k+1,l+1)
