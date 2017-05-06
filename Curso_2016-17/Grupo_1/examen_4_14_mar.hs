-- Informática (1º del Grado en Matemáticas)
-- 4º examen de evaluación continua (14 de marzo de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Graphics.Gnuplot.Simple
import Data.Numbers.Primes
import Data.Matrix

-- ---------------------------------------------------------------------
-- Ejercicio 1. Decimos que una lista de números enteros es un camino
-- primo si todos sus elementos son primos y cada uno se diferencia del
-- anterior en un único dígito. Por ejemplo, 
--    [1033, 1733, 3733, 3739, 3779, 8779, 8179] es un camino primo, y
--    [1033, 1733, 3733, 3739, 3793, 4793] no lo es.
--
-- Definir la función
--    caminoPrimo:: [Integer] -> Bool
-- tal que (caminoPrimo xs) se verifica si xs es un camino primo. Por
-- ejemplo, 
--    caminoPrimo [1033, 1733, 3733, 3739, 3779, 8779, 8179] == True
--    caminoPrimo [1033, 1733, 3733, 3739, 3793, 4793]       == False
-- ---------------------------------------------------------------------

caminoPrimo :: [Integer] -> Bool
caminoPrimo ps = all pred (zip ps (tail ps))
  where pred (x,y) = isPrime x && isPrime y && unDigitoDistinto x y

unDigitoDistinto :: Integer-> Integer -> Bool
unDigitoDistinto n m = aux (show n) (show m)
  where aux (x:xs) (y:ys) | x /= y    = xs == ys
                          | otherwise = aux xs ys
        aux _ _ = False

-- ---------------------------------------------------------------------
-- Ejercicio 2. Consideramos el siguiente tipo algebraico de los árboles
-- binarios: 
--    data Arbol a = H a
--                 | N a (Arbol a) (Arbol a)
--       deriving (Show,Eq)
-- y el árbol
--    a1:: Arbol Int
--    a1 = N 9 (N 3 (H 2) (N 4 (H 1) (H 5))) (H 8)
--
--                      9
--                    /   \
--                   3     8
--                  / \
--                 2   4
--                    / \
--                   1   5
--
-- En este árbol hay una rama en la que todos sus elementos son mayores
-- que 7, pero no hay ninguna rama en la que todos sus elementos sean
-- pares. 
-- 
-- Definir la función
--    propExisteTodos :: (a -> Bool) -> Arbol a -> Bool
-- tal que (propExisteTodos p a) se verifica si hay una rama en la que
-- todos sus nodos (internos u hoja) cumple la propiedad p. Por ejemplo
--    propExisteTodos even a1  ==  False
--    propExisteTodos (>7) a1  ==  True
--    propExisteTodos (<=9) a1 ==  True
-- ---------------------------------------------------------------------

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
  deriving (Show,Eq)
 
a1 :: Arbol Int
a1 = N 9 (N 3 (H 2) (N 4 (H 1) (H 5))) (H 8)
 
propiedadEA :: (a -> Bool) -> Arbol a -> Bool
propiedadEA p a = any (all p) (ramas a)

ramas :: Arbol a -> [[a]]
ramas (H x)     = [[x]]
ramas (N x i d) = [x:ys | ys <- ramas i ++ ramas d]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Representamos un tablero del juego de los barcos
-- mediante una matriz de 0 y 1, en la que cada barco está formado por
-- uno o varios 1 consecutivos, tanto en horizontal como en vertical.
-- Por ejemplo, las  siguientes matrices representan distintos tableros:
--    ej1, ej2, ej3, ej4 :: Matrix Int
--    ej1 = fromLists [[0,0,1,1],
--                     [1,0,1,0],
--                     [0,0,0,1],
--                     [1,1,0,1]]
--    ej2 = fromLists [[0,0,0,1],
--                     [1,0,0,0],
--                     [0,0,1,1],
--                     [1,0,0,1]]
--    ej3 = fromLists [[1,1,0,0],
--                     [0,1,0,1],
--                     [1,1,1,0],
--                     [0,0,1,0]]
--    ej4 = joinBlocks (ej1,ej3,ej2,ej3)  
-- 
-- Definir la función
--    posicionesComunes :: Matrix Int -> Matrix Int -> [(Int,Int)]
-- tal que (posicionesComunes p q) es la lista con las posiciones
-- comunes ocupadas por algún barco en ambas matrices. Por ejemplo,
--    posicionesComunes ej1 ej2 == [(1,4),(2,1),(3,4),(4,1),(4,4)]
--    posicionesComunes ej1 ej3 == []
--    posicionesComunes ej2 ej3 == [(3,3)]
-- ---------------------------------------------------------------------

ej1, ej2, ej3, ej4 :: Matrix Int
ej1 = fromLists [[0,0,1,1],
                 [1,0,1,0],
                 [0,0,0,1],
                 [1,1,0,1]]
ej2 = fromLists [[0,0,0,1],
                 [1,0,0,0],
                 [0,0,1,1],
                 [1,0,0,1]]
ej3 = fromLists [[1,1,0,0],
                 [0,1,0,1],
                 [1,1,1,0],
                 [0,0,1,0]]
ej4 = joinBlocks (ej1,ej3,ej2,ej3)  

-- 1ª definición:
posicionesComunes :: Matrix Int -> Matrix Int -> [(Int,Int)]
posicionesComunes p q =
  [(i,j) | i <-[1..m], j <-[1..n], a !(i,j) == 2]
  where m = nrows p
        n = ncols p
        a = p + q

-- 2ª definición:
posicionesComunes2 :: Matrix Int -> Matrix Int -> [(Int,Int)]
posicionesComunes2 p q =
  [(i,j) | i <-[1..m], j <-[1..n], p !(i,j) == 1 && q!(i,j) == 1]
  where m = nrows p
        n = ncols p

-- ---------------------------------------------------------------------
-- Ejercicio 3.2 Definir una función
--    juego :: Matrix Int -> IO ()
-- que pregunte al usuario por una posición y devuelva "Tocado" o
-- "Agua" según lo que haya en esa posición en el tablero ej4. Una
-- posible sesión puede ser la siguiente.
--    ghci> juego ej4
--    Elije una fila: 
--    3
--    Elije una columna: 
--    3
--    Agua
-- ---------------------------------------------------------------------

juego :: Matrix Int -> IO ()
juego p =
    do putStrLn "Elije una fila: "
       ci <- getLine
       let i = read ci 
       putStrLn "Elije una columna: "
       cj <- getLine
       let j = read cj
       if p ! (i,j) == 1
          then (putStrLn "Tocado")
          else (putStrLn "Agua")

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. La fracción continua determinada por las sucesiones
-- as = [a1,a2,a3,...] y bs = [b1,b2,b3,...] es la siguiente
--                    b1
--     a1 + --------------------------------------
--                         b2
--           a2 + ---------------------------------
--                               b3
--                 a3 + ---------------------------
--                                     b4
--                       a4 + ---------------------
--                                           b5
--                             a5 + ---------------
--                                      ...
-- 
-- Definir la función
--    aproxFracionContinua:: [Int] -> [Int] -> Int -> Double
-- tal que (aproxFracionContinua xs ys n) es la aproximación con n
-- términos de la fracción contínua determinada por xs e ys. Por
-- ejemplo, 
--    aproxFracionContinua [1,3..] [2,4..] 100       == 1.5414940825367982
--    aproxFracionContinua (repeat 1) (repeat 3) 100 == 2.302775637731995
-- ---------------------------------------------------------------------

aproxFracionContinua :: [Int] -> [Int] -> Int -> Double
aproxFracionContinua xs ys n = foldl f (a + b) (zip as bs)
  where (a:as) = map fromIntegral (reverse (take n xs))
        (b:bs) = map fromIntegral (reverse (take n ys))
        f z (x,y) = x + y/z


-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Una aproximación de pi mediante fracciones continuas
-- es la siguiente:
--    4                   1^2
--    --  = 1 + --------------------------------------
--    pi                        2^2
--              3 + ---------------------------------
--                                    3^2
--                    5 + ---------------------------
--                                           4^2
--                          7 + ---------------------
--                                                5^2
--                                9 + ---------------
--                                         ...
-- 
-- 
-- Definir la función
--    aproximacionPi :: Int -> Double
-- tal que (aproximacionPi n) es la n-ésima aproximación de pi, mediante
-- la expresión anterior. Por ejemplo,
--    aproximacionPi 100 == 3.141592653589793
-- ---------------------------------------------------------------------

aproximacionPi :: Int -> Double
aproximacionPi n = 4/(aproxFracionContinua as bs n)
  where as = [1,3..]
        bs = map (^2) [1..]

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Definir la función
--    grafica :: [Int] -> IO ()
-- tal que (grafica ns) dibuja la gráfica de las k-ésimas aproximaciones
-- de pi donde k toma los valores de la lista ns.
-- ---------------------------------------------------------------------

grafica :: [Int] -> IO ()
grafica ns = plotList [] $ map aproximacionPi ns



