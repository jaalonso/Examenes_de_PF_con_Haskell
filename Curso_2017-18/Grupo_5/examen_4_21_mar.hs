-- Informática (1º del Grado en Matemáticas), Grupo 5
-- 4º examen de evaluación continua (21 de marzo de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Librerías auxiliares
-- ---------------------------------------------------------------------

import Data.List
import Data.Numbers.Primes
import Data.Matrix

-- -----------------------------------------------------------------------
-- Ejercicio 1.1. Se considera una enumeración de los números primos:
--     p(1)=2,  p(2)=3, p(3)=5, p(4)=7, p(5)=11, p(6)=13, p(7)=17,...
-- Dado un entero positivo x, definimos:
-- + longitud de x como el mayor i tal que el primo p(i) aparece en la
--   factorización en números primos de x
-- + altura de x el mayor exponente n que aparece en la factorización
--   en números primos de x
--
-- Por ejemplo, 3500 tiene longitud 4 y altura 3, pues 3500=2^2*5^3*7^1;
-- y 34 tiene longitud 7 y altura 1, pues 34 = 2*17.

-- Definir las funciones
--    longitud :: Integer -> Integer
--    altura   :: Integer -> Integer
-- que calculan la longitud y la altura, respectivamente, de un entero
-- positivo. Por ejemplo, 
--    longitud 3500  ==  4
--    altura 3500    ==  3

longitud :: Integer -> Integer
longitud 1 = 0
longitud x = genericLength (takeWhile (<= last (primeFactors x)) primes)

altura :: Integer -> Integer
altura 1 = 0
altura x = maximum  (map genericLength (group (primeFactors x)))

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Diremos que dos enteros positivos a y b están
-- relacionados si tienen la misma longitud y la misma altura.
-- 
-- Definir la lista infinita
--    paresRel :: [(Integer,Integer)]
-- que enumera todos los pares (a,b), con 1<=a<b, tales que a y b están
-- relacionados. Por ejemplo, 
--    λ> take 9 paresRel
--    [(3,6),(5,10),(9,12),(7,14),(5,15),(10,15),(9,18),(12,18),(7,21)]
-- ---------------------------------------------------------------------

paresRel :: [(Integer,Integer)]
paresRel =
  [(a,b) | b <- [1..]
         , a <- [1..b-1]
         , longitud a == longitud b
         , altura a == altura b]

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Calcular en qué posición aparece el par (31,310) en la
-- lista infinta ParesRel
-- ---------------------------------------------------------------------

-- El cálculo es
--    λ> 1 + genericLength (takeWhile (/= (31,310)) paresRel)
--    712

-- ---------------------------------------------------------------------
-- Ejercicio 2. Representamos los puntos del plano como pares de números
-- reales 
--    type Punto = (Float,Float)
-- y un camino de k pasos como una lista de k+1 puntos,
--    xs = [x(0),x(1),....,x(k)]
-- Por ejemplo, consideramos el siguiente camino de 4 pasos:
--    camino :: [Punto]
--    camino = [(0,0),(2,2),(5,2),(5,-1),(0,-1)]                
-- 
-- Definir la función
--    mediaPasos :: [Punto] -> Double
-- tal que (mediaPasos xs) es la media aritmética de las longitudes de
-- los pasos que conforman el camino xs. Por ejemplo
--    mediaPasos camino  ==  3.4571068
-- ---------------------------------------------------------------------

type Punto =(Float,Float)

camino :: [Punto]
camino = [(0,0),(2,2),(5,2),(5,-1),(0,-1)]                

mediaPasos :: [Punto] -> Float
mediaPasos xs =
  sum [distancia p q | (p,q) <- zip xs (tail xs)] / k  
  where k = genericLength xs - 1

distancia :: Punto -> Punto -> Float
distancia (x1,y1) (x2,y2) = sqrt ((x1-x2)^2+(y1-y2)^2)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Representamos los árboles binarios mediante el tipo de
-- dato 
--    data Arbol a = H a | N a (Arbol a) (Arbol a) deriving Show
-- 
-- Una forma de ampliar un árbol binario es añadiendo un nuevo nivel donde
-- las nuevas hojas sean iguales a la suma de los valores de los nodos
-- desde el padre hasta llegar a la raíz (inclusives). Por ejemplo:
--       5               5       |         3                3
--      / \             / \      |        / \             /   \
--     2   0    ==>    2   0     |       2  -9    ==>    2    -9
--                    / \ / \    |                      / \   / \
--                   7  7 5  5   |                     5   5 -6 -6
--                                                    /\   /\
--                                                   6  6 5  5
--
-- Definir la función
--    ampliaArbol:: Num a => Arbol a -> Arbol a
-- tal que (ampliaArbol a) es el árbol a ampliado en un nivel. Por
-- ejemplo, 
--    λ> ampliaArbol (N 5 (H 2)(H 0))
--    N 5 (N 2 (H 7) (H 7)) (N 0 (H 5) (H 5))
--    λ> ampliaArbol (H 1)
--    N 1 (H 1) (H 1)
--    λ> ampliaArbol N 1 (H 1) (H 1)
--    N 1 (N 1 (H 2) (H 2)) (N 1 (H 2) (H 2))
-- ---------------------------------------------------------------------

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
  deriving Show

ampliaArbol :: Num a => Arbol a -> Arbol a
ampliaArbol a = aux 0 a
  where aux n (H x)     = N x (H (n+x)) (H (n+x))
        aux n (N x i d) = N x (aux (n+x) i) (aux (n+x) d)

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Representamos las matrices mediante la librería
-- Data.Matrix.
-- 
-- Definir la función
--    ampliada :: Num a => Matrix a -> Matrix a
-- tal que (ampliada p) devuelve la matriz obtenida a partir de p
-- al ampliar cada fila con la suma de sus elementos. Por ejemplo, 
--   λ> ampliada (fromLists [[1,2,3],[1,-1,0],[2,2,5]])
--   (  1  2  3  6 )
--   (  1 -1  0  0 )
--   (  2  2  5  9 )
-- ---------------------------------------------------------------------

ampliada :: Num a => Matrix a -> Matrix a
ampliada p = matrix n (m+1) f
  where n = nrows p
        m = ncols p
        f (i,j) | j <= m    = p!(i,j)
                | otherwise = sum [p!(i,z) | z <- [1..m]]

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función
--    bordeNulo :: Num a => Matrix a -> Matrix a
-- tal que (bordeNulo p) se verifica si todos los elementos de los
-- bordes de la matriz p son nulos. Por ejemplo.
--    λ> bordeNulo (fromLists [[0,0,0,0],[0,3,1,0],[0,0,0,0]])
--    True
--    λ> bordeNulo (fromLists [[0,0,0,0],[0,3,1,0],[0,0,0,-1]])
--    False
-- ---------------------------------------------------------------------

bordeNulo :: (Num a,Eq a) => Matrix a -> Bool
bordeNulo p = all (==0) ([p!(1,j) | j <- [1..m]] ++
                         [p!(n,j) | j <- [1..m]] ++
                         [p!(i,1) | i <- [2..n-1]] ++
                         [p!(i,m) | i <- [2..n-1]])
  where n = nrows p
        m = ncols p

