-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 8º examen de evaluación continua (24 de junio de 2011)
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    ordena :: (a -> a -> Bool) -> [a] -> [a]
-- tal que (ordena r xs) es la lista obtenida ordenando los elementos de
-- xs según la relación r. Por ejemplo,
--   ghci> ordena (\x y -> abs x < abs y) [-6,3,7,-9,11]
--   [3,-6,7,-9,11]
--   ghci> ordena (\x y -> length x < length y) [[2,1],[3],[],[1]]
--   [[],[3],[1],[2,1]]
-- ---------------------------------------------------------------------

ordena :: (a -> a -> Bool) -> [a] -> [a]
ordena _ [] = []
ordena r (x:xs) =
    (ordena r menores) ++ [x] ++ (ordena r mayores)
        where menores = [y | y <- xs, r y x]
              mayores = [y | y <- xs, not (r y x)]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Se consideran el tipo de las matrices definido por 
--    type Matriz a = Array (Int,Int) a
-- y, como ejemplo, la matriz q definida por
--    q :: Matriz Int
--    q = array ((1,1),(2,2)) [((1,1),3),((1,2),2),((2,1),3),((2,2),1)]
-- 
-- Definir la función
--    indicesMaximo :: (Num a, Ord a) => Matriz a -> [(Int,Int)]
-- tal que (indicesMaximo p) es la lista de los índices del elemento
-- máximo de la matriz p. Por ejemplo,
--    indicesMaximo q  ==  [(1,1),(2,1)]
-- ---------------------------------------------------------------------

type Matriz a = Array (Int,Int) a

q :: Matriz Int
q = array ((1,1),(2,2)) [((1,1),3),((1,2),2),((2,1),3),((2,2),1)]

indicesMaximo :: (Num a, Ord a) => Matriz a -> [(Int,Int)]
indicesMaximo p = [(i,j) | (i,j) <- indices p, p!(i,j) == m]
    where m = maximum (elems p)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Los montículo se pueden representar mediante el
-- siguiente tipo de dato algebraico.
--    data Monticulo = Vacio 
--                   | M Int Monticulo Monticulo
--                   deriving Show
-- Por ejemplo, el montículo
--         1
--        / \
--       /   \
--      5     4
--     / \   /
--    7   6 8
-- se representa por
--    m1, m2, m3 :: Monticulo 
--    m1 = M 1 m2 m3
--    m2 = M 5 (M 7 Vacio Vacio) (M 6 Vacio Vacio)
--    m3 = M 4 (M 8 Vacio Vacio) Vacio
-- 
-- Definir las funciones
--    ramaDerecha :: Monticulo -> [Int]
--    rango :: Monticulo -> Int
-- tales que (ramaDerecha m) es la rama derecha del montículo m. Por ejemplo,
--    ramaDerecha m1  ==  [1,4]
--    ramaDerecha m2  ==  [5,6]
--    ramaDerecha m3  ==  [4]
-- y (rango m) es el rango del montículo m; es decir, la menor distancia
-- desde la raíz de m a un montículo vacío. Por ejemplo,  
--    rango m1        ==  2   
--    rango m2        ==  2
--    rango m3        ==  1
-- ---------------------------------------------------------------------

data Monticulo = Vacio 
               | M Int Monticulo Monticulo
               deriving Show

m1, m2, m3 :: Monticulo
m1 = M 1 m2 m3
m2 = M 5 (M 7 Vacio Vacio) (M 6 Vacio Vacio)
m3 = M 4 (M 8 Vacio Vacio) Vacio

ramaDerecha :: Monticulo -> [Int]
ramaDerecha Vacio = []
ramaDerecha (M v i d) = v : ramaDerecha d

rango :: Monticulo -> Int
rango Vacio = 0
rango (M _ i d) = 1 + min (rango i) (rango d)

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Los polinomios pueden representarse mediante listas
-- dispersas. Por ejemplo, el polinomio x^5+3x^4-5x^2+x-7 se representa
-- por [1,3,0,-5,1,-7]. En dicha lista, obviando el cero, se producen
-- tres cambios de signo: del 3 al -5, del -5 al 1 y del 1 al
-- -7. Llamando C(p) al número de cambios de signo en la lista de
-- coeficientes del polinomio p(x), tendríamos entonces que en este caso 
-- C(p)=3. 
--
-- La regla de los signos de Descartes dice que el número de raíces
-- reales positivas de una ecuación polinómica con coeficientes reales
-- igualada a cero es, como mucho, igual al número de cambios de signo
-- que se produzcan entre sus coeficientes (obviando los ceros). Por
-- ejemplo, en el caso anterior la ecuación tendría como mucho tres
-- soluciones reales positivas, ya que C(p)=3. 
-- 
-- Además, si la cota C(p) no se alcanza, entonces el número de raíces
-- positivas de la ecuación difiere de ella un múltiplo de dos. En el
-- ejemplo anterior esto significa que la ecuación puede tener tres
-- raíces positivas o tener solamente una, pero no podría ocurrir que
-- tuviera dos o que no tuviera ninguna. 
-- 
-- Definir, por comprensión, la función
--    cambiosC :: [Int] -> [(Int,Int)]
-- tal que (cambiosC xs) es la lista de los pares de elementos de xs con
-- signos distintos, obviando los ceros. Por ejemplo,
--    cambiosC [1,3,0,-5,1,-7]  ==  [(3,-5),(-5,1),(1,-7)]
-- ---------------------------------------------------------------------

cambiosC :: [Int] -> [(Int,Int)]
cambiosC xs = [(x,y) | (x,y) <- consecutivos (noCeros xs), x*y < 0]
    where consecutivos xs = zip xs (tail xs)

-- (noCeros xs) es la lista de los elementos de xs distintos de cero. 
-- Por ejemplo,  
--    noCeros [1,3,0,-5,1,-7]  ==  [1,3,-5,1,-7]
noCeros = filter (/=0)

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir, por recursión, la función
--    cambiosR :: [Int] -> [(Int,Int)]
-- tal que (cambiosR xs) es la lista de los pares de elementos de xs con
-- signos distintos, obviando los ceros. Por ejemplo,
--    cambiosR [1,3,0,-5,1,-7]  ==  [(3,-5),(-5,1),(1,-7)]
-- ---------------------------------------------------------------------

cambiosR :: [Int] -> [(Int,Int)]
cambiosR xs = cambiosR' (noCeros xs)
    where cambiosR' (x:y:xs)
              | x*y < 0   = (x,y) : cambiosR' (y:xs)
              | otherwise = cambiosR' (y:xs)
          cambiosR' _ = []

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Comprobar con QuickCheck que las dos definiciones son
-- equivalentes. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_cambios :: [Int] -> Bool
prop_cambios xs =
    cambiosC xs == cambiosR xs

-- La comprobación es
--    ghci> quickCheck prop_cambios
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.4. Usando las anteriores definiciones y la regla de
-- Descartes, definir la función
--    nRaicesPositivas :: [Int] -> [Int]
-- tal que (nRaicesPositivas p) es la lista de los posibles números de
-- raíces positivas del polinomio p (representado mediante una lista
-- dispersa) según la regla de los signos de Descartes. Por ejemplo, 
--    nRaicesPositivas [1,3,0,-5,1,-7]  ==  [3,1]
-- que significa que la ecuación x^5+3x^4-5x^2+x-7=0 puede tener 3 ó 1
-- raíz positiva.
-- ---------------------------------------------------------------------

nRaicesPositivas :: [Int] -> [Int]
nRaicesPositivas xs = [n,n-2..0]
    where n = length (cambiosC xs)

-- ---------------------------------------------------------------------
-- Nota: El ejercicio 4 está basado en el artículo de Gaussiano "La
-- regla de los signos de Descartes" http://bit.ly/iZXybH
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Se considera la siguiente enumeración de los pares de
-- números naturales 
--    (0,0),
--    (0,1), (1,0),
--    (0,2), (1,1), (2,0),
--    (0,3), (1,2), (2,1), (3,0),
--    (0,4), (1,3), (2,2), (3,1), (4,0),
--    (0,5), (1,4), (2,3), (3,2), (4,1), (5,0), ...
-- 
-- Definir la función
--    siguiente :: (Int,Int) -> (Int,Int)
-- tal que (siguiente (x,y)) es el siguiente del término (x,y) en la
-- enumeración. Por ejemplo,
--    siguiente (2,0)  ==  (0,3)
--    siguiente (0,3)  ==  (1,2)
--    siguiente (1,2)  ==  (2,1)
-- ---------------------------------------------------------------------

siguiente :: (Int,Int) -> (Int,Int)
siguiente (x,0) = (0,x+1)
siguiente (x,y) = (x+1,y-1)

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir la constante
--    enumeracion :: [(Int,Int)]
-- tal que enumeracion es la lista que representa la anterior
-- enumeracion de los pares de números naturales. Por ejemplo,
--    take 6 enumeracion == [(0,0),(0,1),(1,0),(0,2),(1,1),(2,0)]
--    enumeracion !! 9  ==  (3,0)
-- ---------------------------------------------------------------------

enumeracion :: [(Int,Int)]
enumeracion = iterate siguiente (0,0)

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Definir la función
--       posicion :: (Int,Int) -> Int
--    tal que (posicion p) es la posición del par p en la
--    enumeración. Por ejemplo,
--       posicion (3,0)  ==  9
--       posicion (1,2)  ==  7
-- ---------------------------------------------------------------------

posicion :: (Int,Int) -> Int
posicion (x,y) = length (takeWhile (/= (x,y)) enumeracion)

-- ---------------------------------------------------------------------
-- Ejercicio 5.4. Definir la propiedad  
--    prop_posicion :: Int -> Bool
-- tal que (prop_posicion n) se verifica si para los n primeros términos
-- (x,y) de la enumeración se cumple que 
--    posicion (x,y) == (x+y)*(x+y+1) `div` 2 + x
-- Comprobar si la propiedad se cumple para los 100 primeros elementos. 
-- ---------------------------------------------------------------------

prop_posicion :: Int -> Bool
prop_posicion n =
    and [posicion (x,y) == (x+y)*(x+y+1) `div` 2 + x | 
         (x,y) <- take n enumeracion]

-- La comprobación es
--    ghci> prop_posicion 100
--    True
