-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 5º examen de evaluación continua (10 de mayo de 2013)
-- ---------------------------------------------------------------------

import Data.Array
import Data.Ratio
import PolOperaciones

-- ---------------------------------------------------------------------
-- Ejercicio 1 (370 del Proyecto Euler). Un triángulo geométrico es un
-- triángulo de lados enteros, representados por la terna (a,b,c) tal
-- que a ≤ b ≤ c y están en progresión geométrica, es decir, 
-- b^2 = a*c. Por ejemplo, un triángulo de lados a = 144, b = 156 y 
-- c = 169. 
-- 
-- Definir la función 
--    numeroTG :: Integer -> Int
-- tal que (numeroTG n) es el número de triángulos geométricos de
-- perímetro menor o igual que n. Por ejemplo
--     numeroTG 10  == 4
--     numeroTG 100 == 83
--     numeroTG 200 == 189
-- ---------------------------------------------------------------------

-- 1ª definición:
numeroTG :: Integer -> Int
numeroTG n = 
    length [(a,b,c) | c <- [1..n], 
                      b <- [1..c],
                      a <- [1..b],
                      a+b+c <= n,
                      b^2 == a*c]

-- 2ª definición:
numeroTG2 :: Integer -> Int
numeroTG2 n = 
    length [(a,b,c) | c <- [1..n],
                      b <- [1..c],
                      b^2 `rem` c == 0,
                      let a = b^2 `div` c,
                      a+b+c <= n]

-- 3ª definición:
numeroTG3 :: Integer -> Int
numeroTG3 n = 
    length [(b^2 `div` c,b,c) | c <- [1..n],
                                b <- [1..c],
                                b^2 `rem` c == 0,
                                (b^2 `div` c)+b+c <= n]

-- Comparación de eficiencia:
--    ghci> numeroTG 200
--    189
--    (2.32 secs, 254235740 bytes)
--    ghci> numeroTG2 200
--    189
--    (0.06 secs, 5788844 bytes)
--    ghci> numeroTG3 200
--    189
--    (0.06 secs, 6315900 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2 (Cálculo numérico) El método de la bisección para
-- calcular un cero de una función en el intervalo [a,b] se basa en el
-- teorema de Bolzano:  
--    "Si f(x) es una función continua en el intervalo [a, b], y si,
--    además, en los extremos del intervalo la función f(x) toma valores
--    de signo opuesto (f(a) * f(b) < 0), entonces existe al menos un
--    valor c en (a, b) para el que f(c) = 0".
-- 
-- La idea es tomar el punto medio del intervalo c = (a+b)/2 y
-- considerar los siguientes casos:
-- * Si f(c) ~= 0, hemos encontrado una aproximación del punto que
--   anula f en el intervalo con un error aceptable.
-- * Si f(c) tiene signo distinto de f(a), repetir el proceso en el
--   intervalo [a,c].
-- * Si no, repetir el proceso en el intervalo [c,b].
-- 
-- Definir la función 
--    ceroBiseccionE :: (Float -> Float) -> Float -> Float -> Float -> Float
-- tal que (ceroBiseccionE f a b e) es una aproximación del punto
-- del intervalo [a,b] en el que se anula la función f, con un error
-- menor que e, aplicando el método de la bisección (se supone que 
-- f(a)*f(b)<0). Por ejemplo,
--    let f1 x = 2 - x
--    let f2 x = x^2 - 3 
--    ceroBiseccionE f1 0 3 0.0001     == 2.000061
--    ceroBiseccionE f2 0 2 0.0001     == 1.7320557
--    ceroBiseccionE f2 (-2) 2 0.00001 == -1.732048
--    ceroBiseccionE cos 0 2 0.0001    == 1.5708008
-- ---------------------------------------------------------------------

ceroBiseccionE :: (Float -> Float) -> Float -> Float -> Float -> Float
ceroBiseccionE f a b e = aux a b
    where aux c d | aceptable m     = m
                  | f c * f m  < 0  = aux c m
                  | otherwise       = aux m d
              where m = (c+d)/2
                    aceptable x = abs (f x) < e 

-- ---------------------------------------------------------------------
-- Ejercicio 3 Definir la función
--    numeroAPol :: Int -> Polinomio Int
-- tal que (numeroAPol n) es el polinomio cuyas raices son las
-- cifras de n. Por ejemplo,
--   numeroAPol 5703 == x^4 + -15*x^3 + 71*x^2 + -105*x
-- ---------------------------------------------------------------------

numeroAPol :: Int -> Polinomio Int
numeroAPol n = numerosAPol (cifras n)

-- (cifras n) es la lista de las cifras de n. Por ejemplo, 
--    cifras 5703  ==  [5,7,0,3]
cifras :: Int -> [Int]
cifras n = [read [c] | c <- show n]

-- (numeroAPol xs) es el polinomio cuyas raices son los elementos de
-- xs. Por ejemplo, 
--    numerosAPol [5,7,0,3]  ==  x^4 + -15*x^3 + 71*x^2 + -105*x
numerosAPol :: [Int] -> Polinomio Int
numerosAPol [] = polUnidad
numerosAPol (x:xs) = 
    multPol (consPol 1 1 (consPol 0 (-x) polCero))
            (numerosAPol xs)

-- La función anterior se puede definir mediante plegado
numerosAPol2 :: [Int] -> Polinomio Int
numerosAPol2 = 
     foldr (\ x -> multPol (consPol 1 1 (consPol 0 (-x) polCero)))
           polUnidad

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Consideremos el tipo de los vectores y de las matrices
--    type Vector a = Array Int a
--    type Matriz a = Array (Int,Int) a
-- y los ejemplos siguientes:
--    p1 :: (Fractional a, Eq a) => Matriz a          
--    p1 = listArray ((1,1),(3,3)) [1,0,0,0,0,1,0,1,0]
--    
--    v1,v2 :: (Fractional a, Eq a) => Vector a          
--    v1 = listArray (1,3) [0,-1,1]
--    v2 = listArray (1,3) [1,2,1]
-- 
-- Definir la función
--    esAutovector :: (Fractional a, Eq a) => 
--                    Vector a -> Matriz a -> Bool
-- tal que (esAutovector v p) compruebe si v es un autovector de p
-- (es decir, el producto de v por p es un vector proporcional a
-- v). Por ejemplo, 
--    esAutovector v2 p1 == False
--    esAutovector v1 p1 == True
-- ---------------------------------------------------------------------

type Vector a = Array Int a
type Matriz a = Array (Int,Int) a

p1:: (Fractional a, Eq a) => Matriz a          
p1 = listArray ((1,1),(3,3)) [1,0,0,0,0,1,0,1,0]

v1,v2:: (Fractional a, Eq a) => Vector a          
v1 = listArray (1,3) [0,-1,1]
v2 = listArray (1,3) [1,2,1]

esAutovector :: (Fractional a, Eq a) => Vector a -> Matriz a -> Bool
esAutovector v p = proporcional (producto p v) v

-- (producto p v) es el producto de la matriz p por el vector v. Por
-- ejemplo, 
--    producto p1 v1  = array (1,3) [(1,0.0),(2,1.0),(3,-1.0)]
--    producto p1 v2  = array (1,3) [(1,1.0),(2,1.0),(3,2.0)]
producto :: (Fractional a, Eq a) => Matriz a -> Vector a -> Vector a 
producto p v =
    array (1,n) [(i, sum [p!(i,j)*v!j | j <- [1..n]]) | i <- [1..m]]
    where (_,n)     = bounds v
          (_,(m,_)) = bounds p

-- (proporcional v1 v2) se verifica si los vectores v1 y v2 son
-- proporcionales. Por ejemplo,
--    proporcional v1 v1                           = True
--    proporcional v1 v2                           = False
--    proporcional v1 (listArray (1,3) [0,-5,5])   = True
--    proporcional v1 (listArray (1,3) [0,-5,4])   = False
--    proporcional (listArray (1,3) [0,-5,5]) v1   = True
--    proporcional v1 (listArray (1,3) [0,0,0])    = True
--    proporcional (listArray (1,3) [0,0,0]) v1    = False
proporcional :: (Fractional a, Eq a) => Vector a -> Vector a -> Bool
proporcional v1 v2 
    | esCero v1 = esCero v2
    | otherwise = and [v2!i == k*(v1!i) | i <- [1..n]]
    where (_,n) = bounds v1
          j     = minimum [i | i <- [1..n], v1!i /= 0]
          k     = (v2!j) / (v1!j)

-- (esCero v) se verifica si v es el vector 0.
esCero :: (Fractional a, Eq a) => Vector a -> Bool 
esCero v = null [x | x <- elems v, x /= 0]

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función
--    autovalorAsociado :: (Fractional a, Eq a) => 
--                         Matriz a -> Vector a -> Maybe a
-- tal que si v es un autovector de p, calcule el autovalor asociado.
-- Por ejemplo,
--    autovalorAsociado p1 v1 == Just (-1.0)
--    autovalorAsociado p1 v2 == Nothing
-- ---------------------------------------------------------------------

autovalorAsociado :: (Fractional a, Eq a) => 
                     Matriz a -> Vector a -> Maybe a
autovalorAsociado p v
    | esAutovector v p = Just (producto p v ! j / v ! j)
    | otherwise        = Nothing
    where (_,n) = bounds v
          j     = minimum [i | i <- [1..n], v!i /= 0]
