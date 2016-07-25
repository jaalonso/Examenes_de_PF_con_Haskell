-- Informática (1º del Grado en Matemáticas, Grupo 2)
-- 5º examen de evaluación continua (22 de marzo de 2012)
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List
import PolOperaciones

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por comprensión, la función 
--    interseccionC :: Eq a => [[a]] -> [a]
-- tal que (interseccionC xss) es la lista con los elementos comunes a
-- todas las listas de xss. Por ejemplo,
--    interseccionC [[1,2],[3]] == []
--    interseccionC [[1,2],[3,2], [2,4,5,6,1]] == [2]
-- ---------------------------------------------------------------------

interseccionC :: Eq a => [[a]] -> [a]
interseccionC []   = []
interseccionC (xs:xss) = [x | x <- xs, and [x `elem` ys| ys <- xss]]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, por recurción, la función 
--    interseccionR :: Eq a => [[a]] -> [a]
-- tal que (interseccionR xss) es la lista con los elementos comunes a
-- todas las listas de xss. Por ejemplo,
--    interseccionR [[1,2],[3]] == []
--    interseccionR [[1,2],[3,2], [2,4,5,6,1]] == [2]
-- ---------------------------------------------------------------------

interseccionR :: Eq a => [[a]] -> [a]
interseccionR [xs]     = xs
interseccionR (xs:xss) = inter xs (interseccionR xss)
    where inter xs ys = [x | x <- xs, x `elem` ys]

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir la función 
--    primerComun :: Ord a => [a] -> [a] -> a
-- tal que (primerComun xs ys) el primer elemento común de las listas xs
-- e ys (suponiendo que ambas son crecientes y, posiblemente,
-- infinitas). Por ejemplo,
--    primerComun [2,4..] [7,10..] == 10
-- ---------------------------------------------------------------------

primerComun :: Ord a => [a] -> [a] -> a
primerComun xs ys = head [x | x <- xs, x `elem` takeWhile (<=x) ys]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, utilizando la función anterior, la función
--    mcm :: Int -> Int -> Int
-- tal que (mcm x y) es el mínimo común múltiplo de x e y. Por ejemplo,
--    mcm 123 45  == 1845
--    mcm 123 450 == 18450
--    mcm 35 450  == 3150
-- ---------------------------------------------------------------------

mcm :: Int -> Int -> Int
mcm x y = primerComun [x*k | k <- [1..]] [y*k | k <- [1..]]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Consideremos el TAD de los polinomios visto en
-- clase. Como ejemplo, tomemos el polinomio x^3 + 3.0*x^2 - 1.0*x - 2.0, 
-- definido por 
--    ejPol :: Polinomio Float
--    ejPol = consPol 3 1 
--                     (consPol 2 3 
--                              (consPol 1 (-1) 
--                                       (consPol 0 (-2) polCero)))
-- 
-- Definir la función 
--    integral :: Polinomio Float -> Polinomio Float
-- tal que (integral p) es la integral del polinomio p. Por ejemplo,
--    integral ejPol == 0.25*x^4 + x^3 + -0.5*x^2 -2.0*x         
-- ---------------------------------------------------------------------

ejPol :: Polinomio Float
ejPol = consPol 3 1 
                 (consPol 2 3 
                          (consPol 1 (-1) 
                                   (consPol 0 (-2) polCero)))
integral :: Polinomio Float -> Polinomio Float
integral p 
    | esPolCero p = polCero
    | otherwise   = consPol (n+1) (b/fromIntegral (n+1)) (integral r)
    where n = grado p
          b = coefLider p
          r = restoPol p

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función 
--    integralDef :: Polinomio Float -> Float-> Float -> Float          
-- tal que (integralDef p a b) es el valor de la integral definida
-- de p entre a y b. Por ejemplo,
--    integralDef ejPol 1 4 == 113.25
-- ---------------------------------------------------------------------

integralDef :: Polinomio Float -> Float -> Float -> Float          
integralDef p a b = valor q b - valor q a
    where q = integral p

-- ---------------------------------------------------------------------
-- Ejercicio 4. El método de la bisección para calcular un cero de una
-- función en el intervalo [a,b] se basa en el teorema de Bolzano:  
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

