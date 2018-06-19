-- Informática (1º del Grado en Matemáticas) Grupo 5
-- 5º examen de evaluación continua (7 de mayo de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Librerías auxiliares
-- ---------------------------------------------------------------------

import Data.List
import Data.Matrix
import I1M.Pila
import I1M.PolOperaciones

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Un número natural x es un cuadrado perfecto si x = b^2
-- para algún natural b. Por ejemplo, 25 es un cuadrado perfecto y 24 no
-- lo es. Un número entero positivo se dirá regular si sus cifras están
-- ordenadas, ya sea en orden creciente o en orden decreciente. Por
-- ejemplo, 11468 y 974000 son regulares y 16832 no lo es. 
-- 
-- Definir la lista infinita
--    regularesPerfectos :: [Integer]
-- cuyos elementos son los cuadrados perfectos que son regulares. Por
-- ejemplo, 
--    λ> take 19 regularesPerfectos
--    [1,4,9,16,25,36,49,64,81,100,144,169,225,256,289,400,441,841,900]
-- ---------------------------------------------------------------------

regularesPerfectos :: [Integer]
regularesPerfectos = filter regular [i^2 | i <- [1..]]

regular :: Integer -> Bool
regular x = cs == ds || cs == reverse ds
  where cs = cifras x
        ds = sort cs

cifras :: Integer -> [Int]
cifras x = [read [i] | i <- show x]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Calcula el mayor cuadrado perfecto regular de 7 cifras. 
-- ---------------------------------------------------------------------

-- El cálculo es
--    λ> last (takeWhile (<=10^7-1) regularesPerfectos)
--    9853321

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    posicionesPares :: Pila a -> Pila a
-- tal que (posicionesPares p) devuelve la pila obtenida tomando los elementos
-- que ocupan las posiciones pares en la pila p. Por ejemplo,
--    λ> posicionesPares (foldr apila vacia [0..9])
--    1|3|5|7|9|-
--    λ> posicionesPares (foldr apila vacia "salamanca")
--    'a'|'a'|'a'|'c'|-
-- ---------------------------------------------------------------------
   
posicionesPares :: Pila a -> Pila a
posicionesPares p
  | esVacia p = vacia
  | esVacia q = vacia
  | otherwise = apila (cima q) (posicionesPares r)  
  where q = desapila p
        r = desapila q

-- ---------------------------------------------------------------------
-- Ejercicio 3. Una matriz de enteros p se dirá segura para la torre si
--    + p solo contiene ceros y unos;
--    + toda fila de p contiene, a lo más, un 1;
--    + toda columna de p contiene, a lo más, un 1.
-- 
--  Definir la función
--     seguraTorre :: Matrix Int -> Bool
--  tal que (seguraTorre p) se verifica si la matriz p es segura para
--  la torre según la definición anterior. Por ejemplo,
--     λ> seguraTorre (fromLists [[0,0,1,0,0],[1,0,0,0,0],[0,0,0,0,1]])
--     True
--     λ> seguraTorre (fromLists [[0,0,0,0],[0,1,0,0],[0,0,0,1],[0,1,1,0]])
--     False
--     λ> seguraTorre (fromLists [[0,1],[2,0]])
--     False
-- ---------------------------------------------------------------------

seguraTorre :: Matrix Int -> Bool
seguraTorre p =
  all (`elem` [0,1]) (toList p) &&
  all tieneComoMaximoUnUno filas &&
  all tieneComoMaximoUnUno columnas 
  where m        = nrows p
        n        = ncols p
        filas    = [[p!(i,j) | j <- [1..n]] | i <- [1..m]]
        columnas = [[p!(i,j) | i <- [1..m]] | j <- [1..n]]

tieneComoMaximoUnUno :: [Int] -> Bool
tieneComoMaximoUnUno xs = length (filter (==1) xs) <= 1

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la función
--    trunca :: (Eq a,Num a) => Int -> Polinomio a -> Polinomio a
-- tal que (trunca k p) es el polinomio formado por aquellos términos de
-- p de grado mayor o igual que k. Por ejemplo, 
--    λ> trunca 3 (consPol 5 2 (consPol 3 (-7) (consPol 2 1 polCero)))
--    2*x^5 + -7*x^3
--    λ> trunca 2 (consPol 5 2 (consPol 3 (-7) (consPol 2 1 polCero)))
--    2*x^5 + -7*x^3 + x^2
--    λ> trunca 4 (consPol 5 2 (consPol 3 (-7) (consPol 2 1 polCero)))
--    2*x^5
-- ---------------------------------------------------------------------

trunca :: (Eq a, Num a) => Int -> Polinomio a -> Polinomio a
trunca k p
  | k < 0       = polCero
  | esPolCero p = polCero
  | k > n       = polCero
  | otherwise   = consPol n a (trunca k (restoPol p))
  where n = grado p
        a = coefLider p

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Un polinomio de enteros se dirá impar si su término
-- independiente es impar y el resto de sus coeficientes (si los
-- hubiera) son pares. 
-- 
-- Definir la función
--    imparPol :: Integral a => Polinomio a -> Bool
-- tal que (imparPol p) se verifica si p es un polinomio impar de
-- acuerdo con la definición anterior. Por ejemplo,
--    λ> imparPol (consPol 5 2 (consPol 3 6 (consPol 0 3 polCero)))
--    True
--    λ> imparPol (consPol 5 2 (consPol 3 6 (consPol 0 4 polCero)))
--    False
--    λ> imparPol (consPol 5 2 (consPol 3 1 (consPol 0 3 polCero)))
--    False
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

imparPol :: Integral a => Polinomio a -> Bool
imparPol p = odd (coeficiente 0 p) &&
             all even [coeficiente k p | k <- [1..grado p]]

coeficiente :: Integral a => Int -> Polinomio a -> a
coeficiente k p
  | esPolCero p = 0
  | k > n       = 0
  | k == n      = coefLider p
  | otherwise   = coeficiente k (restoPol p)
  where n = grado p

-- 2ª solución
-- ===========
 
imparPol2 :: Integral a => Polinomio a -> Bool
imparPol2 p =
  all even [coeficiente k (sumaPol p polUnidad) | k <- [0..grado p]]



