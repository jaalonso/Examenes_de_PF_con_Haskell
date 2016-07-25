-- Informática (1º del Grado en Matemáticas)
-- 4º examen de evaluación continua (20 de marzo de 2014)
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.Ratio
import Data.List
import PolOperaciones

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Consideremos la sucesión siguiente
--     a0 = 1
--     a1 = 1
--     an = 7*a(n-1) - a(n-2) - 2
-- 
-- Definir, por recursión, la función
--     suc :: Integer -> Integer  
-- tal que (suc n) es el n-ésimo término de la sucesión anterior. Por
-- ejemplo, 
--     suc 1 == 1
--     suc 4 == 169
--     suc 8 == 372100
-- Por ejemplo,
--    ghci> [suc n | n <-[0..10]]
--    [1,1,4,25,169,1156,7921,54289,372100,2550409,17480761]
-- ---------------------------------------------------------------------

suc :: Integer -> Integer  
suc 0 = 1
suc 1 = 1
suc n = 7*(suc (n-1)) - (suc (n-2)) - 2

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, usando evaluación perezosa. la función
--     suc' :: Integer -> Integer  
-- tal que (suc n) es el n-ésimo término de la sucesión anterior. Por
-- ejemplo, 
--     suc 1 == 1
--     suc 4 == 169
--     suc 8 == 372100
-- Por ejemplo,
--    ghci> [suc n | n <-[0..10]]
--    [1,1,4,25,169,1156,7921,54289,372100,2550409,17480761]
--    ghci> suc' 30
--    915317035111995882133681
-- ---------------------------------------------------------------------

-- La sucesión es
sucesion:: [Integer]
sucesion = 1:1:zipWith f (tail sucesion) sucesion 
    where f x y = 7*x-y-2

-- Por ejemplo, el cálculo de los 4 primeros términos es
--    take 4 sucesion
--    = take 4 (1:1:zipWith f (tail sucesion) sucesion)
--    = 1:take 3 (1:zipWith f (tail sucesion) sucesion)
--    = 1:1:take 2 (zipWith f (tail sucesion) sucesion)
--    = 1:1:take 2 (zipWith f (1:R2) (1:1:R2))
--    = 1:1:take 2 (4:zipWith f R2 (1:R2))
--    = 1:1:4:take 1 (zipWith f (4:R3) (1:4:R3))
--    = 1:1:4:take 1 (25:zipWith f R3 (4:R3))
--    = 1:1:4:25:take 0 (25:zipWith f R3 (4:R3))
--    = 1:1:4:25:[]
--    = [1,1,4,25]
        
suc' :: Integer -> Integer        
suc' n = sucesion `genericIndex` n        

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Calcular el término 100 de la sucesión anterior.
-- ---------------------------------------------------------------------

-- El cálculo es
--    ghci> suc' 100
--    30068434349082593880211806294947596752920546625789181082505523070321286807046578601

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Comprobar que los primeros 30 términos de la sucesión
-- son cuadrados perfectos.
-- ---------------------------------------------------------------------

esCuadrado :: (Integral a) => a -> Bool
esCuadrado n = y*y == n
    where y = floor (sqrt (fromIntegral n))

-- La comprobación es        
--    ghci> and [esCuadrado (suc' n) | n <-[0..30]]
--    True

-- ---------------------------------------------------------------------
-- Ejercicio 2. Consideremos los árboles binarios definidos por el tipo
-- siguiente: 
--    data Arbol t = Hoja t 
--               | Nodo (Arbol t) t (Arbol t)
--               deriving (Show, Eq)
-- y el siguiente ejemplo de árbol   
--    ejArbol :: Arbol Int
--    ejArbol = Nodo (Nodo (Hoja 1) 3 (Hoja 4)) 
--                   5 
--                   (Nodo (Hoja 6) 7 (Hoja 9))
-- 
-- Definir la función 
--    transforma :: (t -> Bool) -> Arbol t -> Arbol (Maybe t)
-- tal que (transforma p a) es el árbol con la misma estructura que a,
-- en el que cada elemento x que verifica el predicado p se sustituye por
-- (Just x) y los que no lo verifican se sustituyen por Nothing. Por
-- ejemplo,
--    ghci> transforma even ejArbol
--    Nodo (Nodo (Hoja Nothing) Nothing (Hoja (Just 4))) 
--         Nothing 
--         (Nodo (Hoja (Just 6)) Nothing (Hoja Nothing))
-- ---------------------------------------------------------------------

data Arbol t = Hoja t 
           | Nodo (Arbol t) t (Arbol t)
           deriving (Show, Eq)

ejArbol :: Arbol Int
ejArbol = Nodo (Nodo (Hoja 1) 3 (Hoja 4)) 
               5 
               (Nodo (Hoja 6) 7 (Hoja 9))

transforma :: (t -> Bool) -> Arbol t -> Arbol (Maybe t)
transforma p (Hoja r) | p r       = Hoja (Just r)
                      | otherwise = Hoja Nothing
transforma p (Nodo i r d) 
    | p r       = Nodo (transforma p i) (Just r) (transforma p d)
    | otherwise = Nodo (transforma p i) Nothing (transforma p d)

-- ---------------------------------------------------------------------
-- Ejercicio 3. El método de la bisección para calcular un cero de una
-- función en el intervalo [a,b] se basa en el Teorema de Bolzano: "Si
-- f(x) es una función continua en el intervalo [a, b], y si, además, en
-- los  extremos del intervalo la función f(x) toma valores de signo
-- opuesto (f(a) * f(b) < 0), entonces existe al menos un valor c en (a,
-- b) para el que f(c) = 0".
-- 
-- La idea es tomar el punto medio del intervalo c = (a+b)/2 y
-- considerar los siguientes casos:
-- (*) Si f(c) ~= 0, hemos encontrado una aproximación del punto que
--     anula f en el intervalo con un error aceptable.
-- (*) Si f(c) tiene signo distinto de f(a), repetir el proceso en el
--     intervalo [a,c].
-- (*) Si no, repetir el proceso en el intervalo [c,b].
-- 
-- Definir la función
--    ceroBiseccionE :: (Double -> Double) ->
--                      Double -> Double -> Double -> Double
-- tal que (ceroBiseccionE f a b e) calcule una aproximación del punto 
-- del intervalo [a,b] en el que se anula la función f, con un error
-- menor que e, aplicando el método de la bisección. Por ejemplo,
-- si f1 y f2 son las funciones definidas por
--    f1 x = 2 - x
--    f2 x = x^2 - 3 
-- entonces
--    ceroBiseccionE f1 0 3 0.0001     == 2.00006103515625
--    ceroBiseccionE f2 0 2 0.0001     == 1.7320556640625
--    ceroBiseccionE f2 (-2) 2 0.00001 == -1.732048
--    ceroBiseccionE cos 0 2 0.0001    == -1.7320480346679688
-- ---------------------------------------------------------------------

f1 x = 2 - x
f2 x = x^2 - 3 

ceroBiseccionE :: (Double -> Double) -> 
                  Double -> Double -> Double -> Double
ceroBiseccionE f a b e = aux a b
    where aux c d | aceptable m     = m
                  | (f c)*(f m) < 0 = aux c m
                  | otherwise       = aux m d
              where m = (c+d)/2
                    aceptable x = abs (f x) < e 

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Los polinomios de Fibonacci se definen como sigue
--    P_0 = 0                    
--    P_1 = 1                    
--    P_n = x*P_(n-1) + P_(n-2)                    
--                    
-- Definir la función 
--    polFibonnaci :: Integer -> Polinomio Rational
-- tal que (polFibonnaci n) es el n-ésimo polinomio de Fibonacci. Por
-- ejemplo, 
--    polFibonnaci 2 == 1 % 1*x
--    polFibonnaci 3 == x^2 + 1 % 1
--    polFibonnaci 4 == x^3 + 2 % 1*x
--    polFibonnaci 5 == x^4 + 3 % 1*x^2 + 1 % 1
-- ---------------------------------------------------------------------   
                    
-- 1ª solución (por recursión)
polFibonnaci :: Integer -> Polinomio Rational
polFibonnaci 0 = polCero
polFibonnaci 1 = polUnidad
polFibonnaci n = 
    sumaPol (multPol (creaPolDispersa [1,0]) (polFibonnaci (n-1)))
            (polFibonnaci (n-2))

-- 2ª solución (evaluación perezosa)
polFibonnaciP :: Integer -> Polinomio Rational
polFibonnaciP n = sucPolinomiosFibonacci `genericIndex` n

sucPolinomiosFibonacci :: [Polinomio Rational]
sucPolinomiosFibonacci =
    polCero:polUnidad:zipWith f (tail sucPolinomiosFibonacci)
                                sucPolinomiosFibonacci
        where f p q = sumaPol (multPol (creaPolDispersa [1,0]) p) q
    
-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Comprobar que P_2 divide a los polinomios de Fibonacci
-- de índice par hasta n = 20.
-- ---------------------------------------------------------------------

divide :: (Fractional a, Eq a) => Polinomio a -> Polinomio a -> Bool
divide p q = esPolCero (resto q p)

-- La comprobación es
-- ghci> and [divide (polFibonnaciP 2) (polFibonnaciP (2*k)) | k <- [2..20]]
-- True


