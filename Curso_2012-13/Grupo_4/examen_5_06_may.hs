-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 5º examen de evaluación continua (6 de mayo de 2013)
-- ---------------------------------------------------------------------

import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por recursión, la función
--    borra :: Eq a => a -> [a] -> [a]
-- tal que (borra x xs) es la lista obtenida borrando la primera
-- ocurrencia del elemento x en la lista xs. Por ejemplo,
--    borra 'a' "salamanca" == "slamanca"
-- ---------------------------------------------------------------------

borra :: Eq a => a -> [a] -> [a]
borra _ [] = []
borra x (y:ys) | x == y    = ys 
               | otherwise = y : borra x ys
                             
-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, por recursión, la función
--    borraTodos :: Eq a => a -> [a] -> [a]
-- tal que (borraTodos x xs) es la lista obtenida borrando todas las
-- ocurrencias de x en la lista xs. Por ejemplo,
--    borraTodos 'a' "salamanca" == "slmnc"
-- ---------------------------------------------------------------------

borraTodos :: Eq a => a -> [a] -> [a]
borraTodos _ [] = []
borraTodos x (y:ys) | x == y    = borraTodos x ys 
                    | otherwise = y : borraTodos x ys

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir, por plegado, la función
--    borraTodosP :: Eq a => a -> [a] -> [a]
-- tal que (borraTodosP x xs) es la lista obtenida borrando todas las
-- ocurrencias de x en la lista xs. Por ejemplo,
--    borraTodosP 'a' "salamanca" == "slmnc"
-- ---------------------------------------------------------------------

borraTodosP :: Eq a => a -> [a] -> [a]
borraTodosP x = foldr f []
    where f y ys | x == y    = ys
                 | otherwise = y:ys

-- usando funciones anónimas la definición es
borraTodosP' :: Eq a => a -> [a] -> [a]
borraTodosP' x = foldr (\ y z -> if x == y then z else (y:z)) []

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Definir, por recursión, la función
--    borraN :: Eq a => Int -> a -> [a] -> [a]
-- tal que (borraN n x xs) es la lista obtenida borrando las n primeras
-- ocurrencias de x en la lista xs. Por ejemplo,
--    borraN 3 'a' "salamanca" == "slmnca"
-- --------------------------------------------------------------------- 

borraN :: Eq a => Int -> a -> [a] -> [a]                             
borraN _ _ [] = []
borraN 0 _ xs = xs
borraN n x (y:ys) | x == y    = borraN (n-1) x ys
                  | otherwise = y : borraN n x ys

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Un número entero positivo x se dirá especial si puede
-- reconstruirse a partir de las cifras de sus factores primos; es decir
-- si el conjunto de sus cifras es igual que la unión de las cifras de
-- sus factores primos. Por ejemplo, 11913 es especial porque sus cifras
-- son [1,1,1,3,9] y sus factores primos son: 3, 11 y 19. 
-- 
-- Definir la función  
--    esEspecial :: Int -> Bool
-- tal que (esEspecial x) se verifica si x es especial. Por ejemplo,
--    ???
-- Calcular el menor entero positivo especial que no sea un número
-- primo. 
-- ---------------------------------------------------------------------

esEspecial :: Int -> Bool
esEspecial x = 
    sort (cifras x) == sort (concat [cifras n | n <- factoresPrimos x])

-- (cifras x) es la lista de las cifras de x. Por ejemplo,
--    cifras 11913  ==  [1,1,9,1,3]
cifras :: Int -> [Int]
cifras x = [read [i] |  i <- show x] 

-- (factoresPrimos x) es la lista de los factores primos de x. Por ejemplo,
--    factoresPrimos 11913  ==  [3,11,19]
factoresPrimos :: Int -> [Int]
factoresPrimos x = filter primo (factores x)

-- (factores x) es la lista de los factores de x. Por ejemplo,
--    ghci> factores 11913
--    [1,3,11,19,33,57,209,361,627,1083,3971,11913]
factores :: Int -> [Int]
factores x = [i | i <- [1..x], mod x i == 0]

-- (primo x) se verifica si x es primo. Por ejemplo,
--    primo 7  ==  True
--    primo 9  ==  False
primo :: Int -> Bool
primo x = factores x == [1,x]

-- El cálculo es
--    ghci> head [x | x <- [1..], esEspecial x, not (primo x)] 
--    735

-- ---------------------------------------------------------------------
-- Ejercicio 3. Una lista de listas de xss se dirá encadenada si el
-- último elemento de cada lista de xss coincide con el primero de la
-- lista siguiente. Por ejemplo, [[1,2,3],[3,4],[4,7]] está encadenada. 
-- 
-- Definir la función 
--    encadenadas :: Eq a => [[a]] -> [[[a]]]
-- tal que (encadenadas xss) es la lista de las permutaciones de xss que
-- son encadenadas. Por ejemplo,
--    ghci> encadenadas ["el","leon","ruge","nicanor"]
--    [["ruge","el","leon","nicanor"],
--     ["leon","nicanor","ruge","el"],
--     ["el","leon","nicanor","ruge"],
--     ["nicanor","ruge","el","leon"]]
-- ---------------------------------------------------------------------

encadenadas :: Eq a => [[a]] -> [[[a]]]
encadenadas xss = filter encadenada (permutations xss)

encadenada :: Eq a => [[a]] -> Bool
encadenada xss = and [last xs == head ys | (xs,ys) <-  zip xss (tail xss)]
 
-- ---------------------------------------------------------------------
-- Ejercicio 4. Representamos los polinomios de una variable mediante un
-- tipo algebraico de datos como en el tema 21 de la asignatura:
--    data Polinomio a = PolCero | ConsPol Int a (Polinomio a)
-- Por ejemplo, el polinomio x^3 + 4x^2 + x - 6  se representa por
--    ej :: Polinomio Int
--    ej = ConsPol 3 1 (ConsPol 2 4 (ConsPol 1 1 (ConsPol 0 (-6) PolCero)))
-- 
-- Diremos que un polinomio es propio si su término independiente es no
-- nulo.  
-- 
-- Definir la función
--    raices :: Polinomio Int -> [Int] 
-- tal que (raices p) es la lista de todas las raíces enteras del
-- polinomio propio p. Por ejemplo,
--    raices ej == [1,-2,-3]
-- ---------------------------------------------------------------------

data Polinomio a = PolCero | ConsPol Int a (Polinomio a)

ej :: Polinomio Int
ej = ConsPol 3 1 (ConsPol 2 4 (ConsPol 1 1 (ConsPol 0 (-6) PolCero)))

raices :: Polinomio Int -> [Int]
raices p = [z | z <- factoresEnteros (termInd p), valor z p == 0]

-- (termInd p) es el término independiente del polinomio p. Por ejemplo, 
--    termInd (ConsPol 3 1 (ConsPol 0 5 PolCero))  ==  5
--    termInd (ConsPol 3 1 (ConsPol 2 5 PolCero))  ==  0
termInd :: Num a => Polinomio a -> a
termInd PolCero = 0
termInd (ConsPol n x p) | n == 0    = x
                        | otherwise = termInd p
                                      
-- (valor c p) es el valor del polinomio p en el punto c. Por ejemplo,
--    valor 2 (ConsPol 3 1 (ConsPol 2 5 PolCero))  ==  28
valor :: Num a => a -> Polinomio a  -> a                                      
valor _ PolCero = 0
valor z (ConsPol n x p) = x*z^n + valor z p

-- (factoresEnteros x) es la lista de los factores enteros de x. Por
-- ejemplo, 
--    factoresEnteros 12  ==  [-1,1,-2,2,-3,3,-4,4,-6,6,-12,12]
factoresEnteros :: Int -> [Int]
factoresEnteros x = concat [[-z,z] | z <- factores (abs x)]
