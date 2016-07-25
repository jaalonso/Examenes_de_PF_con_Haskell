-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 5º examen de evaluación continua (28 de marzo de 2012)
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck
import PolRepTDA

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. La moda estadística se define como el valor (o los
-- valores) con una mayor frecuencia en una lista de datos. 
-- 
-- Definir la función 
--    moda :: [Int] -> [Int]
-- tal que (moda ns) es la lista de elementos de xs con mayor frecuencia
-- absoluta de aparición en xs. Por ejemplo,
--    moda [1,2,3,2,3,3,3,1,1,1] == [1,3]
--    moda [1,2,2,3,2]           == [2]
--    moda [1,2,3]               == [1,2,3]
--    moda []                    == []
-- ---------------------------------------------------------------------

moda :: [Int] -> [Int]
moda xs = nub [x | x <- xs, ocurrencias x xs == m]
    where m = maximum [ocurrencias x xs | x <-xs]

-- (ocurrencias x xs) es el número de ocurrencias de x en xs. Por
-- ejemplo,
--    ocurrencias 1 [1,2,3,4,3,2,3,1,4] == 2
ocurrencias :: Int -> [Int] -> Int
ocurrencias x xs = length [y | y <- xs, x == y]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que los elementos de
-- (moda xs) pertenecen a xs.
-- ---------------------------------------------------------------------   

-- La propiedad es
prop_moda_pertenece :: [Int] -> Bool
prop_moda_pertenece xs = and [x `elem` xs | x <- moda xs]

-- La comprobación es
--    ghci> quickCheck prop_moda_pertenece
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comprobar con QuickCheck que para cualquier elemento
-- de xs que no pertenezca a (moda xs), la cantidad de veces que aparece
-- x en xs es estrictamente menor que la cantidad de veces que aparece
-- el valor de la moda (para cualquier valor de la lista de elementos de
-- la moda).
-- ---------------------------------------------------------------------

-- La propiedad es
prop_modas_resto_menores :: [Int] -> Bool
prop_modas_resto_menores xs =
    and [ocurrencias x xs < ocurrencias m xs | 
         x <- xs, 
         x `notElem` ys,
         m <- ys]
    where ys = moda xs

-- La comprobación es
--    ghci> quickCheck prop_modas_resto_menores
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Representaremos un recorrido como una secuencia de
-- puntos en el espacio de dos dimensiones. Para ello utilizaremos la
-- siguiente definición 
--    data Recorrido = Nodo Double Double Recorrido
--                   | Fin
--                     deriving Show
-- De esta forma, el recorrido que parte del punto (0,0) pasa por el
-- punto (1,2) y termina en el (2,4) se representará como 
--    rec0 :: Recorrido
--    rec0 = Nodo 0 0 (Nodo 1 2 (Nodo 2 4 Fin))
--    A continuación se muestran otros ejemplos definidos
--    rec1, rec2, rec3, rec4 :: Recorrido
--    rec1 = Nodo 0 0 (Nodo 1 1 Fin)
--    rec2 = Fin
--    rec3 = Nodo 1 (-1) (Nodo 2 3 (Nodo 5 (-2) (Nodo 1 0 Fin)))
--    rec4 = Nodo 0 0 
--            (Nodo 0 2 
--              (Nodo 2 0 
--                (Nodo 0 0 
--                  (Nodo 2 2 
--                    (Nodo 2 0 
--                      (Nodo 0 0 Fin))))))
--
-- Definir la función 
--    distanciaRecorrido :: Recorrido -> Double
-- tal que (distanciaRecorrido ps) esla suma de las distancias de todos
-- los segmentos de un recorrido ps. Por ejemplo, 
--    distanciaRecorrido rec0     ==  4.4721359549995
--    distanciaRecorrido rec1     ==  1.4142135623730951
--    distanciaRecorrido rec2     ==  0.0
-- ---------------------------------------------------------------------

data Recorrido = Nodo Double Double Recorrido
               | Fin
                 deriving Show

rec0, rec1, rec2, rec3, rec4 :: Recorrido
rec0 = Nodo 0 0 (Nodo 1 2 (Nodo 2 4 Fin))
rec1 = Nodo 0 0 (Nodo 1 1 Fin)
rec2 = Fin
rec3 = Nodo 1 (-1) (Nodo 2 3 (Nodo 5 (-2) (Nodo 1 0 Fin)))
rec4 = Nodo 0 0 
        (Nodo 0 2 
          (Nodo 2 0 
            (Nodo 0 0 
              (Nodo 2 2 
                (Nodo 2 0 
                  (Nodo 0 0 Fin))))))

distanciaRecorrido :: Recorrido -> Double
distanciaRecorrido Fin = 0
distanciaRecorrido (Nodo _ _ Fin) = 0
distanciaRecorrido (Nodo x y r@(Nodo x' y' n)) =
    distancia (x,y) (x',y') + distanciaRecorrido r

-- (distancia p q) es la distancia del punto p al q. Por ejemplo, 
--    distancia (0,0) (3,4)  ==  5.0
distancia :: (Double,Double) -> (Double,Double) -> Double
distancia (x,y) (x',y') =
    sqrt ((x-x')^2 + (y-y')^2)

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función 
--    nodosDuplicados :: Recorrido -> Int
-- tal que (nodosDuplicados e) es el número de nodos por los que el
-- recorrido r pasa dos o más veces. Por ejemplo,
--    nodosDuplicados rec3 == 0
--    nodosDuplicados rec4 == 2
-- ---------------------------------------------------------------------

nodosDuplicados :: Recorrido -> Int
nodosDuplicados Fin = 0
nodosDuplicados (Nodo x y r) 
    | existeNodo r x y = 1 + nodosDuplicados (eliminaNodo r x y)
    | otherwise        = nodosDuplicados r

-- (existeNodo r x y) se verifica si el nodo (x,y) está en el recorrido
-- r. Por ejemplo,
--    existeNodo rec3 2 3  ==  True
--    existeNodo rec3 3 2  ==  False
existeNodo :: Recorrido -> Double -> Double -> Bool
existeNodo Fin _ _ = False
existeNodo (Nodo x y r) x' y'
           | x == x' && y == y' = True
           | otherwise          = existeNodo r x' y'

-- (eliminaNodo r x y) es el recorrido obtenido eliminando en r las
-- ocurrencias del nodo (x,y). Por ejemplo,
--    ghci> rec3
--    Nodo 1.0 (-1.0) (Nodo 2.0 3.0 (Nodo 5.0 (-2.0) (Nodo 1.0 0.0 Fin)))
--    ghci> eliminaNodo rec3 2 3
--    Nodo 1.0 (-1.0) (Nodo 5.0 (-2.0) (Nodo 1.0 0.0 Fin))
eliminaNodo :: Recorrido -> Double -> Double -> Recorrido
eliminaNodo Fin _ _ = Fin
eliminaNodo (Nodo x y r) x' y' 
            | x == x' && y == y' = eliminaNodo r x' y'
            | otherwise          = Nodo x y (eliminaNodo r x' y')

-- ---------------------------------------------------------------------
-- Ejercicio 3. Se dice que un polinomio es completo si todos los
-- coeficientes desde el término nulo hasta el término de mayor grado
-- son distintos de cero.
--
-- Para hacer este ejercicio se utilizará algunas de las
-- implementaciones del tipo abstracto de datos de polinomio definidas
-- en el tema 21 y los siguientes ejemplos,
--    pol1, pol2, pol3 :: Polinomio Int
--    pol1 = polCero
--    pol2 = consPol 5 2 (consPol 3 1 (consPol 0 (-1) polCero))
--    pol3 = consPol 3 1 (consPol 2 2 (consPol 1 3 (consPol 0 4 polCero)))
-- 
-- Definir la función 
--    polinomioCompleto :: Num a => Polinomio a -> Bool
-- tal que (polinomioCompleto p) se verifica si p es un polinomio
-- completo. Por ejemplo,
--    polinomioCompleto pol1  == False
--    polinomioCompleto pol2  == False
--    polinomioCompleto pol3  == True
---------------------------------------------------

pol1, pol2, pol3 :: Polinomio Int
pol1 = polCero
pol2 = consPol 5 2 (consPol 3 1 (consPol 0 (-1) polCero))
pol3 = consPol 3 1 (consPol 2 2 (consPol 1 3 (consPol 0 4 polCero)))

polinomioCompleto :: Num a => Polinomio a -> Bool
polinomioCompleto p = 0 `notElem` coeficientes p 

-- (coeficientes p) es la lista de los coeficientes de p. Por ejemplo,
--    coeficientes pol1  ==  [0]
--    coeficientes pol2  ==  [2,0,1,0,0,-1]
--    coeficientes pol3  ==  [1,2,3,4]
coeficientes :: Num a => Polinomio a -> [a]
coeficientes p = [coeficiente n p | n <- [g,g-1..0]]
    where g = grado p

-- (coeficiente k p) es el coeficiente del término de grado k
-- del polinomio p. Por ejemplo,
--    pol2                ==  2*x^5 + x^3 + -1
--    coeficiente 5 pol2  ==  2
--    coeficiente 6 pol2  ==  0
--    coeficiente 4 pol2  ==  0
--    coeficiente 3 pol2  ==  1
coeficiente :: Num a => Int -> Polinomio a -> a
coeficiente k p | k > n     = 0
                | k == n    = c
                | otherwise = coeficiente k r
                where n = grado p
                      c = coefLider p
                      r = restoPol p
