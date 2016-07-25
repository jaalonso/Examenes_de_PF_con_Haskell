-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 6º examen de evaluación continua (21 de junio de 2010)
-- ---------------------------------------------------------------------

import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    calculaPi :: Int -> Double
-- tal que (calculaPi n) es la aproximación del número pi calculada
-- mediante la expresión
--    4*(1 - 1/3 + 1/5 - 1/7 ... 1/(2*n+1))
-- Por ejemplo,
--    calculaPi 3    ==  2.8952380952380956
--    calculaPi 300  ==  3.1449149035588526
-- Indicación: La potencia es **, por ejemplo 2**3 es 8.0.
-- ---------------------------------------------------------------------

calculaPi :: Int -> Double
calculaPi n = 4 * sum [(-1)**x/(2*x+1) | x <- [0..fromIntegral n]]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. En la Olimpiada de Matemática del 2010 se planteó el
-- siguiente problema: 
--   Una sucesión pucelana es una sucesión creciente de 16 números
--   impares positivos consecutivos, cuya suma es un cubo perfecto. 
--   ¿Cuántas sucesiones pucelanas tienen solamente números de tres 
--   cifras? 
-- 
-- Definir la función
--    pucelanas :: [[Int]]
-- tal que pucelanas es la lista de las sucesiones pucelanas. Por
-- ejemplo, 
--    ghci> head pucelanas
--    [17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47]
-- ---------------------------------------------------------------------

pucelanas :: [[Int]]
pucelanas = [[x,x+2..x+30] | x <- [1..],
                             esCubo (sum [x,x+2..x+30])]

-- (esCubo n) se verifica si n es un cubo. Por ejemplo,
--    esCubo 27  ==  True
--    esCubo 28  ==  False
esCubo x = y^3== x
    where y = ceiling (fromIntegral x ** (1/3))

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    pucelanasConNcifras :: Int -> [[Int]]
-- tal que (pucelanasConNcifras n) es la lista de las sucesiones
-- pucelanas que tienen sólamente números de n cifras. Por ejemplo,
--    ghci> pucelanasConNcifras 2
--    [[17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47]]
-- ---------------------------------------------------------------------

pucelanasConNcifras :: Int -> [[Int]]
pucelanasConNcifras n = [[x,x+2..x+30] | x <- [10^(n-1)+1..10^n-31],
                                         esCubo (sum [x,x+2..x+30])]

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Calcular cuántas sucesiones pucelanas tienen solamente
-- números de tres cifras.
-- ---------------------------------------------------------------------

-- El cálculo es
--    ghci> length (pucelanasConNcifras 3)
--    3

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    inflexion :: Ord a => [a] -> Maybe a
-- tal que (inflexion xs) es el primer elemento de la lista en donde se
-- cambia de creciente a decreciente o de decreciente a creciente y
-- Nothing si no se cambia. Por ejemplo,
--    inflexion [2,2,3,5,4,6]    ==  Just 4
--    inflexion [9,8,6,7,10,10]  ==  Just 7
--    inflexion [2,2,3,5]        ==  Nothing
--    inflexion [5,3,2,2]        ==  Nothing
-- ---------------------------------------------------------------------

inflexion :: Ord a => [a] -> Maybe a
inflexion (x:y:zs) 
    | x <  y = decreciente (y:zs)
    | x == y = inflexion (y:zs)
    | x >  y = creciente (y:zs)
inflexion _  = Nothing

-- (creciente xs) es el segundo elemento de la primera parte creciente
-- de xs y Nothing, en caso contrario. Por ejemplo,
--    creciente [4,3,5,6]  ==  Just 5
--    creciente [4,3,5,2,7]  ==  Just 5
--    creciente [4,3,2]  ==  Nothing
creciente (x:y:zs) 
    | x <  y    = Just y
    | otherwise = creciente (y:zs)
creciente _     = Nothing

-- (decreciente xs) es el segundo elemento de la primera parte
-- decreciente de xs y Nothing, en caso contrario. Por ejemplo,
--    decreciente [4,2,3,1,0]  ==  Just 2
--    decreciente [4,5,3,1,0]  ==  Just 3
--    decreciente [4,5,7]      ==  Nothing
decreciente (x:y:zs) 
    | x >  y    = Just y
    | otherwise = decreciente (y:zs)
decreciente _   = Nothing

