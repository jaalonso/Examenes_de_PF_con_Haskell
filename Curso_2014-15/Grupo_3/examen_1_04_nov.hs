-- Informática (1º del Grado en Matemáticas)
-- 1º examen de evaluación continua (4 de noviembre de 2014)
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, usando listas por comprensión, la función
--      mulPosC :: Int ->  [Int] -> [Int]
-- tal que (mulPosC x xs) es la lista de los elementos de xs que son 
-- múltiplos positivos de x. Por ejemplo,
--    mulPosC 3 [1,6,-5,-9,33] == [6,33]
-- ---------------------------------------------------------------------

mulPosC :: Int -> [Int] -> [Int]
mulPosC x xs = [y | y <- xs, y > 0, rem y x == 0]  

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por recursión, la función
--    mulPosR :: Int -> [Int] -> [Int]
-- tal que (mulPosR x xs) es la lista de los elementos de xs que son 
-- múltiplos positivos de x. Por ejemplo,
--    mulPosR 3 [1,6,-5,-9,33] == [6,33]
-- ---------------------------------------------------------------------

mulPosR :: Int -> [Int] -> [Int]
mulPosR _ [] = []
mulPosR x (y:ys) | y > 0 && rem y x == 0 = y : mulPosR x ys
                 | otherwise             = mulPosR x ys

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Diremos que una lista numérica es muy creciente si
-- cada elemento es mayor estricto que el doble del anterior. 
-- 
-- Definir el predicado 
--    muyCreciente :: (Ord a, Num a) => [a] -> Bool
-- tal que (muyCreciente xs) se verifica si xs es una lista muy
-- creciente. Por ejemplo,
--    muyCreciente [3,7,100,220] == True
--    muyCreciente [1,5,7,1000]  == False
-- ---------------------------------------------------------------------

muyCreciente :: (Ord a, Num a) => [a] -> Bool
muyCreciente xs = and [y > 2*x | (x,y) <- zip xs (tail xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Para generar listas muy crecientes, consideramos la
-- función 
--    f :: Integer -> Integer -> Integer
-- dada por las ecuaciones recursivas:
--    f(x,0) = x
--    f(x,n) = 2*f(x,n-1) + 1, si n > 0
-- 
--  Definir la función 
--    lista :: Int -> Integer -> [Integer]
-- tal que (lista n x) es la lista [f(x,0),f(x,1),...,f(x,n)]
-- Por ejemplo,
--    lista 5 4 == [4,9,19,39,79]
-- ---------------------------------------------------------------------

f :: Integer -> Integer -> Integer
f x 0 = x
f x n = 2 * f x (n-1) + 1

lista :: Int -> Integer -> [Integer]
lista n x = take n [f x i | i <- [0..]]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Representamos un conjunto de n masas en el plano
-- mediante una lista de n pares de la forma ((ai,bi),mi) donde (ai,bi)
-- es la posición y mi es la masa puntual.
-- 
-- Definir la función
--    masaTotal ::  [((Float,Float),Float)] -> Float 
-- tal que (masaTotal xs) es la masa total del conjunto xs. Por ejemplo, 
--    masaTotal  [((-1,3),2),((0,0),5),((1,4),3)] == 10.0
-- ---------------------------------------------------------------------

masaTotal :: [((Float,Float),Float)] -> Float
masaTotal xs = sum [m | (_,m) <- xs]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Se define el diámetro de un conjunto de puntos del
-- plano como la mayor distancia entre dos puntos del conjunto. 
-- 
-- Definir la función
--    diametro :: [[(Float,Float),Float)] -> Float
-- tal que (diametro xs) es el diámetro del conjunto de masas xs. Por
-- ejemplo, 
--    diametro  [((-1,3),2),((0,0),5),((1,4),3)] == 4.1231055
-- ---------------------------------------------------------------------

diametro :: [((Float,Float),Float)] -> Float
diametro xs = maximum [dist p q | (p,_) <- xs, (q,_) <- xs] 
    where dist (a,b) (c,d) = sqrt ((a-c)^2+(b-d)^2)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Se define el grado de similitud entre dos cadenas de
-- texto como la menor posición en la que ambas cadenas difieren, o bien
-- como la longitud de la cadena menor si una cadena es una segmento
-- inicial de la otra. 
--  
-- Definir la función:
--    grado :: String -> String -> Int
-- tal que (grado xs ys) es el grado de similitud entre las cadenas xs e ys.
-- Por ejemplo,
--    grado "cadiz" "calamar" == 2
--    grado "sevilla" "betis" == 0
--    grado "pi" "pitagoras"  == 2
-- ---------------------------------------------------------------------

-- 1ª definición:
grado :: String -> String -> Int 
grado xs ys | null zs   = min (length xs) (length ys)
            | otherwise = head zs
    where zs = [i | ((x,y),i) <- zip (zip xs ys) [0..], x /= y] 

-- 2ª definición:
grado2 :: String -> String -> Int 
grado2 xs ys = length (takeWhile (\(x,y) -> x == y) (zip xs ys))
