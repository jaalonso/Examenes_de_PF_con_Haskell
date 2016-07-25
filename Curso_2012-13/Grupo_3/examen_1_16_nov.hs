-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 1º examen de evaluación continua (15 de noviembre de 2012)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función numeroPrimos, donde (numeroPrimos m n)
-- es la cantidad de número primos entre 2^m y 2^n. Por ejemplo, 
--   numerosPrimos 2 6   == 16
--   numerosPrimos 2 7   == 29
--   numerosPrimos 10 12 == 392
-- ---------------------------------------------------------------------

numerosPrimos:: Int -> Int -> Int
numerosPrimos m n = length [x | x <- [2^m..2^n], primo x]

-- (primo x) se verifica si x es primo. Por ejemplo,
--    primo 30  == False
--    primo 31  == True  
primo n = factores n == [1, n]

-- (factores n) es la lista de los factores del número n. Por ejemplo,
--    factores 30  ==  [1,2,3,5,6,10,15,30]  
factores n = [x | x <- [1..n], n `rem` x == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función masOcurrentes tal que
-- (masOcurrentes xs) es la lista de los elementos de xs que ocurren el
-- máximo número de veces. Por ejemplo,
--    masOcurrentes [1,2,3,4,3,2,3,1,4] == [3,3,3]
--    masOcurrentes [1,2,3,4,5,2,3,1,4] == [1,2,3,4,2,3,1,4]
--    masOcurrentes "Salamanca"         == "aaaa"
-- ---------------------------------------------------------------------

masOcurrentes xs = [x | x <- xs, ocurrencias x xs == m]
    where m = maximum [ocurrencias x xs | x <-xs]

-- (ocurrencias x xs) es el número de ocurrencias de x en xs. Por
-- ejemplo, 
--    ocurrencias 1 [1,2,3,4,3,2,3,1,4]  ==  2
ocurrencias x xs = length [x' | x' <- xs, x == x']  

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. En este esjercicio se consideran listas de ternas de
-- la forma (nombre, edad, población). 
-- 
-- Definir la función puedenVotar tal que (puedenVotar t) es la
-- lista de las personas de t que tienen edad para votar. Por ejemplo,
--    ghci> :{
--    *Main| puedenVotar [("Ana", 16, "Sevilla"), ("Juan", 21, "Coria"), 
--    *Main|              ("Alba", 19, "Camas"), ("Pedro",18,"Sevilla")]
--    *Main| :}
--    ["Juan","Alba","Pedro"]
-- ---------------------------------------------------------------------

puedenVotar t = [x | (x,y,_) <- t, y >= 18]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función puedenVotarEn tal que (puedenVotar
-- t p) es la lista de las personas de t que pueden votar en la
-- población p. Por ejemplo, 
--    ghci> :{
--    *Main| puedenVotarEn [("Ana", 16, "Sevilla"), ("Juan", 21, "Coria"), 
--    *Main|                ("Alba", 19, "Camas"),("Pedro",18,"Sevilla")] 
--    *Main|               "Sevilla"
--    *Main| :}
--    ["Pedro"]
-- ---------------------------------------------------------------------

puedenVotarEn t c = [x | (x,y,z) <- t, y >= 18, z == c]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Dos listas xs, ys de la misma longitud son
-- perpendiculares si el producto escalar de ambas es 0, donde el
-- producto escalar de dos listas de enteros xs e ys viene
-- dado por la suma de los productos de los elementos correspondientes.
-- 
-- Definir la función perpendiculares tal que (perpendiculares xs yss)
-- es la lista de los elementos de yss que son perpendiculares a xs.
-- Por ejemplo,
--    ghci> perpendiculares [1,0,1] [[0,1,0], [2,3,1], [-1,7,1],[3,1,0]]
--    [[0,1,0],[-1,7,1]]
-- ---------------------------------------------------------------------

perpendiculares xs yss = [ys | ys <-yss, productoEscalar xs ys == 0]

-- (productoEscalar xs ys) es el producto escalar de xs por ys. Por
-- ejemplo, 
--    productoEscalar [2,3,5] [6,0,2]  ==  22
productoEscalar xs ys = sum [x*y | (x,y) <- zip xs ys]
