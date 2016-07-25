-- Informática (1º del Grado en Matemáticas)
-- 1º examen de evaluación continua (5 de noviembre de 2012)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. [2.5 puntos] Definir la función 
--    divisoresPrimos :: Integer -> [Integer]
-- tal que (divisoresPrimos x) es la lista de los divisores primos de x. 
-- Por ejemplo, 
--    divisoresPrimos 40  ==  [2,5]
--    divisoresPrimos 70  ==  [2,5,7]
-- ------------------------------------------------------------------------

divisoresPrimos :: Integer -> [Integer]
divisoresPrimos x = [n | n <- divisores x, primo n]

-- (divisores n) es la lista de los divisores del número n. Por ejemplo,
--    divisores 30  ==  [1,2,3,5,6,10,15,30]  
divisores :: Integer -> [Integer]
divisores n = [x | x <- [1..n], n `mod` x == 0]

-- (primo n) se verifica si n es primo. Por ejemplo,
--    primo 30  == False
--    primo 31  == True  
primo :: Integer -> Bool
primo n = divisores n == [1, n]

-- ---------------------------------------------------------------------
-- Ejercicio 2. [2.5 puntos] La multiplicidad de x en y es la mayor
-- potencia de x que divide a y. Por ejemplo, la multiplicidad de 2 en
-- 40 es 3 porque 40 es divisible por 2^3 y no lo es por 2^4. Además, la
-- multiplicidad de 1 en cualquier número se supone igual a 1.
-- 
-- Definir la función
--    multiplicidad :: Integer -> Integer -> Integer
-- tal que (multiplicidad x y) es la
-- multiplicidad de x en y. Por ejemplo,
--    multiplicidad 2 40  ==  3
--    multiplicidad 5 40  ==  1
--    multiplicidad 3 40  ==  0
--    multiplicidad 1 40  ==  1
-- ---------------------------------------------------------------------

multiplicidad :: Integer -> Integer -> Integer
multiplicidad 1 _ = 1
multiplicidad x y = 
    head [n | n <- [0..], y `rem` (x^n) == 0, y `rem` (x^(n+1)) /= 0]

-- ---------------------------------------------------------------------
-- Ejercicio 3. [2.5 puntos] Un número es libre de cuadrados si no es
-- divisible el cuadrado de ningún entero mayor que 1. Por ejemplo, 70
-- es libre de cuadrado porque sólo es divisible por 1, 2, 5, 7 y 70; en
-- cambio, 40 no es libre de cuadrados porque es divisible por 2^2.
-- 
-- Definir la función  
--    libreDeCuadrados :: Integer -> Bool
-- tal que (libreDeCuadrados x) se verifica si x es libre de cuadrados. 
-- Por ejemplo,  
--    libreDeCuadrados 70  ==  True
--    libreDeCuadrados 40  ==  False
-- Calcular los 10 primeros números libres de cuadrado de 3 cifras. 
-- ---------------------------------------------------------------------

-- 1ª definición:
libreDeCuadrados :: Integer -> Bool
libreDeCuadrados x = x == product (divisoresPrimos x)

-- NOTA: La función primo está definida en el ejercicio 1.

-- 2ª definición
libreDeCuadrados2 :: Integer -> Bool
libreDeCuadrados2 x = 
    and [multiplicidad n x == 1 | n <- divisores x]

-- 3ª definición
libreDeCuadrados3 :: Integer -> Bool
libreDeCuadrados3 n = 
    null [x | x <- [2..n], rem n (x^2) == 0]


-- El cálculo es
--    ghci> take 10 [n | n <- [100..], libreDeCuadrados n]
--    [101,102,103,105,106,107,109,110,111,113]

-- ---------------------------------------------------------------------
-- Ejercicio 4. [2.5 puntos] La distancia entre dos números es el valor
-- absoluto de su diferencia. Por ejemplo, la distancia entre 2 y 5 es
-- 3. 
-- 
-- Definir la función
--    cercanos :: [Int] -> [Int] -> [(Int,Int)]
-- tal que (cercanos xs ys) es la lista de pares de elementos de xs e ys
-- cuya distancia es mínima. Por ejemplo,
--    cercanos [3,7,2,1] [5,11,9]  ==  [(3,5),(7,5),(7,9)]
-- ---------------------------------------------------------------------

cercanos :: [Int] -> [Int] -> [(Int,Int)]
cercanos xs ys =
    [(x,y) | (x,y) <- pares, abs (x-y) == m]
    where pares = [(x,y) | x <- xs, y <- ys]
          m = minimum [abs (x-y) | (x,y) <- pares]
