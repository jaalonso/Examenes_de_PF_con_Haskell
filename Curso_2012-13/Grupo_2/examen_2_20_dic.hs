-- Informática (1º del Grado en Matemáticas)
-- 2º examen de evaluación continua (20 de diciembre de 2012)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Un entero positivo n es libre de cuadrado si no es
-- divisible por ningún m^2 > 1. Por ejemplo, 10 es libre de cuadrado
-- (porque 10 = 2*5) y 12 no lo es (ya que es divisible por 2^2). 
-- Definir la función  
--    libresDeCuadrado :: Int -> [Int]
-- tal que (libresDeCuadrado n) es la lista de los primeros n números
-- libres de cuadrado. Por ejemplo,
--    libresDeCuadrado 15  ==  [1,2,3,5,6,7,10,11,13,14,15,17,19,21,22]
-- ---------------------------------------------------------------------

libresDeCuadrado :: Int -> [Int]
libresDeCuadrado n = 
    take n [n | n <- [1..], libreDeCuadrado n]

-- (libreDeCuadrado n) se verifica si n es libre de cuadrado. Por
-- ejemplo, 
--    libreDeCuadrado 10  ==  True
--    libreDeCuadrado 12  ==  False
libreDeCuadrado :: Int -> Bool
libreDeCuadrado n =
    null [m | m <- [2..n], rem n (m^2) == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    duplicaPrimo :: [Int] -> [Int]
-- tal que (duplicaPrimo xs) es la lista obtenida sustituyendo cada
-- número primo de xs por su doble. Por ejemplo,
--    duplicaPrimo [2,5,9,7,1,3]  ==  [4,10,9,14,1,6]
-- --------------------------------------------------------------------- 

duplicaPrimo :: [Int] -> [Int]
duplicaPrimo []     = []
duplicaPrimo (x:xs) | primo x   = (2*x) : duplicaPrimo xs
                    | otherwise = x : duplicaPrimo xs

-- (primo x) se verifica si x es primo. Por ejemplo,
--    primo 7  ==  True
--    primo 8  ==  False
primo :: Int -> Bool
primo x = divisores x == [1,x]

-- (divisores x) es la lista de los divisores de x. Por ejemplo,
--    divisores 30  ==  [1,2,3,5,6,10,15,30]
divisores :: Int -> [Int]
divisores x = [y | y <- [1..x], rem x y == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    ceros :: Int -> Int 
-- tal que (ceros n) es el número de ceros en los que termina el número
-- n. Por ejemplo, 
--    ceros 3020000  ==  4
-- ---------------------------------------------------------------------

ceros :: Int -> Int 
ceros n | rem n 10 /= 0 = 0
        | otherwise     = 1 + ceros (div n 10)

-- ---------------------------------------------------------------------
-- Ejercicio 4. [Problema 387 del Proyecto Euler]. Un número de Harshad
-- es un entero divisible entre la suma de sus dígitos. Por ejemplo, 201
-- es un número de Harshad porque es divisible por 3 (la suma de sus
-- dígitos). Cuando se elimina el último dígito de 201 se obtiene 20 que
-- también es un número de Harshad. Cuando se elimina el último dígito
-- de 20 se obtiene 2 que también es un número de Harshad. Los números
-- como el 201 que son de Harshad y que los números obtenidos eliminando
-- sus últimos dígitos siguen siendo de Harshad se llaman números de
-- Harshad hereditarios por la derecha. Definir la función
--    numeroHHD :: Int -> Bool
-- tal que (numeroHHD n) se verifica si n es un número de Harshad
-- hereditario por la derecha. Por ejemplo,
--    numeroHHD 201  ==  True
--    numeroHHD 140  ==  False
--    numeroHHD 1104 ==  False
-- Calcular el mayor número de Harshad hereditario por la derecha con
-- tres dígitos.
-- ---------------------------------------------------------------------

-- (numeroH n) se verifica si n es un número de Harshad.
--    numeroH 201  ==  True
numeroH :: Int -> Bool
numeroH n = rem n (sum (digitos n)) == 0

-- (digitos n) es la lista de los dígitos de n. Por ejemplo,
--    digitos 201  ==  [2,0,1]
digitos :: Int -> [Int]
digitos n = [read [d] | d <- show n]

numeroHHD :: Int -> Bool 
numeroHHD n | n < 10    = True
            | otherwise = numeroH n && numeroHHD (div n 10) 

-- El cálculo es
--    ghci> head [n | n <- [999,998..100], numeroHHD n]
--    902

