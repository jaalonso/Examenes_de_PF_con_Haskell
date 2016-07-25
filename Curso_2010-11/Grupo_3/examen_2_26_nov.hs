-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 2º examen de evaluación continua (26 de noviembre de 2010)
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por recursión, la función 
--    sustituyeImpar :: [Int] -> [Int]
-- tal que (sustituyeImpar x) es la lista obtenida sustituyendo cada
-- número impar de xs por el siguiente número par. Por ejemplo,
--    sustituyeImpar [3,2,5,7,4]  ==  [4,2,6,8,4]
-- ---------------------------------------------------------------------

sustituyeImpar :: [Int] -> [Int]
sustituyeImpar []     = []
sustituyeImpar (x:xs) | odd x     = x+1 : sustituyeImpar xs
                      | otherwise = x : sustituyeImpar xs

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickChek que para cualquier
-- lista de números enteros xs, todos los elementos de la lista
-- (sustituyeImpar xs) son números pares. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_sustituyeImpar :: [Int] -> Bool
prop_sustituyeImpar xs = and [even x | x <- sustituyeImpar xs]

-- La comprobación es
--    ghci> quickCheck prop_sustituyeImpar
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.1 El número e se puede definir como la suma de la serie
--    1/0! + 1/1! + 1/2! + 1/3! +...
-- 
-- Definir la función aproxE tal que (aproxE n) es la aproximación de e
-- que se obtiene sumando los términos de la serie hasta 1/n!. Por
-- ejemplo,  
--    aproxE 10    ==  2.718281801146385
--    aproxE 100   ==  2.7182818284590455
--    aproxE 1000  ==  2.7182818284590455
-- ---------------------------------------------------------------------

aproxE n = 1 + sum [1/(factorial k) | k <- [1..n]]

-- (factorial n) es el factorial de n. Por ejemplo,
--    factorial 5  ==  120
factorial n = product [1..n]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la constante e como 2.71828459.
-- ---------------------------------------------------------------------

e = 2.71828459

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Definir la función errorE tal que (errorE x) es el
-- menor número de términos de la serie anterior necesarios para obtener
-- e con un error menor que x. Por ejemplo,
--    errorE 0.001   == 6.0
--    errorE 0.00001 == 8.0
-- ---------------------------------------------------------------------

errorE x = head [n | n <- [0..], abs(aproxE n - e) < x]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Un número natural n se denomina abundante si es menor
-- que la suma de sus divisores propios.
-- 
-- Definir una función 
--    numeroAbundante:: Int -> Bool 
-- tal que (numeroAbundante n) se verifica si n es un número
-- abundante. Por ejemplo,
--    numeroAbundante 5   ==  False
--    numeroAbundante 12  ==  True
--    numeroAbundante 28  ==  False
--    numeroAbundante 30  ==  True
-- ---------------------------------------------------------------------

numeroAbundante:: Int -> Bool 
numeroAbundante n = n < sum (divisores n)

-- (divisores n) es la lista de los divisores de n. Por ejemplo,
--    divisores 24  ==  [1,2,3,4,6,8,12]
divisores:: Int -> [Int]
divisores n = [m | m <- [1..n-1], n `mod` m == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función 
--    numerosAbundantesMenores :: Int -> [Int]
-- tal que (numerosAbundantesMenores n) es la lista de números
-- abundantes menores o iguales que n. Por ejemplo,
--    numerosAbundantesMenores 50  ==  [12,18,20,24,30,36,40,42,48]
-- ---------------------------------------------------------------------

numerosAbundantesMenores :: Int -> [Int]
numerosAbundantesMenores n = [x | x <- [1..n], numeroAbundante x]

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir la función 
--    todosPares :: Int -> Bool
-- tal que (todosPares n) se verifica si todos los números abundantes
-- menores o iguales que n son pares. Comprobar  el valor de dicha
-- función para n = 10, 100 y 1000. 
-- ---------------------------------------------------------------------

todosPares :: Int -> Bool
todosPares n = and [even x | x <- numerosAbundantesMenores n]

-- La comprobación es
--    ghci> todosPares 10
--    True
--    ghci> todosPares 100
--    True
--    ghci> todosPares 1000
--    False

-- ---------------------------------------------------------------------
-- Ejercicio 3.4. Definir la constante 
--    primerAbundanteImpar :: Int
-- cuyo valor es el primer número natural abundante impar.
-- ---------------------------------------------------------------------

primerAbundanteImpar :: Int
primerAbundanteImpar = head [x | x <-[1..], numeroAbundante x, odd x]

-- Su valor es
--    ghci> primerAbundanteImpar
--    945
