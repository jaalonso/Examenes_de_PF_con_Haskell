-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 1º examen de evaluación continua (26 de octubre de 2016)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    ceros :: Int -> Int
-- tal que (ceros n) es el número de ceros en los que termina el número
-- n. Por ejemplo,
--    ceros 30500  ==  2
--    ceros 30501  ==  0
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión):
ceros :: Int -> Int
ceros n | n `rem` 10 == 0 = 1 + ceros (n `div`10)
        | otherwise       = 0

-- 2ª definición (por comprensión):
ceros2 :: Int -> Int
ceros2 0 = 1
ceros2 n = head [x | x <- [0..]
                   , n `rem` (10^x) /= 0] - 1

-- ---------------------------------------------------------------------
-- Ejercicio 2. Una representación de 46 en base 3 es [1,0,2,1] pues
--    46 = 1*3^0 + 0*3^1 + 2*3^2 + 1*3^3.
-- Una representación de 20 en base 2 es [0,0,1,0,1] pues
--    20 = 1*2^2 + 1*2^4. 
-- 
-- Definir la función
--    enBase :: Int -> [Int] -> Int
-- tal que (enBase b xs) es el número n tal que su representación en
-- base b es xs. Por ejemplo, 
--    enBase 3 [1,0,2,1]        == 46
--    enBase 2 [0,0,1,0,1]      == 20
--    enBase 2 [1,1,0,1]        == 11
--    enBase 5 [0,2,1,3,1,4,1]  == 29160
-- ---------------------------------------------------------------------

enBase :: Int -> [Int] -> Int
enBase b xs = sum [y*b^n | (y,n) <- zip xs [0..]]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función 
--    repetidos :: Eq a => [a] -> [a]
-- tal que (repetidos xs) es la lista de los elementos repetidos de
-- xs. Por ejemplo, 
--    repetidos [1,3,2,1,2,3,4] == [1,3,2]
--    repetidos [1,2,3]         == []
-- ---------------------------------------------------------------------

repetidos :: Eq a => [a] -> [a]
repetidos [] = []
repetidos (x:xs) | x `elem` xs = x : repetidos xs
                 | otherwise   =  repetidos xs

-- ---------------------------------------------------------------------
-- Ejercicio 4. [Problema 37 del proyecto Euler] Un número
-- primo es truncable si los números que se obtienen eliminado cifras,
-- de derecha a izquierda, son primos. Por ejemplo, 599 es un primo
-- truncable porque 599, 59 y 5 son primos; en cambio, 577 es un primo
-- no truncable porque 57 no es primo.  
-- 
-- Definir la función 
--    primoTruncable :: Int -> Bool
-- tal que (primoTruncable x) se verifica si x es un primo
-- truncable. Por ejemplo,
--    primoTruncable 599  ==  True
--    primoTruncable 577  ==  False
-- ---------------------------------------------------------------------

primoTruncable :: Int -> Bool
primoTruncable x 
  | x < 10    = primo x
  | otherwise = primo x && primoTruncable (x `div` 10)

-- (primo x) se verifica si x es primo.
primo :: Int -> Bool
primo x = factores x == [1,x]

-- (factores x) es la lista de los factores de x.
factores :: Int -> [Int]
factores x = [y | y <- [1..x]
                , x `rem` y == 0]
