-- Informática (1º del Grado en Matemáticas)
-- 1º examen de evaluación continua (27 de octubre de 2015)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir por comprensión la función
--    posiciones :: Int -> [Int] -> [Int]
-- tal que (posiciones x xs) es la lista de las posiciones (contando desde 0)
-- de las ocurrencias del elemento x en la lista xs. Por ejemplo,
--    posiciones 2 [1,2,3,4]                  ==  [1]
--    posiciones 2 [1,2,3,4,1,2,3,1]          ==  [1,5]
--    posiciones 2 [1,2,3,4,1,2,3,1,3,2,4,2]  ==  [1,5,9,11]
-- ---------------------------------------------------------------------

posiciones :: Int -> [Int] -> [Int]
posiciones x xs =
    [i | (y,i) <- zip xs [0..], x == y]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Una forma de aproximar el número pi es usando la
-- siguiente igualdad: 
--
--            pi         1     1*2     1*2*3     1*2*3*4     
--           --- = 1 + --- + ----- + ------- + --------- + ....
--            2         3     3*5     3*5*7     3*5*7*9
--
-- Es decir, la serie cuyo término general n-ésimo es el cociente entre el
-- producto de los primeros n números y los primeros n números impares:
--
--                       Product i   
--           s(n) =  -----------------
--                    Product (2*i+1)
--
-- Definir por comprensión la función:
--    aproximaPi :: Double -> Double
-- tal que (aproximaPi n) es la aproximación del número pi calculada con la
-- serie anterior hasta el término n-ésimo. Por ejemplo,
--    aproximaPi 10   ==  3.141106021601377
--    aproximaPi 30   ==  3.1415926533011587
--    aproximaPi 50   ==  3.1415926535897922
-- ---------------------------------------------------------------------

aproximaPi :: Double -> Double
aproximaPi n =
    2*(sum [product [1..m] / product [2*i+1 | i <- [1..m]] | m <- [0..n] ])

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir por recursión la función
--    subPar :: Integer -> Integer
-- tal que (subPar n) es el número formado por las cifras que ocupan una
-- posición par (contando desde 0 por las unidades) en n en el mismo
-- orden. Por ejemplo, 
--    subPar 123        ==     13
--    subPar 123456     ==    246
--    subPar 123456789  ==  13579
-- ---------------------------------------------------------------------

-- 1ª solución
subPar :: Integer -> Integer
subPar 0 = 0
subPar n = (subPar (n `div` 100))*10 + (n `mod` 10)

-- 2ª solución
subPar2 :: Integer -> Integer
subPar2 n = read (reverse (aux (reverse (show n))))
    where aux (x:y:zs) = x : aux zs
          aux xs       = xs

-- 3ª solución
subPar3 :: Integer -> Integer
subPar3 n = aux (digitos n) 
    where aux []       = 0
          aux [x]      = x
          aux (x:y:zs) = x + 10 * aux zs

digitos :: Integer -> [Integer]
digitos n | n < 10 = [n]
          | otherwise = n `rem` 10 : digitos (n `div` 10)

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir por recursión la función
--    subImpar :: Integer -> Integer
-- tal que (subImpar n) es el número formado por las cifras que ocupan una
-- posición impar (contando desde 0 por las unidades) en n en el mismo
-- orden. Por ejemplo, 
--    subImpar 123        ==     2
--    subImpar 123456     ==   135
--    subImpar 123456789  ==  2468
-- ---------------------------------------------------------------------

-- 1ª solución
subImpar :: Integer -> Integer
subImpar 0 = 0
subImpar n = (subImpar (n `div` 100))*10 + ((n `mod` 100) `div` 10)

-- 2ª solución
subImpar2 :: Integer -> Integer
subImpar2 n = read (reverse (aux (reverse (show n))))
    where aux (x:y:zs) = y : aux zs
          aux xs       = []

-- 3ª solución
subImpar3 :: Integer -> Integer
subImpar3 n = aux (digitos n) 
    where aux []       = 0
          aux [x]      = 0
          aux (x:y:zs) = y + 10 * aux zs

-- ---------------------------------------------------------------------
-- Ejercicio 4. Una forma de aproximar el número e es usando la
-- siguiente igualdad: 
--
--                      1     1     1     1     
--            e  = 1 + --- + --- + --- + --- + ....
--                      1!    2!    3!    4!
--
-- Es decir, la serie cuyo término general n-ésimo es el recíproco del
-- factorial de n:
--
--                    1  
--           s(n) =  ---
--                    n!
--
-- Definir por recursión la función:
--    aproximaE :: Double -> Double
-- tal que (aproximaE n) es la aproximación del número e calculada con la
-- serie anterior hasta el término n-ésimo. Por ejemplo,
--    aproximaE 5   ==  2.7166666666666663
--    aproximaE 10  ==  2.7182818011463845
--    aproximaE 15  ==  2.718281828458995
--    aproximaE 20  ==  2.7182818284590455
-- ---------------------------------------------------------------------

aproximaE :: Double -> Double
aproximaE 0 = 1
aproximaE n = (1/(product [1..n])) + aproximaE (n-1)
