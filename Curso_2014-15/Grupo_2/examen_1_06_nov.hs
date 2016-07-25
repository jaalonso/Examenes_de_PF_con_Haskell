-- Informática (1º del Grado en Matemáticas)
-- 1º examen de evaluación continua (6 de noviembre de 2014)   
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir por comprensión, recursión y con funciones de
-- orden superior, la función 
--    escalonada :: (Num a, Ord a) => [a] -> Bool
-- tal que (escalonada xs) se verifica si la diferencia entre números
-- consecutivos de xs  es siempre, en valor absoluto, mayor que 2. Por
-- ejemplo, 
--    escalonada [1,5,8,23,5] == True
--    escalonada [3,6,8,1]    == False
--    escalonada [-5,-2,4 ]   == True
--    escalonada [5,2]        == True
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
escalonadaC :: (Num a, Ord a) => [a] -> Bool
escalonadaC xs = and [abs (x-y) > 2 | (x,y) <- zip xs (tail xs)]

-- 2ª definición (por recursión):
escalonadaR :: (Num a, Ord a) => [a] -> Bool
escalonadaR (x:y:zs) = abs (x-y) > 2 && escalonadaR (y:zs)
escalonadaR _        = True

-- 3ª definición (con funciones de orden superior):
escalonadaO :: (Num a, Ord a) => [a] -> Bool
escalonadaO xs = all ((>2) . abs) (zipWith (-) xs (tail xs))

-- ----------------------------------------------------------------------
-- Ejercicio 2. Definir la función 
--    elementos :: Ord a => [a] -> [a] -> Int
-- tal que (elementos xs ys) es el número de elementos de xs son menores
-- que los correpondientes de ys hasta el primero que no lo sea. Por
-- ejemplo, 
--    elementos "prueba" "suspenso"        ==  2
--    elementos [1,2,3,4,5] [2,3,4,3,8,9]  ==  3
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión):
elementosR :: Ord a => [a] -> [a] -> Int
elementosR (x:xs) (y:ys) | x < y     = 1 + elementosR xs ys
                         | otherwise = 0
elementosR _ _ = 0

-- 2ª definición (con funciones de orden superior):
elementosO :: Ord a => [a] -> [a] -> Int
elementosO xs ys = length (takeWhile menor (zip xs ys))
    where menor (x,y) = x < y

-- ----------------------------------------------------------------------
-- Ejercicio 3. Definir por comprensión  y recursión la función 
--    sumaPosParR :: Int -> Int
-- tal que (sumaPosParR x) es la suma de los dígitos de x que ocupan
-- posición par. Por ejemplo,
--    sumaPosPar 987651 = 8+6+1 = 15
--    sumaPosPar 98765  = 8+6   = 14
--    sumaPosPar 9876   = 8+6   = 14
--    sumaPosPar 987    = 8     =  8
--    sumaPosPar 9              =  0
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión):
sumaPosParR :: Int -> Int
sumaPosParR x 
    | even (length (digitos x )) = aux x
    | otherwise                  = aux (div x 10)
    where aux x | x < 10    = 0
                | otherwise = mod x 10 + sumaPosParR (div x 10)

digitos :: Int -> [Int]
digitos x = [read [y] | y <- show x]

-- 2ª definición (por comprensión):
sumaPosParC :: Int -> Int
sumaPosParC x = sum [y | (y,z) <- zip (digitos x) [1 ..], even z]

-- ----------------------------------------------------------------------
-- Ejercicio 3. Define la función 
--    sinCentrales :: [a] -> [a]
-- tal que (sinCentrales xs) es la lista obtenida eliminando el elemento
-- central de xs si xs es de longitud impar y sus dos elementos
-- centrales si es de longitud par. Por ejemplo, 
--    sinCentrales [1,2,3,4] ==  [1,4]
--    sinCentrales [1,2,3]   ==  [1,3]
--    sinCentrales [6,9]     ==  []
--    sinCentrales [7]       ==  []
--    sinCentrales []        ==  []
-- ---------------------------------------------------------------------

sinCentrales :: [a] -> [a]
sinCentrales [] = []
sinCentrales xs | even n    = init ys ++ zs
                | otherwise = ys ++ zs
      where n         = length xs
            (ys,z:zs) = splitAt (n `div` 2) xs 
