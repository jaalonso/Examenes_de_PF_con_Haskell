-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- Examen de la 2ª convocatoria (15 de septiembre de 2010)
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función
--    diagonal :: [[a]] -> [a]
-- tal que (diagonal m) es la diagonal de la matriz m. Por ejemplo,
--    diagonal [[3,5,2],[4,7,1],[6,9,0]]  ==  [3,7,0]
--    diagonal [[3,5,2],[4,7,1]]          ==  [3,7]
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión):
diagonal :: [[a]] -> [a]
diagonal ((x1:_):xs) = x1 : diagonal [tail x | x <- xs]
diagonal _ = []

-- Segunda definición (sin recursión):
diagonal2 :: [[a]] -> [a]
diagonal2 = flip (zipWith (!!)) [0..]

-- ---------------------------------------------------------------------.
-- Ejercicio 1.2. Definir la función
--    matrizDiagonal ::  Num a => [a] -> [[a]]
-- tal que (matrizDiagonal xs) es la matriz cuadrada cuya diagonal es el
-- vector xs y los restantes elementos son iguales a cero. Por ejemplo, 
--    matrizDiagonal [2,5,3]  ==  [[2,0,0],[0,5,0],[0,0,3]]
-- ---------------------------------------------------------------------

matrizDiagonal :: Num a => [a] -> [[a]]
matrizDiagonal [] = []
matrizDiagonal (x:xs) =
    (x: [0 | _ <- xs]) : [0:zs | zs <- ys]
    where ys = matrizDiagonal xs

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comprobar con QuickCheck si se verifican las
-- siguientes propiedades: 
-- 1. Para cualquier lista xs, (diagonal (matrizDiagonal xs)) es igual a
--    xs.  
-- 2. Para cualquier matriz m, (matrizDiagonal (diagonal m)) es igual a
--    m.  
-- ---------------------------------------------------------------------

-- La primera propiedad es
prop_diagonal1 :: [Int] -> Bool
prop_diagonal1 xs =
    diagonal (matrizDiagonal xs) == xs

-- La comprobación es
--    ghci> quickCheck prop_diagonal1
--    +++ OK, passed 100 tests.

-- La segunda propiedad es
prop_diagonal2 :: [[Int]] -> Bool
prop_diagonal2 m =
    matrizDiagonal (diagonal m) == m

-- La comprobación es 
--    ghci> quickCheck prop_diagonal2
--    *** Failed! Falsifiable (after 4 tests and 5 shrinks):  
--    [[0,0]]
-- lo que indica que la propiedad no se cumple y que [[0,0]] es un
-- contraejemplo, 

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. El enunciado del problema 1 de la Fase nacional de la
-- Olimpiada Matemática Española del 2009 dice: 
--    Hallar todas las sucesiones finitas de n números naturales
--    consecutivos a1, a2, ..., an, con n ≥ 3, tales que 
--    a1 + a2 + ... + an = 2009.
--  
-- En este ejercicio vamos a resolver el problema con Haskell.
-- 
-- Definir la función
--    sucesionesConSuma :: Int -> [[Int]]
-- tal que (sucesionesConSuma x) es la lista de las sucesiones finitas
-- de n números naturales consecutivos a1, a2, ..., an, con n ≥ 3, tales
-- que  
--    a1 + a2 + ... + an = x.
-- Por ejemplo.
--    sucesionesConSuma 9   ==  [[2,3,4]]
--    sucesionesConSuma 15  ==  [[1,2,3,4,5],[4,5,6]]
-- ---------------------------------------------------------------------

-- 1ª definición:
sucesionesConSuma :: Int -> [[Int]]
sucesionesConSuma x = 
    [[a..b] | a <- [1..x], b <- [a+2..x], sum [a..b] == x] 

-- 2ª definición (con la fórmula de la suma de las progresiones
-- aritméticas): 
sucesionesConSuma' :: Int -> [[Int]]
sucesionesConSuma' x = 
    [[a..b] | a <- [1..x], b <- [a+2..x], (a+b)*(b-a+1) `div` 2 == x] 

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Resolver el problema de la Olimpiada con la función
-- sucesionesConSuma.  
-- ---------------------------------------------------------------------

-- Las soluciones se calculan con
--    ghci> sucesionesConSuma' 2009
--    [[17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,
--      38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,
--      59,60,61,62,63,64,65],
--     [29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,
--      50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69],
--     [137,138,139,140,141,142,143,144,145,146,147,148,149,150],
--     [284,285,286,287,288,289,290]]
-- Por tanto, hay 4 soluciones.

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función
--    sumasNcuadrados :: Int -> Int -> [[Int]]
-- tal que (sumasNcuadrados x n) es la lista de las descomposiciones de
-- x en sumas decrecientes de n cuadrados. Por ejemplo,
--    sumasNcuadrados 10 4  ==  [[3,1,0,0],[2,2,1,1]]
-- ---------------------------------------------------------------------

sumasNcuadrados :: Int -> Int -> [[Int]]
sumasNcuadrados x 1 | a^2 == x  = [[a]] 
                    | otherwise = []
    where a = ceiling (sqrt (fromIntegral x))
sumasNcuadrados x n = 
    [a:y:ys | a <- [x',x'-1..0],
              (y:ys) <- sumasNcuadrados (x-a^2) (n-1),
              y <= a]
    where x' = ceiling (sqrt (fromIntegral x))

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función 
--    numeroDeCuadrados :: Int -> Int
-- tal que (numeroDeCuadrados x) es el menor número de cuadrados que se
-- necesita para escribir x como una suma de cuadrados. Por ejemplo,
--    numeroDeCuadrados 6  ==  3
--    sumasNcuadrados 6 3  ==  [[2,1,1]]
-- ---------------------------------------------------------------------

numeroDeCuadrados :: Int -> Int
numeroDeCuadrados x = head [n | n <- [1..], sumasNcuadrados x n /= []]

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Calcular el menor número n tal que todos los números
-- de 0 a 100 pueden expresarse como suma de n cuadrados. 
-- ---------------------------------------------------------------------

-- El cálculo de n es 
--    ghci> maximum [numeroDeCuadrados x | x <- [0..100]]
--    4

-- ---------------------------------------------------------------------
-- Ejercicio 3.4. Comprobar con QuickCheck si todos los números
-- positivos pueden expresarse como suma de n cuadrados (donde n es el
-- número calculado anteriormente). 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_numeroDeCuadrados x =
    x >= 0 ==> numeroDeCuadrados x <= 4

-- La comprobación es
--    ghci> quickCheck prop_numeroDeCuadrados
--    OK, passed 100 tests.
