-- Informática (1º del Grado en Matemáticas)
-- 2º examen de evaluación continua (30 de noviembre de 2016)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    cuadrado :: [Integer] -> Integer
-- tal que (cuadrado xs) es el número obtenido uniendo los cuadrados de
-- los números de la lista xs. Por ejemplo,
--    cuadrado [2,6,9] == 43681
--    cuadrado [10]    == 100
-- ---------------------------------------------------------------------

-- 1ª definición (usando recursión)
cuadrado1 :: [Integer] -> Integer
cuadrado1 xs = read (aux xs)
  where aux [] = ""
        aux (y:ys) = show (y^2) ++ aux ys
 
-- 2ª definición (plegado a la derecha)
cuadrado2 :: [Integer] -> Integer
cuadrado2 = read . aux
  where aux = foldr f "" 
        f x y = show (x^2) ++ y

-- 3ª definición (por comprensión)
cuadrado3 :: [Integer] -> Integer
cuadrado3 ys = read (concat [show (y^2) | y <- ys])
 
-- 4ª definición (con orden superior)
cuadrado4 :: [Integer] -> Integer
cuadrado4 = read . concatMap (show . (^2))
 
-- 5ª definición (con acumulador)
cuadrado5 :: [Integer] -> Integer
cuadrado5 xs = read (aux xs "")
  where aux [] ys     = ys
        aux (x:xs) ys = aux xs (ys ++ show (x^2))

-- 6ª definición (con plegado a la izquierda)
cuadrado6 :: [Integer] -> Integer
cuadrado6 xs = read (foldl g "" xs)
  where g ys x = ys ++ show (x^2)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Un elemento de una lista es una cima si es mayor que los
-- que tiene más cerca, su antecesor y su sucesor.
-- 
-- Definir la función 
--    cimas:: [Int] -> [Int]
-- tal que (cimas xs) es la lista de las cimas de xs. Por ejemplo, 
--    cimas [80,1,7,5,9,1]  ==  [7,9]
--    cimas [1,7,8,4]       ==  [8]
--    cimas [3,2,6,1,5,4]   ==  [6,5]
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión)
cimasC ::  [Int] -> [Int]
cimasC xs = [x | (y,x,z) <- trozos xs, y < x, x > z] 

trozos :: [a] -> [(a,a,a)]
trozos (x:y:z:zs) = (x,y,z) : trozos (y:z:zs)
trozos _          = []

-- 2ª definición (orden superior)
cimasS ::  [Int] -> [Int]
cimasS xs = concatMap f (trozos xs)
  where f (a,b,c) | a < b && b > c = [b]
                  | otherwise      = [] 

-- 3ª definición (por recursión)
cimasR ::  [Int] -> [Int]
cimasR (x:y:z:xs) | x < y && y > z = y : cimasR (y:z:xs)
                  | otherwise      = cimasR (y:z:xs)
cimasR _ = []

-- 4ª definición (plegado a la derecha)
cimasPR ::  [Int] -> [Int]
cimasPR xs = concat (foldr f [] (trozos xs))
    where f (a,b,c) ys | a < b && b > c = [b] : ys
                       | otherwise      = []  : ys
 
-- 5ª definición (con acumuladores)
cimasA ::  [Int] -> [Int]
cimasA xs = concat (aux (trozos xs) [[]])
  where aux [] ac = ac
        aux ((a,b,c):ts) ac
          | a < b && b > c = aux ts (ac ++ [[b]])
          | otherwise      = aux ts (ac ++ [[]])

-- 6ª definición (por plegado a la izquierda)
cimasPL ::  [Int] -> [Int]
cimasPL xs = concat (foldl g [] (trozos xs))
    where g acum (a,b,c)
            | a < b && b > c = acum ++ [[b]]
            | otherwise      = acum ++ [[]]

