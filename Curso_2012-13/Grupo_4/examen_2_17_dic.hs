-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 2º examen de evaluación continua (17 de diciembre de 2012)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir, usando funciones de orden superior (map,
-- filter, ... ), la función
--    sumaCuad :: [Int] -> (Int,Int)
-- tal que (sumaCuad xs) es el par formado porla suma de los cuadrados
-- de los elementos pares de xs, por una parte, y la suma de los
-- cuadrados de los elementos impares, por otra. Por ejemplo,
--    sumaCuad [1,3,2,4,5] == (20,35)
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
sumaCuad1 :: [Int] -> (Int,Int)
sumaCuad1 xs = 
    (sum [x^2 | x <- xs, even x],sum [x^2 | x <- xs, odd x])

-- 2ª definición (con filter):
sumaCuad2 :: [Int] -> (Int,Int)
sumaCuad2 xs = 
    (sum [x^2 | x <- filter even xs],sum [x^2 | x <- filter odd xs])

-- 3ª definición (con map yfilter):
sumaCuad3 :: [Int] -> (Int,Int)
sumaCuad3 xs = 
    (sum (map (^2) (filter even xs)),sum (map (^2) (filter odd xs)))

-- 4ª definición (por recursión):
sumaCuad4 :: [Int] -> (Int,Int)
sumaCuad4 xs = aux xs (0,0)
    where aux [] (a,b) = (a,b)
          aux (x:xs) (a,b) | even x    = aux xs (x^2+a,b)
                           | otherwise = aux xs (a,x^2+b)

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir, por recursión, el predicado
--    alMenosR :: Int -> [Int] -> Bool 
-- tal que (alMenosR k xs) se verifica si xs contiene, al menos, k
-- números primos. Por ejemplo, 
--    alMenosR 1 [1,3,7,10,14] == True
--    alMenosR 3 [1,3,7,10,14] == False
-- ---------------------------------------------------------------------

alMenosR :: Int -> [Int] -> Bool 
alMenosR 0 _  = True
alMenosR _ [] = False
alMenosR k (x:xs) | esPrimo x = alMenosR (k-1) xs
                  | otherwise = alMenosR k xs

-- (esPrimo x) se verifica si x es primo. Por ejemplo,
--    esPrimo 7  ==  True
--    esPrimo 9  ==  False
esPrimo :: Int -> Bool
esPrimo x =
    [n | n <- [1..x], rem x n == 0] == [1,x]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, por comprensión, el predicado
--    alMenosC :: Int -> [Int] -> Bool 
-- tal que (alMenosC k xs) se verifica si xs contiene, al menos, k
-- números primos. Por ejemplo, 
--    alMenosC 1 [1,3,7,10,14] == True
--    alMenosC 3 [1,3,7,10,14] == False
-- ---------------------------------------------------------------------

alMenosC :: Int -> [Int] -> Bool 
alMenosC k xs = length [x | x <- xs, esPrimo x] >= k

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la La función 
--    alternos :: (a -> b) -> (a -> b) -> [a] -> [b] 
-- tal que (alternos f g xs) es la lista obtenida aplicando
-- alternativamente las funciones f y g a los elementos de la lista
-- xs. Por ejemplo, 
--    ghci> alternos (+1) (*3) [1,2,3,4,5] 
--    [2,6,4,12,6]
--    ghci> alternos (take 2) reverse ["todo","para","nada"]
--    ["to","arap","na"]
-- ---------------------------------------------------------------------

alternos :: (a -> b) -> (a -> b) -> [a] -> [b] 
alternos _ _ [] = []
alternos f g (x:xs) = f x : alternos g f xs

