-- Informática (1º del Grado en Matemáticas)
-- 2º examen de evaluación continua (2 de diciembre de 2015)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Una lista hermanada es una lista de números
-- estrictamente positivos en la que cada elemento tiene algún factor
-- primo en común con el siguiente, en caso de que exista, o alguno de
-- los dos es un 1. Por ejemplo, 
--    + [2,6,3,9,1,5] es una lista hermanada pues 2 y 6 tienen un factor
--      en común (2); 6 y 3 tienen un factor en común (3); 3 y 9 tienen un
--      factor en común (3); de 9 y 1 uno es el número 1; y de 1 y 5 uno
--      es el número 1. 
--    + [2,3,5] no es una lista hermanada pues 2 y 3 no tienen ningún
--      factor primo en común. 
--
-- Definir, por comprensión, la función
--    hermanadaC :: [Int] -> Bool
-- tal que (hermanada xs) se verifica si la lista xs es hermanada según la
-- definición anterior. Por ejemplo,
--    hermanadaC [2,6,3,9,1,5]  ==  True
--    hermanadaC [2,3,5]        ==  False
-- ---------------------------------------------------------------------

hermanadaC :: [Int] -> Bool
hermanadaC xs = and [hermanos p | p <- zip xs (tail xs)]

-- (hermanos (x,y)) se verifica si x e y son hermanos; es decir, alguno es
-- igual a 1 o tienen algún factor primo en común
hermanos :: (Int, Int) -> Bool
hermanos (x,y) = x == 1 || y == 1 || gcd x y /= 1

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, usando funciones de orden superior, la función
--    hermanadaS :: [Int] -> Bool
-- tal que (hermanada xs) se verifica si la lista xs es hermanada según la
-- definición anterior. Por ejemplo,
--    hermanadaS [2,6,3,9,1,5]  ==  True
--    hermanadaS [2,3,5]        ==  False
-- ---------------------------------------------------------------------

hermanadaS :: [Int] -> Bool
hermanadaS xs = all hermanos (zip xs (tail xs))

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir, por recursión, la función
--    hermanadaR :: [Int] -> Bool
-- tal que (hermanada xs) se verifica si la lista xs es hermanada según la
-- definición anterior. Por ejemplo,
--    hermanadaR [2,6,3,9,1,5]  ==  True
--    hermanadaR [2,3,5]        ==  False
-- ---------------------------------------------------------------------

hermanadaR :: [Int] -> Bool
hermanadaR (x1:x:xs) = hermanos (x1,x) && hermanadaR (x:xs)
hermanadaR _          = True

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Definir, por plegado, la función
--    hermanadaP :: [Int] -> Bool
-- tal que (hermanada xs) se verifica si la lista xs es hermanada según la
-- definición anterior. Por ejemplo,
--    hermanadaP [2,6,3,9,1,5]  ==  True
--    hermanadaP [2,3,5]        ==  False
-- ---------------------------------------------------------------------

hermanadaP :: [Int] -> Bool
hermanadaP xs =
    foldl (\ws p -> hermanos p && ws) True (zip xs (tail xs))

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir, por recursión con acumulador, la función
--    sumaEnPosicion :: [Int] -> [Int] -> Int
-- tal que (sumaEnPosicion xs ys) es la suma de todos los elementos de xs
-- cuyas posiciones se indican en ys. Si alguna posición no existe en xs
-- entonces el valor se considera nulo. Por ejemplo,
--    sumaEnPosicion [1,2,3] [0,2]  ==  4
--    sumaEnPosicion [4,6,2] [1,3]  ==  6
--    sumaEnPosicion [3,5,1] [0,1]  ==  8
-- ---------------------------------------------------------------------

sumaEnPosicion :: [Int] -> [Int] -> Int
sumaEnPosicion xs ys = aux xs [y | y <- ys, 0 <= y, y < n] 0
    where n = length xs
          aux _  []     r = r
          aux xs (y:ys) r = aux xs ys (r + xs!!y)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    productoInfinito :: [Int] -> [Int]
-- tal que (productoInfinito xs) es la lista infinita que en la posición
-- N tiene el producto de los N primeros elementos de la lista infinita
-- xs. Por ejemplo, 
--    take 5 (productoInfinito [1..])    ==  [1,2,6,24,120]
--    take 5 (productoInfinito [2,4..])  ==  [2,8,48,384,3840]
--    take 5 (productoInfinito [1,3..])  ==  [1,3,15,105,945]
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
productoInfinito1 :: [Integer] -> [Integer]
productoInfinito1 xs = [product (take n xs) | n <- [1..]]

-- 2ª definición (por recursión)
productoInfinito2 :: [Integer] -> [Integer]
productoInfinito2 (x:y:zs) = x : productoInfinito2 (x*y:zs)

-- 2ª definición (por recursión y map)
productoInfinito3 :: [Integer] -> [Integer]
productoInfinito3 []     = [1]
productoInfinito3 (x:xs) = map (x*) (1 : productoInfinito3 xs)

-- 4ª definición (con scanl1)
productoInfinito4 :: [Integer] -> [Integer]
productoInfinito4 = scanl1 (*)

-- Comparación de eficiencia
--    ghci> take 20 (show (productoInfinito1 [2,4..] !! 10000))
--    "11358071114466915693"
--    (0.35 secs, 98,287,328 bytes)
--    ghci> take 20 (show (productoInfinito2 [2,4..] !! 10000))
--    "11358071114466915693"
--    (0.35 secs, 98,840,440 bytes)
--    ghci> take 20 (show (productoInfinito3 [2,4..] !! 10000))
--    "11358071114466915693"
--    (7.36 secs, 6,006,360,472 bytes)
--    ghci> take 20 (show (productoInfinito4 [2,4..] !! 10000))
--    "11358071114466915693"
--    (0.34 secs, 96,367,000 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    siembra :: [Int] -> [Int]
-- tal que (siembra xs) es la lista ys obtenida al repartir cada
-- elemento x de la lista xs poniendo un 1 en las x siguientes
-- posiciones de la lista ys. Por ejemplo, 
--    siembra [4]      ==  [0,1,1,1,1] 
--    siembra [0,2]    ==  [0,0,1,1]
--    siembra [4,2]    ==  [0,1,2,2,1]
-- El tercer ejemplo se obtiene sumando la siembra de 4 en la posición 0
-- (como el ejemplo 1) y el 2 en la posición 1 (como el ejemplo 2). 
-- Otros ejemplos son 
--    siembra [0,4,2]  ==  [0,0,1,2,2,1]
--    siembra [3]      ==  [0,1,1,1]
--    siembra [3,4,2]  ==  [0,1,2,3,2,1]
--    siembra [3,2,1]  ==  [0,1,2,3]
--
-- Comprobar con QuickCheck que la suma de los elementos de (siembra xs)
-- es igual que la suma de los de xs.
-- 
-- Nota: Se supone que el argumento es una lista de números no negativos
-- y que se puede ampliar tanto como sea necesario para repartir los
-- elementos. 
-- ---------------------------------------------------------------------

siembra :: [Int] -> [Int]
siembra [] = []
siembra (x:xs) = mezcla (siembraElemento x) (0 : siembra xs)

siembraElemento :: Int -> [Int]
siembraElemento x = 0 : replicate x 1

mezcla :: [Int] -> [Int] -> [Int]
mezcla xs ys =
    take (max (length xs) (length ys))
         (zipWith (+) (xs ++ repeat 0) (ys ++ repeat 0))

-- La propiedad es
prop_siembra :: [Int] -> Bool
prop_siembra xs =
    sum (siembra ys) == sum ys
    where ys = map (\x -> 1 + abs x) xs

-- La comprobación es
--    ghci> quickCheck prop_siembra
--    +++ OK, passed 100 tests.
