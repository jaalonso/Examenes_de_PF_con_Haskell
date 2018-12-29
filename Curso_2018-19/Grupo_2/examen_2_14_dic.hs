-- Informática (1º del Grado en Matemáticas, Grupo 2)
-- 2º examen de evaluación continua (14 de diciembre de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. El resultado de agregar una lista consiste en construir
-- otra lista obtenida sumando elementos consecutivos de la primera. Por
-- ejemplo, el resultado de agregar la lista [1,2,3,4] es la lista
-- [3,5,7] que se obtiene sumando 1 y 2; 2 y 3; y 3 y 4. 
--
-- Definir la función
--    agregarLista :: [Int] -> [Int]
-- tal que (agregarLista xs) es la lista obtenida agregando la lista xs.
-- Por ejemplo,
--    agregarLista [1,2,3,4]  ==  [3,5,7]
--    agregarLista [3,5,7]    ==  [8,12]
--    agregarLista [8,12]     ==  [20]
--    agregarLista [20]       ==  []
--    agregarLista []         ==  []
-- ---------------------------------------------------------------------

-- 1ª definición
agregarLista :: [Int] -> [Int]
agregarLista xs =
  zipWith (+) xs (tail xs)

-- 2ª definición
agregarLista2 :: [Int] -> [Int]
agregarLista2 xs =
  [x + y | (x,y) <- zip xs (tail xs)]

-- 3ª definición
agregarLista3 :: [Int] -> [Int]
agregarLista3 (x:y:zs) = (x+y) : agregarLista3 (y:zs)
agregarLista3 _        = []
    
-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    incrementaDigitos :: Integer -> Integer
-- tal que (incrementaDigitos n) es el número que se obtiene
-- incrementando en una unidad todos los dígitos del número n (pasando
-- del 9 al 0). Por ejemplo,
--    incrementaDigitos 13579  ==  24680
--    incrementaDigitos  8765  ==  9876
--    incrementaDigitos   249  ==  350
--    incrementaDigitos    89  ==  90
--    incrementaDigitos    98  ==  9
--    incrementaDigitos    99  ==  0
--    incrementaDigitos     9  ==  0
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

incrementaDigitos :: Integer -> Integer
incrementaDigitos =
  digitosAnumero . map incrementaDigito . digitos

-- (digitos n) es la lista de los dígitos de n. Por ejemplo, 
--    digitos 325  ==  [3,2,5]
digitos :: Integer -> [Integer]
digitos n = [read [c] | c <- show n]

-- (incrementaDigito n) es el número que se obtiene incrementando en una
-- unidad el dígito n (pasando del 9 al 0). Por ejemplo,
--    incrementaDigito 3  ==  4
--    incrementaDigito 9  ==  0
incrementaDigito :: Integer -> Integer
incrementaDigito n = (n + 1) `mod` 10

-- (digitosAnumero ns) es el número correspondiente a la lista de
-- dígitos ns. Por ejemplo,
--    digitosAnumero [3,2,5]  ==  325
digitosAnumero :: [Integer] -> Integer
digitosAnumero = read . concatMap show

-- 2ª definición
-- =============

incrementaDigitos2 :: Integer -> Integer
incrementaDigitos2 n =
  read [incrementaDigito2 c | c <- show n]

-- (incrementaDigito2 c) es el dígito que se obtiene incrementando en una
-- unidad el dígito c (pasando del 9 al 0). Por ejemplo,
--    incrementaDigito '3'  ==  '4'
--    incrementaDigito '9'  ==  '0'
incrementaDigito2 :: Char -> Char  
incrementaDigito2 '9' = '0'
incrementaDigito2 d   = succ d

-- 3ª definición
-- =============

incrementaDigitos3 :: Integer -> Integer
incrementaDigitos3 =
  read . map incrementaDigito2 . show

-- 4ª definición
-- =============

incrementaDigitos4 :: Integer -> Integer
incrementaDigitos4 =
  read . map incrementaDigito4 . show

-- (incrementaDigito4 c) es el dígito que se obtiene incrementando en una
-- unidad el dígito c (pasando del 9 al 0). Por ejemplo,
--    incrementaDigito '3'  ==  '4'
--    incrementaDigito '9'  ==  '0'
incrementaDigito4 :: Char -> Char
incrementaDigito4 c =
  head [y | (x,y) <- zip "0123456789" "1234567890"
          , x == c]

-- ---------------------------------------------------------------------
-- Ejercicio 3. La distancia Manhattan entre dos listas de números es la
-- suma de las diferencias en valor absoluto entre los pares de
-- elementos que ocupan la misma posición, descartando los elementos
-- desparejados. Por ejemplo, la distancia Manhattan entre las listas
-- [1,5,9] y [8,7,6,4] es |1-8|+|5-7|+|9-6| = 12, pues el 4 se descarta
-- ya que está desparejado. 
--
-- Definir la función
--    distanciaManhattan :: [Int] -> [Int] -> Int
-- tal que (distanciaManhattan xs ys) es la distancia Manhattan entre
-- las listas xs e ys. Por ejemplo,
--    distanciaManhattan [1,5,9] [8,7,6,4]  ==  12
--    distanciaManhattan [1,-1] [-2,2]      ==  6
--    distanciaManhattan [1,2,3] [2,1]      ==  2
--    distanciaManhattan [3,-2] [3,4,5]     ==  6
--    distanciaManhattan [] [1,2,3]         ==  0
--    distanciaManhattan [1,2] []           ==  0
-- ---------------------------------------------------------------------

-- 1ª definición
distanciaManhattan :: [Int] -> [Int] -> Int
distanciaManhattan [] _          = 0
distanciaManhattan _ []          = 0
distanciaManhattan (x:xs) (y:ys) = abs (x-y) + distanciaManhattan xs ys

-- 2ª definición
distanciaManhattan2 :: [Int] -> [Int] -> Int
distanciaManhattan2 (x:xs) (y:ys) = abs (x-y) + distanciaManhattan xs ys
distanciaManhattan2 _ _           = 0

-- 3ª definición
distanciaManhattan3 :: [Int] -> [Int] -> Int
distanciaManhattan3 xs ys =
  sum [abs (x-y) | (x,y) <- zip xs ys]

-- 4ª definición
distanciaManhattan4 :: [Int] -> [Int] -> Int
distanciaManhattan4 xs ys =
  sum (zipWith (\x y -> abs (x-y)) xs ys)

-- 5ª definición
distanciaManhattan5 :: [Int] -> [Int] -> Int
distanciaManhattan5 =
  (sum .) . zipWith ((abs .) . (-))

-- 6ª definición
distanciaManhattan6 :: [Int] -> [Int] -> Int
distanciaManhattan6 xs ys =
  foldr (\(x,y) r -> abs (x-y) + r) 0 (zip xs ys)
  
-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    ultimoMaximal :: (Int -> Int) -> [Int] -> Int
-- tal que (ultimoMaximal f xs) es el último elemento de la lista no
-- vacía xs donde la función f alcanza un valor máximo. Por ejemplo,
--   ultimoMaximal abs [4,-2,3,-4,-3]  ==  -4
--   ultimoMaximal (^2) [-2,3,2,-3]    ==  -3
--   ultimoMaximal (3-) [1,2,3,4]      ==  1
-- ---------------------------------------------------------------------

-- 1ª definición
ultimoMaximal :: (Int -> Int) -> [Int] -> Int
ultimoMaximal f xs =
  last [x | x <- xs, f x == m]
  where m = maximum [f x | x <- xs]

-- 2ª definición
ultimoMaximal2 :: (Int -> Int) -> [Int] -> Int
ultimoMaximal2 f xs = aux (head xs) xs
  where aux y [] = y
        aux y (x:xs) | f x >= f y = aux x xs
                     | otherwise  = aux y xs

-- 3ª definición
ultimoMaximal3 :: (Int -> Int) -> [Int] -> Int
ultimoMaximal3 f xs =
  foldl (\y x -> if f x >= f y then x else y) (head xs) xs

-- 4ª definición
ultimoMaximal4 :: (Int -> Int) -> [Int] -> Int
ultimoMaximal4 f xs =
  foldl1 (\y x -> if f x >= f y then x else y) xs
