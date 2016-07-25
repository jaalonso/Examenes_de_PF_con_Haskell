-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 5º examen de evaluación continua (14 de marzo de 2011)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Se consideran las funciones
--    duplica  :: [a] -> [a]
--    longitud :: [a] -> Int
-- tales que
--    (duplica xs) es la lista obtenida duplicando los elementos de xs y
--    (longitud xs) es el número de elementos de xs.
-- Por ejemplo,
--    duplica  [7,2,5]  ==  [7,7,2,2,5,5]
--    longitud [7,2,5]  ==  3
-- 
-- Las definiciones correspondientes son
--    duplica [] = []                     -- duplica.1
--    duplica (x:xs) = x:x:duplica xs     -- duplica.2
--    
--    longitud [] = 0                     -- longitud.1
--    longitud (x:xs) = 1 + longitud xs   -- longitud.2
-- Demostrar por inducción que 
--    longitud (duplica xs) = 2 * longitud xs
-- ---------------------------------------------------------------------

{-
 Demostración: Hay que demostrar que
    longitud (duplica xs) = 2 * longitud xs
 Lo haremos por inducción en xs.

 Caso base: Hay que demostrar que
    longitud (duplica []) = 2 * longitud []
 En efecto
    longitud (duplica xs) 
    = longitud []            [por duplica.1]
    = 0                      [por longitud.1]
    = 2 * 0                  [por aritmética]
    = longitud []            [por longitud.1]

 Paso de inducción: Se supone la hipótesis de inducción
    longitud (duplica xs) = 2 * longitud xs
 Hay que demostrar que
    longitud (duplica (x:xs)) = 2 * longitud (x:xs)
 En efecto,
    longitud (duplica (x:xs))
    = longitud (x:x:duplica xs)       [por duplica.2]
    = 1 + longitud (x:duplica xs)     [por longitud.2]
    = 1 + 1 + longitud (duplica xs)   [por longitud.2]
    = 1 + 1 + 2*(longitud xs)         [por hip. de inducción]
    = 2 * (1 + longitud xs)           [por aritmética]
    = 2 * longitud (x:xs)             [por longitud.2]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 2. Las expresiones aritméticas pueden representarse usando
-- el siguiente tipo de datos 
--    data Expr = N Int | S Expr Expr | P Expr Expr  
--              deriving Show
-- Por ejemplo, la expresión 2*(3+7) se representa por
--    P (N 2) (S (N 3) (N 7))
-- 
-- Definir la función
--    valor :: Expr -> Int                   
-- tal que (valor e) es el valor de la expresión aritmética e. Por
-- ejemplo, 
--    valor (P (N 2) (S (N 3) (N 7)))  ==  20
-- ---------------------------------------------------------------------

data Expr = N Int | S Expr Expr | P Expr Expr  
          deriving Show
                   
valor :: Expr -> Int                   
valor (N x)   = x 
valor (S x y) = valor x + valor y
valor (P x y) = valor x * valor y

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función 
--    esFib :: Int -> Bool
-- tal que (esFib x) se verifica si existe un número n tal que x es el
-- n-ésimo término de la sucesión de Fibonacci. Por ejemplo, 
--    esFib 89  ==  True
--    esFib 69  ==  False
-- ---------------------------------------------------------------------

esFib :: Int -> Bool
esFib n = n == head (dropWhile (<n) fibs)

-- fibs es la sucesión de Fibonacci. Por ejemplo,
--    take 10 fibs  ==  [0,1,1,2,3,5,8,13,21,34]
fibs :: [Int]
fibs = 0:1:[x+y | (x,y) <- zip fibs (tail fibs)]

-- ---------------------------------------------------------------------
-- Ejercicio 4 El ejercicio 4 de la Olimpiada Matemáticas de 1993 es el
-- siguiente: 
--    Demostrar que para todo número primo p distinto de 2 y de 5,
--    existen infinitos múltiplos de p de la forma 1111......1 (escrito
--    sólo con unos).
--   
-- Definir la función
--    multiplosEspeciales :: Integer -> Int -> [Integer]
-- tal que (multiplosEspeciales p n) es una lista de n múltiplos p de la
-- forma 1111...1 (escrito sólo con unos), donde p es un número primo
-- distinto de 2 y 5. Por ejemplo,
--    multiplosEspeciales 7 2  ==  [111111,111111111111]
-- ---------------------------------------------------------------------

-- 1ª definición
multiplosEspeciales :: Integer -> Int -> [Integer]
multiplosEspeciales p n = take n [x | x <- unos, mod x p == 0]

-- unos es la lista de los números de la forma 111...1 (escrito sólo con
-- unos). Por ejemplo,
--    take 5 unos  ==  [1,11,111,1111,11111]
unos :: [Integer]
unos = 1 : [10*x+1 | x <- unos]

-- Otra definición no recursiva de unos es
unos2 :: [Integer]
unos2 = [div (10^n-1) 9 | n <- [1..]]

-- 2ª definición (sin usar unos)
multiplosEspeciales2 :: Integer -> Int -> [Integer]
multiplosEspeciales2 p n = 
    [div (10^((p-1)*x)-1) 9 | x <- [1..fromIntegral n]]

