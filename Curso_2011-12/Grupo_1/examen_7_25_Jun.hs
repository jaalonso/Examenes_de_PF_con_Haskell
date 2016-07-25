-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 7º examen de evaluación continua (25 de junio de 2012)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. [2 puntos] Definir la función
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

-- 2ª definición (sin recursión):
ceros2 :: Int -> Int
ceros2 n = length (takeWhile (=='0') (reverse (show n))) 

-- ---------------------------------------------------------------------
-- Ejercicio 2. [2 puntos] Definir la función
--    superpar :: Int -> Bool
-- tal que (superpar n) se verifica si n es un número par tal que todos
-- sus dígitos son pares. Por ejemplo,
--    superpar 426  ==  True
--    superpar 456  ==  False
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión)
superpar :: Int -> Bool
superpar n | n < 10    = even n
           | otherwise = even n && superpar (n `div` 10)

-- 2ª definición (por comprensión):
superpar2 :: Int -> Bool
superpar2 n = and [even d | d <- digitos n]

digitos :: Int -> [Int]
digitos n = [read [d] | d <- show n]

-- 3ª definición (por recursión sobre los dígitos):
superpar3 :: Int -> Bool
superpar3 n = sonPares (digitos n)
    where sonPares []     = True
          sonPares (d:ds) = even d && sonPares ds

-- la función sonPares se puede definir por plegado:
superpar3' :: Int -> Bool
superpar3' n = sonPares (digitos n)
    where sonPares ds = foldr ((&&) . even) True ds

-- 4ª definición (con all):
superpar4 :: Int -> Bool
superpar4 n = all even (digitos n)

-- 5ª definición (con filter):
superpar5 :: Int -> Bool
superpar5 n = filter even (digitos n) == digitos n

-- ---------------------------------------------------------------------
-- Ejercicio 3. [2 puntos] Definir la función
--    potenciaFunc :: Int -> (a -> a) -> a -> a
-- tal que (potenciaFunc n f x) es el resultado de aplicar n veces la
-- función f a x. Por ejemplo,
--    potenciaFunc 3 (*10) 5  ==  5000
--    potenciaFunc 4 (+10) 5  ==  45
-- ---------------------------------------------------------------------

potenciaFunc :: Int -> (a -> a) -> a -> a
potenciaFunc 0 _ x = x
potenciaFunc n f x = potenciaFunc (n-1) f (f x)

-- 2ª definición (con iterate):
potenciaFunc2 :: Int -> (a -> a) -> a -> a
potenciaFunc2 n f x = last (take (n+1) (iterate f x)) 

-- ---------------------------------------------------------------------
-- Ejercicio 4. [2 puntos] Las expresiones aritméticas con una variable
-- (denotada por X) se pueden representar mediante el siguiente tipo 
--    data Expr = Num Int
--              | Suma Expr Expr
--              | X
-- Por ejemplo, la expresión "X+(13+X)" se representa por
-- "Suma X (Suma (Num 13) X)".
-- 
-- Definir la función
--    numVars :: Expr -> Int
-- tal que (numVars e) es el número de variables en la expresión e. Por
-- ejemplo, 
--    numVars (Num 3)                     ==  0
--    numVars X                           ==  1
--    numVars (Suma X (Suma (Num 13) X))  ==  2
-- ---------------------------------------------------------------------

data Expr = Num Int
          | Suma Expr Expr
          | X

numVars :: Expr -> Int
numVars (Num n)    = 0
numVars (Suma a b) = numVars a + numVars b
numVars X          = 1

-- ---------------------------------------------------------------------
-- Ejercicio 5. [2 puntos] Cuentan que Alan Turing tenía una bicicleta
-- vieja, que tenía una cadena con un eslabón débil y además uno de los
-- radios de la rueda estaba doblado. Cuando el radio doblado coincidía
-- con el eslabón débil, entonces la cadena se rompía.  
--
-- La bicicleta se identifica por los parámetros (i,d,n) donde 
-- * i es el número del eslabón que coincide con el radio doblado al
--   empezar a andar,
-- * d es el número de eslabones que se desplaza la cadena en cada
--   vuelta de la rueda y  
-- * n es el número de eslabones de la cadena (el número n es el débil).
-- Si i=2 y d=7 y n=25, entonces la lista con el número de eslabón que 
-- toca el radio doblado en cada vuelta es 
--    [2,9,16,23,5,12,19,1,8,15,22,4,11,18,0,7,14,21,3,10,17,24,6,...
-- Con lo que la cadena se rompe en la vuelta número 14.
-- 
-- 1. Definir la función
--       eslabones :: Int -> Int -> Int -> [Int]
--    tal que (eslabones i d n) es la lista con los números de eslabones 
--    que tocan el radio doblado en cada vuelta en una bicicleta de tipo 
--    (i,d,n). Por ejemplo, 
--       take 10 (eslabones 2 7 25)  ==  [2,9,16,23,5,12,19,1,8,15]
-- 
-- 2. Definir la función
--       numeroVueltas :: Int -> Int -> Int -> Int 
--    tal que (numeroVueltas i d n) es el número de vueltas que pasarán 
--    hasta que la cadena se rompa en una bicicleta de tipo (i,d,n). Por 
--    ejemplo,
--       numeroVueltas 2 7 25  ==  14
-- ---------------------------------------------------------------------

eslabones :: Int -> Int -> Int -> [Int]
eslabones i d n = [(i+d*j) `mod` n | j <- [0..]]

-- 2ª definición (con iterate):
eslabones2 :: Int -> Int -> Int -> [Int]
eslabones2 i d n = map (`mod` n) (iterate (+d) i)

numeroVueltas :: Int -> Int -> Int -> Int
numeroVueltas i d n = length (takeWhile (/=0) (eslabones i d n)) 
