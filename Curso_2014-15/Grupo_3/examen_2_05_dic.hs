-- Informática (1º del Grado en Matemáticas)
-- 1º examen de evaluación continua (5 de diciembre de 2014)
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por comprensión, la función
--    cuentaC :: Ord a => a -> a -> [a] -> Int        
-- tal que (cuentaC x y xs) es el número de elementos de la lista xs que
-- están en el intervalo [x,y]. Por ejemplo,
--    cuentaC 50 150 [12,3456,100,78,711] == 2 
-- ---------------------------------------------------------------------

cuentaC :: Ord a => a -> a -> [a] -> Int
cuentaC x y xs = length [z | z <- xs, x <= z && z <= y]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, usando orden superior (map, filter, ...), la
-- función 
--    cuentaS :: Ord a => a -> a -> [a] -> Int        
-- tal que (cuentaS x y xs) es el número de elementos de la lista xs que
-- están en el intervalo [x,y]. Por ejemplo,
--    cuentaS 50 150 [12,3456,100,78,711] == 2 
-- ---------------------------------------------------------------------

cuentaS ::  Ord a => a -> a -> [a] -> Int
cuentaS x y = length . filter (>=x) . filter (<=y) 

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir, por recursión, la función
--    cuentaR :: Ord a => a -> a -> [a] -> Int        
-- tal que (cuentaR x y xs) es el número de elementos de la lista xs que
-- están en el intervalo [x,y]. Por ejemplo,
--    cuentaR 50 150 [12,3456,100,78,711] == 2 
-- ---------------------------------------------------------------------

cuentaR ::  Ord a => a -> a -> [a] -> Int
cuentaR _ _ [] = 0
cuentaR x y (z:zs) | x <= z && z <= y = 1 + cuentaR x y zs
                   | otherwise        = cuentaR x y zs 

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Definir, por plegado (foldr), la función
--    cuentaP :: Ord a => a -> a -> [a] -> Int        
-- tal que (cuentaP x y xs) es el número de elementos de la lista xs que
-- están en el intervalo [x,y]. Por ejemplo,
--    cuentaP 50 150 [12,3456,100,78,711] == 2 
-- ---------------------------------------------------------------------

-- 1ª definición:
cuentaP ::  Ord a => a -> a -> [a] -> Int
cuentaP x y = foldr (\z u -> if x <= z && z <= y then 1 + u else u) 0

-- 2ª definición:
cuentaP2 ::  Ord a => a -> a -> [a] -> Int
cuentaP2 x y = foldr f 0
    where f z u | x <= z && z <= y = 1 + u 
                | otherwise        = u    

-- ---------------------------------------------------------------------
-- Ejercicio 1.5. Comprobar con QuickCheck que las definiciones de
-- cuenta son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_cuenta :: Int -> Int -> [Int] -> Bool
prop_cuenta x y zs =
    cuentaS x y zs == n &&
    cuentaR x y zs == n &&
    cuentaP x y zs == n 
    where n = cuentaC x y zs

-- La comprobación es
--    ghci> quickCheck prop_cuenta
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    mdp :: Integer -> Integer
-- tal que (mdp x) es el mayor divisor primo del entero positivo x. Por
-- ejemplo, 
--    mdp 100    ==  5     
--    mdp 45     ==  9
--    mdp 12345  ==  823
-- ---------------------------------------------------------------------

mdp :: Integer -> Integer
mdp x = head [i | i <- [x,x-1..2], rem x i == 0, primo i]

primo :: Integer -> Bool
primo x = divisores x == [1,x]
   where divisores x = [y | y <- [1..x], rem x y == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función 
--    busca :: Integer -> Integer -> Integer
-- tal que (busca a b) es el menor entero por encima de a cuyo mayor
-- divisor primo es mayor o igual que b. Por ejemplo, 
--    busca 2014  1000 == 2017
--    busca 2014 10000 == 10007
-- ---------------------------------------------------------------------

busca :: Integer -> Integer -> Integer
busca a b = head [i | i <- [max a b..], mdp i >= b]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Consideramos el predicado
--    comun :: Eq b => (a -> b) -> [a] -> Bool
-- tal que (comun f xs) se verifica si al aplicar la función f a los
-- elementos de xs obtenemos siempre el mismo valor. Por ejemplo,
--    comun (^2) [1,-1,1,-1]                 == True
--    comun (+1) [1,2,1]                     == False
--    comun length ["eva","iba","con","ana"] == True
-- 
-- Definir, por recursión, el predicado comun.
-- ---------------------------------------------------------------------

-- 1ª definición
comunR ::  Eq b => (a -> b) -> [a] -> Bool
comunR f (x:y:xs) = f x == f y && comunR f (y:xs)
comunR _ _        = True

-- 2ª definición:
comunR2 ::  Eq b => (a -> b) -> [a] -> Bool
comunR2 _ [] = True
comunR2 f (x:xs) = aux xs 
    where z          = f x
          aux []     = True
          aux (y:ys) = f y == z && aux ys 

-- Comparación de eficiencia:
--    ghci> comunR (\n -> product [1..n]) (replicate 20 20000)
--    True
--    (39.71 secs, 11731056160 bytes)
--    
--    ghci> comunR2 (\n -> product [1..n]) (replicate 20 20000)
--    True
--    (20.36 secs, 6175748288 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir, por comprensión, el predicado comun.
-- ---------------------------------------------------------------------

-- 1ª definición
comunC ::  Eq b => (a -> b) -> [a] -> Bool
comunC f xs = and [f a == f b | (a,b) <- zip xs (tail xs)]

-- 2ª definición
comunC2 ::  Eq b => (a -> b) -> [a] -> Bool
comunC2 _ []     = True
comunC2 f (x:xs) = and [f y == z | y <- xs]
    where z = f x

-- Comparación de eficiencia:
--    ghci> comunC (\n -> product [1..n]) (replicate 20 20000)
--    True
--    (39.54 secs, 11731056768 bytes)
--    
--    ghci> comunC2 (\n -> product [1..n]) (replicate 20 20000)
--    True
--    (20.54 secs, 6175747048 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    extension :: String -> (String,String)
-- tal que (extension cs) es el par (nombre,extensión) del fichero
-- cs. Por ejemplo, 
--    extension "examen.hs"    ==  ("examen",hs")
--    extension "index.html"   ==  ("index","html")
--    extension "sinExt"       ==  ("sinExt","")
--    extension "raro.pdf.ps"  ==  ("raro","pdf.ps")
-- -------------------------------------------------------------

extension :: String -> (String,String)
extension cs 
    | '.' `notElem` cs = (cs,"")
    | otherwise        = (takeWhile (/='.') cs, tail (dropWhile (/='.') cs)) 

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Un entero positivo x es especial si en x y en x^2
-- aparecen los mismos dígitos. Por ejemplo, 10 es especial porque en 10
-- y en 10^2 = 100 aparecen los mismos dígitos (0 y 1). Asimismo, 
-- 4762 es especial porque en 4762 y en 4762^2 = 22676644 aparecen los 
-- mismos dígitos (2, 4, 6 y 7).
-- 
-- Definir el predicado 
--    especial :: Integer -> Bool
-- tal que (especial x) se verifica si x es un entero positivo especial.
-- ---------------------------------------------------------------------

especial :: Integer -> Bool
especial x = digitos x == digitos (x^2) 
    where digitos z = [d | d <- ['0'..'9'], d `elem` show z]

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir la función 
--    especiales :: Int -> [Integer]
-- tal que (especiales x) es la lista de los x primeros números
-- especiales que no son potencias de 10. Por ejemplo,
--    espaciales 5 == [4762,4832,10376,10493,11205]
-- ---------------------------------------------------------------------

especiales :: Int -> [Integer]
especiales x = take x [i | i <- [1..], not (pot10 i), especial i]
    where pot10 z = z `elem` takeWhile (<= z) (map (10^) [0 ..])

