-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 2º examen de evaluación continua (21 de diciembre de 2012)
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función f 
--   f :: Int -> Integer
-- tal que (f k) es el menor número natural x tal que x^k comienza
-- exactamente por k unos. Por ejemplo, 
--   f 3 = 481 
--   f 4 = 1826
-- ---------------------------------------------------------------------

f :: Int -> Integer
f 1 = 1
f k = head [x | x <- [1..], empiezaCon1 k (x^k)]                         

-- (empiezaCon1 k n) si el número x empieza exactamento con k unos. Por
-- ejemplo, 
--    empiezaCon1 3 111461  ==  True
--    empiezaCon1 3 111146  ==  False
--    empiezaCon1 3 114116  ==  False
empiezaCon1 :: Int -> Integer -> Bool
empiezaCon1 k n = length (takeWhile (==1) (cifras n)) == k

-- (cifras n) es la lista de las cifras de n. Por ejemplo,
--    cifras 111321  ==  [1,1,1,3,2,1]
cifras:: Integer -> [Integer]
cifras n = [read [x]| x <- show n]
                         
-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir la función verificaE tal que 
-- (verificaE k ps x) se cumple si x verifica exactamente k propiedades
-- de la lista ps. Por ejemplo,
--    verificaE 2 [(>0),even,odd] 5  ==  True
--    verificaE 1 [(>0),even,odd] 5  ==  False
-- ---------------------------------------------------------------------

verificaE :: Int -> [t -> Bool] -> t -> Bool
verificaE k ps x = length [p | p <- ps, p x] == k

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función verificaA tal que 
-- (verificaA k ps x) se cumple si x verifica, como máximo, k
-- propiedades de la lista ps. Por ejemplo,
--    verificaA 2 [(>10),even,(<20)] 5    == True
--    verificaA 2 [(>0),even,odd,(<20)] 5 == False
-- ---------------------------------------------------------------------

verificaA :: Int -> [t -> Bool] -> t -> Bool
verificaA k ps x = length [p | p <- ps, p x] <= k

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Definir la función verificaE tal que 
-- (verificaE k ps x) se cumple si x verifica, al menos, k propiedades
-- de la lista ps. Por ejemplo,
--    verificaM 2 [(>0),even,odd,(<20)] 5 == True
--    verificaM 4 [(>0),even,odd,(<20)] 5   ==  False
-- ---------------------------------------------------------------------

verificaM :: Int -> [t -> Bool] -> t -> Bool
verificaM k ps x = length [p | p <- ps, p x] >= k

-- Nota: Otra forma de definir las funciones anteriores es la siguiente

verificaE2 k ps x = verifica ps x == k

verificaA2 k ps x = verifica ps x >= k

verificaM2 k ps x = verifica ps x <= k

-- donde (verifica ps x) es el número de propiedades de ps que verifica
-- el elemento x. Por ejemplo,
--    verifica [(>0),even,odd,(<20)] 5   ==  3
verifica ps x = sum [1 | p <- ps, p x] 

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función intercalaDigito tal que
-- (intercalaDigito d n) es el número que resulta de intercalar el
-- dígito d delante de los dígitos de n menores que d. Por ejemplo,
--    intercalaDigito 5 1263709 == 51526537509
--    intercalaDigito 5 6798    == 6798
-- ---------------------------------------------------------------------

intercalaDigito :: Integer -> Integer -> Integer
intercalaDigito d n = listaNumero (intercala d (cifras n))

-- (intercala y xs) es la lista que resulta de intercalar el
-- número y delante de los elementos de xs menores que y. Por ejemplo,
--    intercala 5 [1,2,6,3,7,0,9]  ==  [5,1,5,2,6,5,3,7,5,0,9]
intercala y [] = []
intercala y (x:xs) | x < y     = y : x : intercala y xs
                   | otherwise = x : intercala y xs

-- (listaNumero xs) es el número correspondiente a la lista de dígitos
-- xs. Por ejemplo,
--    listaNumero [5,1,5,2,6,5,3,7,5,0,9]  ==  51526537509
listaNumero :: [Integer] -> Integer
listaNumero xs = sum [x*(10^k) | (x,k) <- zip (reverse xs) [0..n]]
    where n = length xs -1

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. (Problema 302 del Proyecto Euler) Un número natural n
-- es se llama fuerte si p^2 es un divisor de n, para todos los factores
-- primos de n. 
-- 
-- Definir la función 
--    esFuerte :: Int -> Bool
-- tal que (esFuerte n) se verifica si n es fuerte. Por ejemplo,
--    esFuerte 800      == True
--    esFuerte 24       == False
--    esFuerte 14567429 == False
-- ---------------------------------------------------------------------

-- 1ª definición (directa)
-- =======================

esFuerte :: Int -> Bool
esFuerte n = and [rem n (p*p) == 0 | p <- xs]
    where xs = [p | p <- takeWhile (<=n) primos, rem n p == 0]

-- primos es la lista de los números primos.
primos :: [Int]
primos = 2 : [x | x <- [3,5..], esPrimo x]

-- (esPrimo x) se verifica si x es primo. Por ejemplo,
--    esPrimo 7  ==  True
--    esPrimo 9  ==  False
esPrimo :: Int -> Bool
esPrimo x = [n | n <- [1..x], rem x n == 0] == [1,x]
 
-- 2ª definición (usando la factorización de n)
-- ============================================

esFuerte2 :: Int -> Bool
esFuerte2 n = and [rem n (p*p) == 0 | (p,_) <- factorizacion n]

-- (factorización n) es la factorización de n. Por ejemplo,
--    factorizacion 300  ==  [(2,2),(3,1),(5,2)]
factorizacion :: Int -> [(Int,Int)]
factorizacion n = 
    [(head xs, fromIntegral (length xs)) | xs <- group (factorizacion' n)]

-- (factorizacion' n) es la lista de todos los factores primos de n; es
-- decir, es una lista de números primos cuyo producto es n. Por ejemplo,
--    factorizacion 300  ==  [2,2,3,5,5]
factorizacion' :: Int -> [Int]
factorizacion' n | n == 1    = []
                 | otherwise = x : factorizacion' (div n x)
                 where x = menorFactor n

-- (menorFactor n) es el menor factor primo de n. Por ejemplo,
--    menorFactor 15  ==  3
--    menorFactor 16  ==  2
--    menorFactor 17  == 17
menorFactor :: Int -> Int
menorFactor n = head [x | x <- [2..], rem n x == 0]

-- Comparación de eficiencia:
-- ==========================

--    ghci> :set +s
--    ghci> esFuerte 14567429
--    False
--    (0.90 secs, 39202696 bytes)
--    ghci> esFuerte2 14567429
--    False
--    (0.01 secs, 517496 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función 
--    esPotencia:: Int -> Bool
-- tal que (esPotencia n) se verifica si n es potencia de algún número
-- entero. Por ejemplo, 
--   esPotencia 81   == True
--   esPotencia 1234 == False
-- ---------------------------------------------------------------------

-- 1ª definición:
-- ==============

esPotencia:: Int -> Bool
esPotencia n = esPrimo n || or [esPotenciaDe n m | m  <- [0..n-1]]

-- (esPotenciaDe n m) se verifica si n es una potencia de m. Por
-- ejemplo, 
--    esPotenciaDe 16 2  ==  True
--    esPotenciaDe 24 2  ==  False
esPotenciaDe:: Int -> Int -> Bool
esPotenciaDe n m = or [m^k == n | k <- [0..n]]

-- 2ª definición
-- =============

esPotencia2 :: Int -> Bool
esPotencia2 1 = True
esPotencia2 n = or [esPotenciaDe2 n m | m <- [2..n-1]]

-- (esPotenciaDe2 n m) se verifica si n es una potencia de m. Por
-- ejemplo, 
--    esPotenciaDe2 16 2  ==  True
--    esPotenciaDe2 24 2  ==  False
esPotenciaDe2 :: Int -> Int -> Bool
esPotenciaDe2 n 1 = n == 1
esPotenciaDe2 n m = aux 1
  where aux k | y == n    = True
              | y > n     = False
              | otherwise = aux (k+1)
              where y = m^k
-- 3ª definición
-- =============

esPotencia3 :: Int -> Bool
esPotencia3 n = todosIguales [x | (_,x) <- factorizacion n]

-- (todosIguales xs) se verifica si todos los elementos de xs son
-- iguales. Por ejemplo,
--    todosIguales [2,2,2]  ==  True
--    todosIguales [2,3,2]  ==  False
todosIguales :: [Int] -> Bool
todosIguales []       = True
todosIguales [_]      = True
todosIguales (x:y:xs) = x == y && todosIguales (y:xs)

-- Comparación de eficiencia
-- =========================

--    ghci> :set +s
--    ghci> esPotencia 1234
--    False
--    (16.87 secs, 2476980760 bytes)
--    ghci> esPotencia2 1234
--    False
--    (0.03 secs, 1549232 bytes)
--    ghci> esPotencia3 1234
--    True
--    (0.01 secs, 520540 bytes)
  
-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Un número natural se llama número de Aquiles si es
-- fuerte, pero no es una potencia perfecta; es decir, no es potencia de
-- un número. Por ejemplo, 864 y 1800 son números de Aquiles, pues 
-- 864 = 2^5·3^3 y 1800 = 2^3·3^2·5^2.
-- 
-- Definir la función 
--    esAquileo:: Int -> Bool
-- tal que (esAquileo n) se verifica si n es fuerte y no es potencia
-- perfecta. Por ejemplo,
--    esAquileo 864  ==  True
--    esAquileo 865  ==  False
-- ---------------------------------------------------------------------
          
-- 1ª definición:
esAquileo :: Int -> Bool
esAquileo n = esFuerte n && not (esPotencia n)

-- 2ª definición:
esAquileo2 :: Int -> Bool
esAquileo2 n = esFuerte2 n && not (esPotencia2 n)

-- 3ª definición:
esAquileo3 :: Int -> Bool
esAquileo3 n = esFuerte2 n && not (esPotencia3 n)

-- Comparación de eficiencia
-- =========================

--    ghci> take 10 [n | n <- [1..], esAquileo n]
--    [72,108,200,288,392,432,500,648,675,800]
--    (24.69 secs, 3495004684 bytes)
--    ghci> take 10 [n | n <- [1..], esAquileo2 n]
--    [72,108,200,288,392,432,500,648,675,800]
--    (0.32 secs, 12398516 bytes)
--    ghci> take 10 [n | n <- [1..], esAquileo3 n]
--    [72,108,144,200,288,324,392,400,432,500]
--    (0.12 secs, 3622968 bytes)
