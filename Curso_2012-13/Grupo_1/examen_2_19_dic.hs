-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 2º examen de evaluación continua (19 de diciembre de 2012)
-- ---------------------------------------------------------------------
 
import Test.QuickCheck
 
-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por comprensión, la función 
--    maximaDiferenciaC :: [Integer] -> Integer
-- tal que (maximaDiferenciaC xs) es la mayor de las diferencias en
-- valor absoluto entre elementos consecutivos de la lista xs. Por
-- ejemplo,  
--    maximaDiferenciaC [2,5,-3]           ==  8
--    maximaDiferenciaC [1,5]              == 4
--    maximaDiferenciaC [10,-10,1,4,20,-2] == 22
-- ---------------------------------------------------------------------
 
maximaDiferenciaC :: [Integer] -> Integer
maximaDiferenciaC xs = 
    maximum [abs (x-y) | (x,y) <- zip xs (tail xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, por recursión, la función 
--    maximaDiferenciaR :: [Integer] -> Integer
-- tal que (maximaDiferenciaR xs) es la mayor de las diferencias en
-- valor absoluto entre elementos consecutivos de la lista xs. Por
-- ejemplo,  
--    maximaDiferenciaR [2,5,-3]           ==  8
--    maximaDiferenciaR [1,5]              == 4
--    maximaDiferenciaR [10,-10,1,4,20,-2] == 22
-- ---------------------------------------------------------------------
 
maximaDiferenciaR :: [Integer] -> Integer
maximaDiferenciaR [x,y]    = abs (x - y)
maximaDiferenciaR (x:y:ys) = max (abs (x-y)) (maximaDiferenciaR (y:ys))
 
-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comprobar con QuickCheck que las definiciones
-- maximaDiferenciaC y maximaDiferenciaR son equivalentes. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_maximaDiferencia :: [Integer] -> Property
prop_maximaDiferencia xs = 
    length xs > 1 ==> maximaDiferenciaC xs == maximaDiferenciaR xs
 
-- La comprobación es
--    ghci> quickCheck prop_maximaDiferencia
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir, por comprensión, la función acumuladaC tal
-- que (acumuladaC xs) es la lista que tiene en cada posición i el valor
-- que resulta de sumar los elementos de la lista xs desde la posicion 0 
-- hasta la i. Por ejemplo,
--    acumuladaC [2,5,1,4,3] == [2,7,8,12,15]
--    acumuladaC [1,-1,1,-1] == [1,0,1,0]
-- ---------------------------------------------------------------------

acumuladaC xs = [sum (take n xs) | n <- [1..length xs]]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, por recursión, la función acumuladaR tal que
-- (acumuladaR xs) es la lista que tiene en cada posición i el valor que
-- resulta de sumar los elementos de la lista xs desde la posicion 0
-- hasta la i. Por ejemplo,
--    acumuladaR [2,5,1,4,3] == [2,7,8,12,15]
--    acumuladaR [1,-1,1,-1] == [1,0,1,0]
-- ---------------------------------------------------------------------

-- 1ª definición:
acumuladaR [] =  []
acumuladaR xs =  acumuladaR (init xs) ++ [sum xs]

-- 2ª definición:
acumuladaR2 [] = []
acumuladaR2 (x:xs) = reverse (aux xs [x])
    where aux [] ys = ys
          aux (x:xs) (y:ys) = aux xs (x+y:y:ys)

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función unitarios tal (unitarios n) es
-- la lista de números [n,nn, nnn, ....]. Por ejemplo. 
--    take 7 (unitarios 3) == [3,33,333,3333,33333,333333,3333333]
--    take 3 (unitarios 1) == [1,11,111]
-- ---------------------------------------------------------------------

unitarios x = [x*(div (10^n-1) 9)| n <- [1 ..]]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función multiplosUnitarios tal que
-- (multiplosUnitarios x y n) es la lista de los n primeros múltiplos de
-- x cuyo único dígito es y. Por ejemplo,
--    multiplosUnitarios 7 1 2  == [111111,111111111111]
--    multiplosUnitarios 11 3 5 == [33,3333,333333,33333333,3333333333]
-- ---------------------------------------------------------------------

multiplosUnitarios x y n = take n [z | z <- unitarios y, mod z x == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir, por recursión, la función inicialesDistintosR 
-- tal que (inicialesDistintosR xs) es el número de elementos que hay en
-- xs antes de que aparezca el primer repetido. Por ejemplo,
--    inicialesDistintosR [1,2,3,4,5,3] == 2
--    inicialesDistintosR [1,2,3]       == 3
--    inicialesDistintosR "ahora"       == 0
--    inicialesDistintosR "ahorA"       == 5
-- ---------------------------------------------------------------------

inicialesDistintosR [] = 0
inicialesDistintosR (x:xs) 
    | elem x xs = 0 
    | otherwise = 1 + inicialesDistintosR xs

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir, por comprensión, la función
-- inicialesDistintosC tal que (inicialesDistintosC xs) es el número de
-- elementos que hay en xs antes de que aparezca el primer repetido. Por
-- ejemplo, 
--    inicialesDistintosC [1,2,3,4,5,3] == 2
--    inicialesDistintosC [1,2,3]       == 3
--    inicialesDistintosC "ahora"       == 0
--    inicialesDistintosC "ahorA"       == 5
-- ---------------------------------------------------------------------

inicialesDistintosC xs = 
    length (takeWhile (==1) (listaOcurrencias xs))

-- (listaOcurrencias xs) es la lista con el número de veces que aparece
-- cada elemento de xs en xs. Por ejemplo,
--    listaOcurrencias [1,2,3,4,5,3]    ==  [1,1,2,1,1,2]
--    listaOcurrencias "repetidamente"  ==  [1,4,1,4,2,1,1,1,1,4,1,2,4]
listaOcurrencias xs = [ocurrencias x xs | x <- xs]

-- (ocurrencias x ys) es el número de ocurrencias de x en ys. Por
-- ejemplo, 
--    ocurrencias 1 [1,2,3,1,5,3,3]  ==  2
--    ocurrencias 3 [1,2,3,1,5,3,3]  ==  3
ocurrencias x ys = length [y | y <- ys, x == y]
