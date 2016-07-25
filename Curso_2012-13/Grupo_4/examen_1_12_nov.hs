-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 1º examen de evaluación continua (12 de noviembre de 2012)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Dada una ecuación de tercer grado de la forma 
--    x^3 + ax^2 + bx + c = 0, 
-- donde a, b y c son números reales, se define el discriminante de la
-- ecuación como
--    d = 4p^3+ 27q^2, 
-- donde p = b - a^3/3 y q = 2a^3/27 - ab/3 + c.
-- 
-- Definir la función 
--    disc :: Float -> Float -> Float -> Float 
-- tal que (disc a b c) es el discriminante de la ecuación 
-- x^3 + ax^2 + bx + c = 0. Por ejemplo,
--    disc 1 (-11) (-9) == -5075.9995
-- ---------------------------------------------------------------------

disc :: Float -> Float -> Float -> Float 
disc a b c = 4*p^3 + 27*q^2 
    where p = b - (a^3)/3 
          q = (2*a^3)/27 - (a*b)/3 + c

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. El signo del discriminante permite determinar el
-- número de raíces reales de la ecuación: 
--    d > 0 : 1 solución, 
--    d = 0 : 2 soluciones y 
--    d < 0 : 3 soluciones
-- 
-- Definir la función 
--    numSol :: Float -> Float > Float -> Int 
-- tal que (numSol a b c) es el número de raíces reales de la ecuación 
-- x^3 + ax^2 + bx + c = 0. Por ejemplo,
--    numSol 1 (-11) (-9) == 3
-- ---------------------------------------------------------------------

numSol :: Float -> Float -> Float -> Int 
numSol a b c
    | d > 0     = 1
    | d == 0    = 2
    | otherwise = 3
    where d = disc a b c 

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir la función 
--    numDiv :: Int -> Int 
-- tal que (numDiv x) es el número de divisores del número natural
-- x. Por ejemplo, 
--    numDiv 11 == 2 
--    numDiv 12 == 6 
-- ---------------------------------------------------------------------

numDiv :: Int -> Int 
numDiv x = length [n | n <- [1..x], rem x n == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función 
--    entre :: Int -> Int -> Int -> [Int] 
-- tal que (entre a b c) es la lista de los naturales entre a y b con,
-- al menos, c divisores. Por ejemplo, 
--    entre 11 16 5 == [12, 16]
-- ---------------------------------------------------------------------

entre :: Int -> Int -> Int -> [Int] 
entre a b c = [x | x <- [a..b], numDiv x >= c]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función 
--    conPos :: [a] -> [(a,Int)] 
-- tal que (conPos xs) es la lista obtenida a partir de xs especificando
-- las posiciones de sus elementos. Por ejemplo,
--    conPos [1,5,0,7] == [(1,0),(5,1),(0,2),(7,3)]
-- ---------------------------------------------------------------------

conPos :: [a] -> [(a,Int)] 
conPos xs = zip xs [0..]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función 
--    pares :: String -> String 
-- tal que (pares cs) es la cadena formada por los caracteres en
-- posición par de cs. Por ejemplo, 
--    pares "el cielo sobre berlin" == "e il or eln"
-- ---------------------------------------------------------------------

pares :: String -> String 
pares cs = [c | (c,n) <- conPos cs, even n]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir el predicado 
--    comparaFecha :: (Int,String,Int) -> (Int,String,Int) -> Bool 
-- que recibe dos fechas en el formato (dd,“mes”,aaaa) y se verifica si
-- la primera fecha es anterior a la segunda. Por ejemplo:
--    comparaFecha (12, "noviembre", 2012) (01, "enero", 2015) == True
--    comparaFecha (12, "noviembre", 2012) (01, "enero", 2012) == False
-- ---------------------------------------------------------------------

comparaFecha :: (Int,String,Int) -> (Int,String,Int) -> Bool 
comparaFecha (d1,m1,a1) (d2,m2,a2) =
    (a1,mes m1,d1) < (a2,mes m2,d2)
    where mes "enero"      = 1
          mes "febrero"    = 2
          mes "marzo"      = 3
          mes "abril"      = 4
          mes "mayo"       = 5
          mes "junio"      = 6
          mes "julio"      = 7
          mes "agosto"     = 8
          mes "septiembre" = 9
          mes "octubre"    = 10
          mes "noviembre"  = 11
          mes "diciembre"  = 12
