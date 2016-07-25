-- Informática (1º del Grado en Matemáticas y en Física)
-- 1º examen de evaluación continua (4 de noviembre de 2013)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Un año es bisiesto si, o bien es divisible por 4 pero no
-- por 100, o bien es divisible por 400. En cualquier otro caso, no lo
-- es. 
-- 
-- Definir el predicado
--    bisiesto :: Int -> Bool
-- tal que (bisiesto a) se verifica si a es un año bisiesto. Por ejemplo:
--    bisiesto 2013 == False        bisiesto 2012 == True
--    bisiesto 1700 == False        bisiesto 1600 == True  
-- ---------------------------------------------------------------------

-- 1ª definición:
bisiesto :: Int -> Bool
bisiesto x = (mod x 4 == 0 && mod x 100 /= 0) || mod x 400 == 0

-- 2ª definición (con guardas):
bisiesto2 :: Int -> Bool
bisiesto2 x | mod x 4 == 0 && mod x 100 /= 0 = True
            | mod x 400 == 0                 = True
            | otherwise                      = False

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función
--    entre :: Int -> Int -> [Int]
-- tal que (entre a b) devuelve la lista de todos los bisiestos entre 
-- los años a y b. Por ejemplo:
--    entre 2000 2019 == [2000,2004,2008,2012,2016]
-- ---------------------------------------------------------------------

entre :: Int -> Int -> [Int]
entre a b = [x | x <- [a..b], bisiesto x]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir el predicado 
--    coprimos :: (Int,Int) -> Bool
-- tal que (coprimos (a,b)) se verifica si a y b son primos entre sí;
-- es decir, no tienen ningún factor primo en común. Por ejemplo,
--    coprimos (12,25) == True
--    coprimos (6,21)  == False
--    coprimos (1,5)   == True 
-- Calcular todos los números de dos cifras coprimos con 30.
-- ---------------------------------------------------------------------

-- 1ª definición
coprimos :: (Int,Int) -> Bool
coprimos (a,b) = and [mod a x /= 0 | x <- factores b]
    where factores x = [z | z <- [2..x], mod x z == 0]

-- 2º definición
coprimos2 :: (Int,Int) -> Bool
coprimos2 (a,b) = gcd a b == 1 

-- El cálculo es
--    ghci> [x | x <- [10..99], coprimos (x,30)]
--    [11,13,17,19,23,29,31,37,41,43,47,49,53,59,61,67,71,73,77,79,83,89,91,97]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    longCamino :: [(Float,Float)] -> Float
-- tal que (longCamino xs) es la longitud del camino determinado por los
-- puntos del plano listados en xs. Por ejemplo,
--    longCamino [(0,0),(1,0),(2,1),(2,0)] == 3.4142137
-- ---------------------------------------------------------------------

longCamino :: [(Float,Float)] -> Float
longCamino xs = 
    sum [sqrt ((a-c)^2+(b-d)^2)| ((a,b),(c,d)) <- zip xs (tail xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Se quiere poner en marcha un nuevo servicio de correo
-- electrónico. Se requieren las siguientes condiciones para las 
-- contraseñas: deben contener un mínimo de 8 caracteres, al menos deben
-- contener dos números, y al menos deben contener una letra
-- mayúscula. Se asume que el resto de caracteres son letras del
-- abecedario sin tildes. 
-- 
-- Definir la función 
--    claveValida :: String -> Bool 
-- tal que (claveValida xs) indica si la contraseña es válida. Por
-- ejemplo, 
--    claveValida "EstoNoVale" == False
--    claveValida "Tampoco7"   == False
--    claveValida "SiVale23"   == True
-- ---------------------------------------------------------------------

claveValida :: String -> Bool
claveValida xs = 
    length xs >= 8 && 
    length [x | x <- xs, x `elem` ['0'..'9']] > 1 && 
    [x | x <- xs, x `elem` ['A'..'Z']] /= []

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función
--    media :: [String] -> Float
-- tal que (media xs) es la media de las longitudes de las contraseñas
-- válidas de xs. Por ejemplo, 
--    media ["EstoNoVale","Tampoco7","SiVale23","grAnada1982"] == 9.5
-- Indicación: Usar fromIntegral.
-- ---------------------------------------------------------------------

media :: [String] -> Float
media xss = 
    fromIntegral (sum [length xs | xs <- validas]) / fromIntegral (length validas)
    where validas = [xs | xs <- xss, claveValida xs]
       
