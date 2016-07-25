-- Informática (1º del Grado en Matemáticas y en Física)
-- 2º examen de evaluación continua (16 de diciembre de 2013)
-- ---------------------------------------------------------------------

import Data.Char

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir las funciones  
--    ultima, primera :: Int -> Int  
-- que devuelven, respectivamente, la última y la primera cifra de un  
-- entero positivo, Por ejemplo:
--    ultima  711 = 1         
--    primera 711 = 7
-- ---------------------------------------------------------------------

ultima, primera :: Int -> Int  
ultima n  = n `rem` 10
primera n = read [head (show n)]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, por recursión, el predicado 
--    encadenadoR :: [Int] -> Bool
-- tal que (encadenadoR xs) se verifica si xs es una lista de
-- enteros positivos encadenados (es decir, la última cifra de cada 
-- número coincide con la primera del siguiente en la lista). Por ejemplo:
--   encadenadoR [711,1024,413,367]  ==  True
--   encadenadoR [711,1024,213,367]  ==  False
-- ---------------------------------------------------------------------

encadenadoR :: [Int] -> Bool
encadenadoR (x:y:zs) = ultima x == primera y && encadenadoR (y:zs)
encadenadoR _        = True

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir, por comprensión, el predicado 
--    encadenadoC :: [Int] -> Bool
-- tal que (encadenadoC xs) se verifica si xs es una lista de
-- enteros positivos encadenados (es decir, la última cifra de cada 
-- número coincide con la primera del siguiente en la lista). Por ejemplo:
--   encadenadoC [711,1024,413,367]  ==  True
--   encadenadoC [711,1024,213,367]  ==  False
-- ---------------------------------------------------------------------

encadenadoC :: [Int] -> Bool
encadenadoC xs = and [ultima x == primera y | (x,y) <- zip xs (tail xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Un entero positivo se dirá semiperfecto si puede
-- obtenerse como la suma de un subconjunto de sus divisores propios (no
-- necesariamente todos). Por ejemplo, 18 es semiperfecto, pues sus
-- divisores propios son 1, 2, 3, 6 y 9 y además 18 = 1+2+6+9. 
--
-- Define el predicado 
--    semiperfecto :: Int -> Bool
-- tal que (semiperfecto x) se verifica si x es semiperfecto. Por
-- ejemplo, 
--   semiperfecto 18  ==  True
--   semiperfecto 15  ==  False
-- ---------------------------------------------------------------------

semiperfecto :: Int -> Bool
semiperfecto n = not (null (sublistasConSuma (divisores n) n))

-- (divisores x) es la lista de los divisores propios de x. Por ejemplo, 
--   divisores 18  ==  [1,2,3,6,9]
divisores :: Int -> [Int]
divisores x = [y | y <- [1..x-1], x `rem` y == 0]

-- (sublistasConSuma xs n) es la lista de las sublistas de la lista de
-- números naturales xs que suman n. Por ejemplo,
--   sublistasConSuma [1,2,3,6,9] 18  ==  [[1,2,6,9],[3,6,9]]
sublistasConSuma :: [Int] -> Int -> [[Int]]
sublistasConSuma [] 0 = [[]]
sublistasConSuma [] _ = []
sublistasConSuma (x:xs) n 
    | x > n = sublistasConSuma xs n
    | otherwise = [x:ys | ys <- sublistasConSuma xs (n-x)] ++
                  sublistasConSuma xs n

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función 
--    semiperfectos :: Int -> [Int] 
-- tal que (semiperfectos n) es la lista de los n primeros números
-- semiperfectos. Por ejemplo: 
--    semiperfectos 10 == [6,12,18,20,24,28,30,36,40,42]
-- ---------------------------------------------------------------------

semiperfectos :: Int -> [Int] 
semiperfectos n = take n [x | x <- [1..], semiperfecto x]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Formatear una cadena de texto consiste en:
--    1. Eliminar los espacios en blanco iniciales.
--    2. Eliminar los espacios en blanco finales.
--    3. Reducir a 1 los espacios en blanco entre palabras.
--    4. Escribir la primera letra en mayúsculas, si no lo estuviera.
-- 
-- Definir la función 
--    formateada :: String -> String 
-- tal que (formateada cs) es la cadena cs formateada. Por ejemplo,
--    formateada "   la   palabra   precisa "  ==  "La palabra precisa"
-- ---------------------------------------------------------------------

formateada :: String -> String
formateada cs = toUpper x : xs
    where (x:xs) = unwords (words cs)

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. El centro de masas de un sistema discreto es el punto
-- geométrico que dinámicamente se comporta como si en él estuviera
-- aplicada la resultante de las fuerzas externas al sistema. 
-- 
-- Representamos un conjunto de n masas en el plano mediante una lista
-- de n pares de la forma ((ai,bi),mi) donde (ai,bi) es la posición y mi
-- la masa puntual. Las coordenadas del centro de masas (a,b) se
-- calculan por
--    a = (a1*m1+a2*m2+ ... an*mn)/(m1+m2+...mn)
--    b = (b1*m1+b2*m2+ ... bn*mn)/(m1+m2+...mn)
--
-- Definir la función 
--    masaTotal ::  [((Float,Float),Float)] -> Float 
-- tal que (masaTotal xs) es la masa total de sistema xs. Por ejemplo,
--    masaTotal  [((-1,3),2),((0,0),5),((1,3),3)]  ==  10
-- ---------------------------------------------------------------------

masaTotal ::  [((Float,Float),Float)] -> Float 
masaTotal xs = sum [m | (_,m) <- xs]

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función 
--    centrodeMasas :: [((Float,Float),Float)] -> (Float,Float) 
-- tal que (centrodeMasas xs) es las coordenadas del centro 
-- de masas del sistema discreto xs. Por ejemplo:
--    centrodeMasas [((-1,3),2),((0,0),5),((1,3),3)] == (0.1,1.5)
-- ---------------------------------------------------------------------

centrodeMasas :: [((Float,Float),Float)] -> (Float,Float) 
centrodeMasas xs = 
    (sum [a*m | ((a,_),m) <- xs] / mt,
     sum [b*m | ((_,b),m) <- xs] / mt)
    where mt = masaTotal xs
