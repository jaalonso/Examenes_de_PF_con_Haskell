-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 1º examen de evaluación continua (30 de noviembre de 2009)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir, por recursión ,la función
--    sumaFactR :: Int -> Int
-- tal que (sumaFactR n) es la suma de los factoriales de los números
-- desde 0 hasta n. Por ejemplo,
--    sumaFactR 3  ==  10
-- ---------------------------------------------------------------------

sumaFactR :: Int -> Int
sumaFactR 0     = 1
sumaFactR (n+1) = factorial (n+1) + sumaFactR n

-- (factorial n) es el factorial de n. Por ejemplo,
--    factorial 4  ==  24
factorial n = product [1..n]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir, por comprensión, la función
--    sumaFactC :: Int -> Int
-- tal que (sumaFactC n) es la suma de los factoriales de los números
-- desde 0 hasta n. Por ejemplo,
--    sumaFactC 3  ==  10
-- ---------------------------------------------------------------------

sumaFactC :: Int -> Int
sumaFactC n = sum [factorial x | x <- [0..n]] 

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir, por recursión, la función
--    copia :: [a] -> Int -> [a]
-- tal que (copia xs n) es la lista obtenida copiando n veces la lista
-- xs. Por ejemplo,
--    copia "abc" 3  ==  "abcabcabc"
-- ---------------------------------------------------------------------

copia :: [a] -> Int -> [a]
copia xs 0 = []
copia xs n = xs ++ copia xs (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir, por recursión, la función
--    incidenciasR :: Eq a => a -> [a] -> Int
-- tal que (incidenciasR x ys) es el número de veces que aparece el
-- elemento x en la lista ys. Por ejemplo,
--    incidenciasR 3 [7,3,5,3]  ==  2
-- ---------------------------------------------------------------------

incidenciasR :: Eq a => a -> [a] -> Int
incidenciasR _ []                 = 0
incidenciasR x (y:ys) | x == y    = 1 + incidenciasR x ys
                      | otherwise = incidenciasR x ys

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir, por comprensión, la función
--    incidenciasC :: Eq a => a -> [a] -> Int
-- tal que (incidenciasC x ys) es el número de veces que aparece el
-- elemento x en la lista ys. Por ejemplo,
--    incidenciasC 3 [7,3,5,3]  ==  2
-- ---------------------------------------------------------------------

incidenciasC :: Eq a => a -> [a] -> Int
incidenciasC x ys = length [y | y <- ys, y == x]
