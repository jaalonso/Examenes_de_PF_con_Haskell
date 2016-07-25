-- Informática (1º del Grado en Matemáticas)
-- 1º examen de evaluación continua (8 de noviembre de 2012)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función primosEntre tal que (primosEntre x y)
-- es la lista de los número primos entre x e y (ambos inclusive). Por
-- ejemplo, 
--    primosEntre 11 44  ==  [11,13,17,19,23,29,31,37,41,43]
-- ---------------------------------------------------------------------

primosEntre x y = [n | n <- [x..y], primo n]

-- (primo x) se verifica si x es primo. Por ejemplo,
--    primo 30  == False
--    primo 31  == True  
primo n = factores n == [1, n]

-- (factores n) es la lista de los factores del número n. Por ejemplo,
--    factores 30  \valor  [1,2,3,5,6,10,15,30]  
factores n = [x | x <- [1..n], n `mod` x == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función posiciones tal que (posiciones x ys)
-- es la lista de las posiciones ocupadas por el elemento x en la lista
-- ys. Por ejemplo, 
--    posiciones 5 [1,5,3,5,5,7]  ==  [1,3,4]
--    posiciones 'a' "Salamanca"  ==  [1,3,5,8]
-- ---------------------------------------------------------------------

posiciones x xs = [i | (x',i) <- zip xs [0..], x == x']

-- ---------------------------------------------------------------------
-- Ejercicio 3. El tiempo se puede representar por pares de la forma
-- (m,s) donde m representa los minutos y s los segundos. Definir la
-- función duracion tal que (duracion t1 t2) es la duración del
-- intervalo de tiempo que se inicia en t1 y finaliza en t2. Por
-- ejemplo, 
--    duracion (2,15) (6,40)  ==  (4,25)
--    duracion (2,40) (6,15)  ==  (3,35)
-- ---------------------------------------------------------------------

tiempo (m1,s1) (m2,s2)
       | s1 <= s2  = (m2-m1,s2-s1)
       | otherwise = (m2-m1-1,60+s2-s1)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función cortas tal que (cortas xs) es la
-- lista de las palabras más cortas (es decir, de menor longitud) de la
-- lista xs. Por ejemplo,
--    ghci> cortas ["hoy", "es", "un", "buen", "dia", "de", "sol"]
--    ["es","un","de"]
-- ---------------------------------------------------------------------

cortas xs = [x | x <- xs, length x == n]
    where n = minimum [length x | x <- xs]
