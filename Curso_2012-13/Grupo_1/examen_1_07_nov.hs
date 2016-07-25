-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 1º examen de evaluación continua (7 de noviembre de 2012)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función ocurrenciasDelMaximo tal que
-- (ocurrenciasDelMaximo xs) es el par formado por el mayor de los
-- números de xs y el número de veces que este aparece en la lista
-- xs, si la lista es no vacía y es (0,0) si xs es la lista vacía. Por
-- ejemplo,  
--    ocurrenciasDelMaximo [1,3,2,4,2,5,3,6,3,2,1,8,7,6,5]  ==  (8,1)
--    ocurrenciasDelMaximo [1,8,2,4,8,5,3,6,3,2,1,8]        ==  (8,3)
--    ocurrenciasDelMaximo [8,8,2,4,8,5,3,6,3,2,1,8]        ==  (8,4)
-- ---------------------------------------------------------------------

ocurrenciasDelMaximo [] = (0,0)
ocurrenciasDelMaximo xs = (maximum xs, sum [1 | y <- xs, y == maximum xs])

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir, por comprensión, la función tienenS tal que
-- (tienenS xss) es la lista de las longitudes de las cadenas de xss que
-- contienen el caracter 's' en mayúsculas o minúsculas. Por ejemplo, 
--    tienenS ["Este","es","un","examen","de","hoy","Suerte"]  ==  [4,2,6]
--    tienenS ["Este"]                                         ==  [4]
--    tienenS []                                               ==  []
--    tienenS [" "]                                            ==  []
-- ---------------------------------------------------------------------

tienenS xss = [length xs | xs <- xss, (elem 's' xs) || (elem 'S' xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Decimos que una lista está algo ordenada si para todo
-- par de elementos consecutivos se cumple que el primero es menor o
-- igual que el doble del segundo. Definir, por comprensión, la función
-- (algoOrdenada xs) que se verifica si la lista xs está algo ordenada. 
-- Por ejemplo, 
--    algoOrdenada [1,3,2,5,3,8]  ==  True
--    algoOrdenada [3,1]          ==  False
-- ---------------------------------------------------------------------

algoOrdenada xs = and [x <= 2*y | (x,y) <- zip xs (tail xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir, por comprensión, la función tripletas tal que 
-- (tripletas xs) es la listas de tripletas de elementos consecutivos de
-- la lista xs. Por ejemplo,
--    tripletas [8,7,6,5,4] == [[8,7,6],[7,6,5],[6,5,4]]
--    tripletas "abcd"      == ["abc","bcd"]
--    tripletas [2,4,3]     == [[2,3,4]]
--    tripletas [2,4]       == []
-- ---------------------------------------------------------------------

-- 1ª definición:
tripletas xs = 
    [[a,b,c] | ((a,b),c) <- zip (zip xs (tail xs)) (tail (tail xs))]

-- 2ª definición:
tripletas2 xs = 
    [[xs!!n,xs!!(n+1),xs!!(n+2)] | n <- [0..length xs -3]]

-- 3ª definición:
tripletas3 xs = [take 3 (drop n xs)| n <- [0..(length xs - 3)]]

-- Se puede definir por recursión
tripletas4 (x1:x2:x3:xs) = [x1,x2,x3] : tripletas (x2:x3:xs)
tripletas4 _             = []

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función tresConsecutivas tal que
-- (tresConsecutivas x ys) se verifica si x tres veces seguidas en la
-- lista ys. Por ejemplo,
--    tresConsecutivas 3 [1,4,2,3,3,4,3,5,3,4,6]  ==  Falsese
--    tresConsecutivas 'a' "abcaaadfg"            ==  True
-- ---------------------------------------------------------------------

tresConsecutivas x ys = elem [x,x,x] (tripletas ys)

-- ---------------------------------------------------------------------
-- Ejercicio 6. Se dice que un número n es malo si el número 666 aparece
-- en 2^n. Por ejemplo, 157 y 192 son malos, ya que:
--    2^157 = 182687704666362864775460604089535377456991567872
--    2^192 = 6277101735386680763835789423207666416102355444464034512896
-- 
-- Definir una función (malo x) que se verifica si el número x es
-- malo. Por ejemplo,
--    malo 157  ==  True
--    malo 192  ==  True
--    malo 221  ==  False
-- ---------------------------------------------------------------------

malo n = tresConsecutivas '6' (show (2^n))
