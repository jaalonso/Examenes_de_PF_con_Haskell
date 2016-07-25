-- Informática (1º del Grado en Matemáticas, Grupo 2)
-- 1º examen de evaluación continua (27 de octubre de 2011)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Los puntos del plano se pueden representar mediante
-- pares de números reales.
-- 
-- Definir la función estanEnLinea tal que (estanEnLinea p1 p2) se
-- verifica si los puntos p1 y p2 están en la misma línea vertical u
-- horizontal. Por ejemplo, 
--    estanEnLinea (1,3) (1,-6)  == True
--    estanEnLinea (1,3) (-1,-6) == False
--    estanEnLinea (1,3) (-1,3)  == True
-- ---------------------------------------------------------------------

estanEnLinea (x1,y1) (x2,y2) = x1 == x2 || y1 == y2

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función pCardinales tal que 
-- (pCardinales (x,y) d) es la lista formada por los cuatro puntos
-- situados al norte, sur este y oeste, a una distancia d de (x,y).
-- Por ejemplo,
--    pCardinales (0,0) 2  == [(0,2),(0,-2),(-2,0),(2,0)]
--    pCardinales (-1,3) 4 == [(-1,7),(-1,-1),(-5,3),(3,3)]
-- ---------------------------------------------------------------------

pCardinales (x,y) d = [(x,y+d),(x,y-d),(x-d,y),(x+d,y)]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función elementosCentrales tal que 
-- (elementosCentrales xs) es la lista formada por el elemento central
-- si xs tiene un número impar de elementos, y los dos elementos
-- centrales si xs tiene un número par de elementos. Por ejemplo,
--    elementosCentrales [1..8] == [4,5]
--    elementosCentrales [1..7] == [4]
-- ---------------------------------------------------------------------

elementosCentrales xs 
    | even n   = [xs!!(m-1), xs!!m]
    | otherwise = [xs !! m]
    where n = length xs
          m = n `div` 2

-- ---------------------------------------------------------------------
-- Ejercicio 4. Consideremos el problema geométrico siguiente: partir un
-- segmento en dos trozos, a y b, de forma que, al dividir la longitud
-- total entre el mayor (supongamos que es a), obtengamos el mismo
-- resultado que al dividir la longitud del mayor entre la del menor.  
-- 
-- Definir la función esParAureo tal que (esParAureo a b)
-- se verifica si a y b forman un par con la característica anterior.
-- Por ejemplo, 
--    esParAureo 3 5                 == False
--    esParAureo 1 2                 == False
--    esParAureo ((1+ (sqrt 5))/2) 1 == True
-- ---------------------------------------------------------------------

esParAureo a b = (a+b)/c == c/d
    where c = max a b
          d = min a b

