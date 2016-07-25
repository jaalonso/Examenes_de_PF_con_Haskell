-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 1º examen de evaluación continua (29 de octubre de 2010)
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función extremos tal que (extremos n xs) es la
-- lista formada por los n primeros elementos de xs y los n finales
-- elementos de xs. Por ejemplo, 
--    extremos 3 [2,6,7,1,2,4,5,8,9,2,3]  ==  [2,6,7,9,2,3]
-- ---------------------------------------------------------------------

extremos n xs = take n xs ++ drop (length xs - n) xs

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir la función puntoMedio tal que 
-- (puntoMedio p1 p2) es el punto medio entre los puntos p1 y p2. Por
-- ejemplo, 
--    puntoMedio (0,2) (0,6)  ==  (0.0,4.0)
--    puntoMedio (-1,2) (7,6) ==  (3.0,4.0)
-- ---------------------------------------------------------------------

puntoMedio (x1,y1) (x2,y2) = ((x1+x2)/2, (y1+y2)/2)

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con quickCheck que el punto medio entre P y
-- Q equidista de ambos puntos.
-- ---------------------------------------------------------------------

-- El primer intento es
prop_puntoMedio (x1,y1) (x2,y2) = 
    distancia (x1,y1) p == distancia (x2,y2) p
    where p = puntoMedio (x1,y1) (x2,y2)

-- (distancia p q) es la distancia del punto p al q. Por ejemplo,           
--    distancia (0,0) (3,4)  ==  5.0
distancia (x1,y1) (x2,y2) = sqrt((x1-x2)^2+(y1-y2)^2)

-- La comprobación es 
--    ghci> quickCheck prop_puntoMedio
--    *** Failed! Falsifiable (after 13 tests and 5 shrinks):  
--    (10.0,-9.69156092012789)
--    (6.0,27.0)

-- Falla, debido a los errores de redondeo. Hay que expresarla en
-- términos de la función ~=.

-- (x ~= y) se verifica si x es aproximadamente igual que y; es decir,
-- el valor absoluto de su diferencia es menor que 0.0001.
x ~= y = abs(x-y) < 0.0001

-- El segundo intento es
prop_puntoMedio2 (x1,y1) (x2,y2) = 
    distancia (x1,y1) p ~= distancia (x2,y2) p
    where p = puntoMedio (x1,y1) (x2,y2)

-- La comprobación es 
--    ghci> quickCheck prop_puntoMedio'
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función ciclo tal que (ciclo xs) es
-- permutación de xs obtenida pasando su último elemento a la primera
-- posición y desplazando los otros elementosPor ejemplo, 
--    ciclo [2,5,7,9]         ==  [9,2,5,7]
--    ciclo ["yo","tu","el"]  ==  ["el","yo","tu"]
-- ---------------------------------------------------------------------

ciclo [] = []
ciclo xs = last xs : init xs

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la función numeroMayor tal que
-- (numeroMayor x y) es el mayor número de dos cifras que puede puede
-- construirse con los dígitos x e y. Por ejemplo,
--    numeroMayor 2 5  ==  52
--    numeroMayor 5 2  ==  52
-- ---------------------------------------------------------------------

numeroMayor x y = 10*a + b
    where a = max x y 
          b = min x y

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función numeroMenor tal que tal que
-- (numeroMenor x y) es el menor número de dos cifras que puede puede
-- construirse con los dígitos x e y. Por ejemplo,
--    numeroMenor 2 5  ==  25
--    numeroMenor 5 2  ==  25
-- ---------------------------------------------------------------------

numeroMenor x y = 10*b + a
    where a = max x y 
          b = min x y

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Comprobar con QuickCheck que el menor número que puede
-- construirse con dos dígitos es menor o igual que el mayor.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_menorMayor x y =
  numeroMenor x y <= numeroMayor x y

-- La comprobación es
--    ghci> quickCheck prop_menorMayor
--    +++ OK, passed 100 tests.
