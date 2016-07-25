-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 1º examen de evaluación continua (14 de noviembre de 2011)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función posicion tal que (posicion x ys) es
-- la primera posición de x en ys. Por ejemplo,
--    posicion 5 [3,4,5,6,5]  ==  2
-- ---------------------------------------------------------------------

posicion x xs = head [i |( c,i) <- zip xs [0..], c == x]

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir la función impares tal que (impares xs) es la
-- lista de los elementos de xs que ocupan las posiciones impares. Por
-- ejemplo, 
--    impares [4,3,5,2,6,1] == [3,2,1]
--    impares [5]           == []
--    impares []            == []
-- ---------------------------------------------------------------------

impares xs = [x | x <- xs, odd (posicion x xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función pares tal que (pares xs) es la
-- lista de los elementos de xs que ocupan las posiciones pares. Por
-- ejemplo,  
--    pares [4,3,5,2,6,1] == [4,5,6]
--    pares [5]           == [5]
--    pares []            == []
-- ---------------------------------------------------------------------

pares xs = [x | x <- xs, even (posicion x xs)]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función separaPorParidad tal que 
-- (separaPorParidad xs) es el par cuyo primer elemento es la lista de
-- los elementos de xs que ocupan las posiciones pares y el segundo es
-- la lista de los que ocupan las posiciones impares. Por ejemplo,
--    separaPorParidad [7,5,6,4,3] == ([7,6,3],[5,4])
--    separaPorParidad [4,3,5]     == ([4,5],[3]) 
-- ---------------------------------------------------------------------

separaPorParidad xs = (pares xs, impares xs)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función eliminaElemento tal que
-- (eliminaElemento xs n) es la lista que resulta de eliminar el n-ésimo
-- elemento de la lista xs. Por ejemplo, 
--    eliminaElemento [1,2,3,4,5] 0    == [2,3,4,5]
--    eliminaElemento [1,2,3,4,5] 2    == [1,2,4,5]
--    eliminaElemento [1,2,3,4,5] (-1) == [1,2,3,4,5]
--    eliminaElemento [1,2,3,4,5] 7    == [1,2,3,4,5]
-- ---------------------------------------------------------------------

-- 1ª definición:
eliminaElemento xs n = take n xs ++ drop (n+1)xs

-- 2ª definición:
eliminaElemento2 xs n = [x| x <- xs, posicion x xs /= n]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir por comprensión, usando la lista [1..10], las
-- siguientes listas
--    l1 = [2,4,6,8,10]
--    l2 = [[1],[3],[5],[7],[9]]
--    l3 = [(1,2),(2,3),(3,4),(4,5),(5,6)]
-- ---------------------------------------------------------------------

l1 = [x | x <- [1..10], even x]

l2 = [[x] | x <- [1..10], odd x]

l3 = [(x,y)| (x,y) <- zip [1..10] (tail [1 .. 10]), x <= 5]

