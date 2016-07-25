-- Informática (1º del Grado en Matemáticas)
-- 1º examen de evaluación continua (6 de noviembre de 2013)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Se dice que dos números son hermanos si tienen el
-- mismo número de divisores propios. Por ejemplo, 6 y 22 son hermanos
-- porque ambos tienen tres divisores propios.
-- 
-- Definir la función hermanos tal que (hermanos x y) se verifica si x e
-- y son hermanos. Por ejemplo,
--    hermanos 6 10 == True
--    hermanos 3 4  == False
-- ---------------------------------------------------------------------

hermanos x y = length (divisoresProp x) == length (divisoresProp y)

divisoresProp x = [y | y <- [1 .. x-1], mod x y == 0]

--- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función hermanosHasta tal que 
-- (hermanosHasta n) es la lista de los pares de números hermanos
-- menores o iguales a n.  Por ejemplo,
--    hermanosHasta 4  ==  [(1,1),(2,2),(2,3),(3,2),(3,3),(4,4)]
-- ---------------------------------------------------------------------

hermanosHasta n = [(x,y) | x <- [1 .. n], y <- [1 .. n], hermanos x y]

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la propiedad prop_hermanos1 tal que 
-- (prop_hermanos1 x y) se verifica si se cumple que x es hermano de y
-- si, y sólo si, y es hermano de x.
-- ---------------------------------------------------------------------

prop_hermanos1 x y = hermanos x y == hermanos y x

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Definir la propiedad prop_hermanos2 tal 
-- (prop_hermanos2 x) se verifica si x es hermano de sí mismo.
-- ---------------------------------------------------------------------

prop_hermanos2 x = hermanos x x 

-- ---------------------------------------------------------------------
-- Ejercicio 1.5. Definir la función primerosHermanos tal que 
-- (primerosHermanos k) es la lista de los primeros k pares de números
-- hermanos tales que el primero es menor que el segundo. Por ejemplo, 
--    ghci> primerosHermanos 10
--    [(2,3),(2,5),(3,5),(2,7),(3,7),(5,7),(6,8),(4,9),(6,10),(8,10)]
-- ---------------------------------------------------------------------

primerosHermanos k = 
    take k [(x,y) | y <- [1..], x <- [1..y-1], hermanos x y]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función superDiv tal que (superDiv n) es la 
-- lista de listas que contienen los divisores de cada uno de los 
-- divisores de n. Por ejemplo, 
--    superDiv 10 == [[1],[1,2],[1,5],[1,2,5,10]]
--    superDiv 12 == [[1],[1,2],[1,3],[1,2,4],[1,2,3,6],[1,2,3,4,6,12]]
-- ---------------------------------------------------------------------

divisores n = [x | x <- [1..n], mod n x == 0]

superDiv n = [divisores x | x <- divisores n]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir una funcion noPrimos tal que (noPrimos n)
-- es la lista que resulta de sustituir cada elemento de (superDiv n)
-- por el número de números no primos que contiene. Por ejemplo.
--    noPrimos 10 == [1,1,1,2]
--    noPrimos 12 == [1,1,1,2,2,4]
-- ---------------------------------------------------------------------

noPrimos n = 
    [length [x | x <- xs, not (primo x)] | xs <- superDiv n]

primo n = divisores n == [1,n]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Una lista es genial si la diferencia en valor absoluto
-- entre cualesquiera dos términos consecutivos es siempre mayor o igual
-- que la posición del primero de los dos. Por ejemplo, [1,3,-4,1] es
-- genial ya que 
--    |1-3|    = 2 >= 0 = posición del 1,  
--    |3-(-4)| = 7 >= 1 = posición del 3,
--    |(-4)-1| = 5 >= 2 = posición del -4. 
-- en cambio, [1,3,0,1,2] no es genial ya que 
--    |1-0| = 1 < 2 = posición del 1.
-- 
-- Definir por comprensión la función genial tal que (genial xs) se
-- verifica si xs es una lista genial. Por ejemplo,
--    genial [1,3,-4,1]  ==  True
--    genial [1,3,0,1,2]  ==  False
-- ---------------------------------------------------------------------

genial :: [Int] -> Bool
genial xs =
    and [abs (x-y) >= n | ((x,y),n) <- zip (zip xs (tail xs)) [0..]]

-- 2ª definición:
genial2 :: [Int] -> Bool
genial2 xs =
    and [abs (x-y) >= n | (x,y,n) <- zip3 xs (tail xs) [0..]]

