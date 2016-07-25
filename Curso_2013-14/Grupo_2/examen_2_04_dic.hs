-- Informática (1º del Grado en Matemáticas)
-- 2º examen de evaluación continua (4 de diciembre de 2013)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- --------------------------------------------------------------------- 
-- Ejercicio 1. Definir, sin usar recursión, la función intro tal que
-- (intro x n xs) es la lista que resulta de introducir x en el lugar n
-- de la lista xs. Si n es  negativo, x se introducirá como primer
-- elemento; si n supera la longitud de xs, x aparecerá al final de
-- xs. Por ejemplo, 
--    intro 5 1 [1,2,3]    == [1,5,2,3]
--    intro 'd' 2 "deo"    == "dedo"
--    intro 5 (-3) [1,2,3] == [5,1,2,3]
--    intro 5 10 [1,2,3]   == [1,2,3,5]
-- ---------------------------------------------------------------------

intro x n xs = take n xs ++ [x] ++ drop n xs

-- --------------------------------------------------------------------- 
-- Ejercicio 2. Definir, por recursión, la función introR tal que sea
-- equivalenta a la función intro del ejercicio anterior.
-- ---------------------------------------------------------------------

introR x n [] = [x]
introR x n (y:xs) | n <= 0            = x:y:xs
                  | n > length (y:xs) = (y:xs) ++ [x]
                  | otherwise         = y : introR x (n-1) xs
                                   
-- --------------------------------------------------------------------- 
-- Ejercicio 3. Definir la función primerosYultimos tal que 
-- (primerosYultimos xss) es el par formado  por la lista de los
-- primeros elementos de las listas no vacías de xss y la lista de los
-- últimos elementos de las listas no vacías de xs. Por ejemplo,
--    ghci> primerosYultimos [[1,2],[5,3,4],[],[0,8,7,6],[],[9]] 
--    ([1,5,0,9],[2,4,6,9])
-- ---------------------------------------------------------------------

primerosYultimos xss =
    ([head xs | xs <- xss, not (null xs)],
     [last xs | xs <- xss, not (null xs)])
                               
-- --------------------------------------------------------------------- 
-- Ejercicio 4. El número personal se calcula sumando las cifras del
-- día/mes/año de nacimiento sucesivamente hasta que quede un solo
-- dígito. Por ejemplo, el número personal de los que han nacido el
-- 29/10/1994 se calcula por
--    29/10/1994 --> 2+9+1+0+1+9+4
--               =   26
--               --> 2+6
--               =   8     
-- 
-- Definir la función personal tal que (personal x y z) es el número
-- personal de los que han nacido el día x del mes y del año z. Por
-- ejemplo, 
--    personal 29 10 1994 == 8
-- ---------------------------------------------------------------------

personal x y z = 
    reduce (sum (concat [digitos x, digitos y, digitos z]))

digitos n | n < 10    = [n]
          | otherwise = n `rem` 10 : digitos (n `div` 10)

reduce x | x < 10    = x
         | otherwise = reduce (sum (digitos x))

-- --------------------------------------------------------------------- 
-- Ejercicio 5. Definir, por recursión, la función parMitad tal que 
-- (parMitad xs) es la lista obtenida sustituyendo cada numero par de la
-- lista xs por su mitad. Por ejemplo,
--    parMitad [1,2,3,4,5,6] = [1,1,3,2,5,3]
-- ---------------------------------------------------------------------

parMitad [] = []
parMitad (x:xs) | even x    = x `div` 2 : parMitad xs
                | otherwise = x : parMitad xs

-- --------------------------------------------------------------------- 
-- Ejercicio 6. Definir la funcion parMitad1 que sea equivalente a
-- parMitad, pero no recursiva.
-- ---------------------------------------------------------------------

parMitad1 = map f 
    where f x | even x = div x 2
              | otherwise = x
                                               
-- --------------------------------------------------------------------- 
-- Ejercicio 7. Comprobar con QuickCheck que las funciones parMitad y
-- parMitad1 son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_parMitad xs = parMitad xs == parMitad1 xs

-- La comprobación es
--    ghci> quickCheck prop_parMitad
--    +++ OK, passed 100 tests.
