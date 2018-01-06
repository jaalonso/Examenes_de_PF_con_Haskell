-- Informática (1º del Grado en Matemáticas, Grupo 2)
-- 1º examen de evaluación continua (30 de octubre de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías                                                        --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Un número es defectivo si es mayor que la suma de sus 
-- divisores propios positivos. Por ejemplo, 15 tiene 3 divisores
-- propios (1, 3 y 5) cuya suma es 9 que es menor que 15. Por tanto, el
-- número 15 es defectivo. 
--
-- Definir la función
--    esDefectivo :: Int -> Bool
-- tal que (esDefectivo x) se verifica si x es defectivo. Por ejemplo, 
--    esDefectivo 15 == True
--    esDefectivo 17 == True
--    esDefectivo 18 == False
-- ---------------------------------------------------------------------

esDefectivo :: Int -> Bool
esDefectivo x = x > sum (divisoresPropios x)

divisoresPropios :: Int -> [Int]
divisoresPropios x = [y | y <-[1..x-1], x `rem` y == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    defectivosMenores :: Int -> [Int]
-- tal que (defectivosMenores n) es la lista de los números defectivos
-- menores que n. Por ejemplo,
--    defectivosMenores 20 == [1,2,3,4,5,7,8,9,10,11,13,14,15,16,17,19]
-- ---------------------------------------------------------------------

-- 1ª definición
defectivosMenores :: Int -> [Int]
defectivosMenores n = [x | x <- [1 ..n-1], esDefectivo x]

-- 2ª definición
defectivosMenores2 :: Int -> [Int]
defectivosMenores2 n = filter esDefectivo [1..n-1]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    defectivosConCifra :: Int -> Int -> [Int]
-- tal que (defectivosConCifra n x) es la lista de los n primeros
-- números defectivos que contienen la cifra x. Por ejemplo,
--    defectivosConCifra 10 3 == [3,13,23,31,32,33,34,35,37,38]
--    defectivosConCifra 10 0 == [10,50,101,103,105,106,107,109,110,130]
-- ---------------------------------------------------------------------

defectivosConCifra :: Int -> Int -> [Int]
defectivosConCifra n x =
  take n [y | y <- [1..]
            , esDefectivo y
            , c `elem` show y]
  where c = head (show x)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Comprobar con QuickCheck que los números primos son
-- defectivos.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_primosSonDefectivos :: Int  -> Property
prop_primosSonDefectivos x =
  esPrimo x ==> esDefectivo x

esPrimo :: Int -> Bool
esPrimo x = divisoresPropios x == [1]

-- La comprobación es
--    ghci> quickCheck prop_primosSonDefectivos
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir, por comprension, la función
--    lugarPar :: [a] -> [a]
-- tal que (lugarPar xs) es la lista de los elementos de xs que ocupan
-- las posiciones pares. Por ejemplo, 
--    lugarPar [0,1,2,3,4] == [0,2,4]
--    lugarPar "ahora si"  == "aoas"
-- ---------------------------------------------------------------------

-- 1ª definición
lugarPar :: [a] -> [a]
lugarPar xs =
  [xs!!n | n <-[0,2 .. length xs - 1]]

-- 2ª definición
lugarPar2 :: [a] -> [a]
lugarPar2 xs =
  [x | (x,n) <- zip xs [0..], even n]

-- Comparación de eficiencia
--    ghci> maximum (lugarPar [1..10^5])
--    99999
--    (5.80 secs, 22,651,424 bytes)
--    ghci> maximum (lugarPar2 [1..10^5])
--    99999
--    (0.06 secs, 48,253,456 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    paresOimpares :: [a] -> [a]
-- tal que (paresOimpares xs) es la lista de los elementos que ocupan
-- las posiciones pares en xs, si la longitud de xs es par y, en caso
-- contrario, es la lista de los elementos que ocupan  posiciones
-- impares en xs. Por ejemplo, 
--    paresOimpares [1,2,3,4,5]        == [2,4]
--    paresOimpares [1,2,3,4]          == [1,3]
--    paresOimpares "zorro y la loba"  == "or  alb"
--    paresOimpares "zorro y la lob"   == "zroyl o"
-- ---------------------------------------------------------------------

paresOimpares :: [a] -> [a]
paresOimpares xs
  | even (length xs) = lugarPar2 xs
  | otherwise        = lugarImpar xs

lugarImpar :: [a] -> [a]
lugarImpar xs =
  [x | (x,n) <- zip xs [0..], odd n]
  




