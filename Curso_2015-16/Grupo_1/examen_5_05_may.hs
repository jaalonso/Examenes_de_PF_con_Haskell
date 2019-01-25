-- Informática (1º del Grado en Matemáticas)
-- 5º examen de evaluación continua (5 de mayo de 2016)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Librerías auxiliares
-- ---------------------------------------------------------------------

import Data.Array
import Data.Maybe
import I1M.Pila
import I1M.PolOperaciones
import Test.QuickCheck
import qualified Data.Matrix as M
import qualified I1M.Cola as C

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Los números felices se caracterizan por lo siguiente:
-- dado n > 0, se reemplaza el número por la suma de los cuadrados de
-- sus dígitos, y se repite el proceso hasta que el número es igual a 1
-- o hasta que se entra en un bucle que no incluye el 1. Por ejemplo, 
--   49 -> 4^2 + 9^2 = 97 -> 9^2 + 7^2 = 130 -> 1^3 + 3^2 = 10 -> 1^2 = 1
--
-- Los números que al finalizar el proceso terminan con 1, son conocidos
-- como números felices. Los que no, son conocidos como números
-- infelices (o tristes).
--
-- Definir la función 
--     orbita:: Int -> [Int] 
-- tal que (orbita n) es la sucesión de números obtenidos a partir de
-- n. Por ejemplo:
--     take 10 (orbita 49) == [49,97,130,10,1,1,1,1,1,1]
--     take 20 (orbita 48)
--     [48,80,64,52,29,85,89,145,42,20,4,16,37,58,89,145,42,20,4,16]
-- ---------------------------------------------------------------------

digitos :: Int -> [Int]
digitos n = [read [x] | x <- show n]

sig :: Int -> Int
sig = sum . (map (^2)) . digitos

orbita :: Int -> [Int]
orbita n = iterate sig n

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Se observan las siguientes propiedades de la órbita de
-- un número natural n:
-- (*) O bien aparece un 1, en cuyo caso todos los siguientes son 1 y el
--     número es feliz.
-- (*) O bien, a partir de un término se repite periódicamente con un
--     periodo de longitud 8 (4,16,37,58,89,145,42,20)
-- 
-- Definir la constante
--    numerosFelices:: [Int]
-- que es la sucesión de números felices. Por ejemplo,
--    take 20 numerosFelices
--    [1,7,10,13,19,23,28,31,32,44,49,68,70,79,82,86,91,94,97,100]
--
-- Nota: Para determinar la felicidad de un número n, es suficiente recorrer
-- su órbita hasta encontrar un 1 o un 4.
-- ---------------------------------------------------------------------

-- 1ª solución
felicidad :: Int -> Maybe Bool
felicidad n = aux (orbita n)
    where aux (x:xs) | x == 1    = Just True
                     | x == 4    = Just False
                     | otherwise = aux xs

esFeliz :: Int -> Bool
esFeliz = fromJust . felicidad

numerosFelices :: [Int]
numerosFelices = filter esFeliz [1..]

-- 2ª solución
esFeliz2 :: Int -> Bool
esFeliz2 n = head (until p tail (orbita n)) == 1
    where p (x:xs) = x == 1 || x == 4

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir las funciones 
--    colaApila:: C.Cola a -> Pila a
--    pilaAcola:: Pila a -> C.Cola a 
-- tales que (colaApila c) transforma la cola c en una pila y pilaAcola
-- realiza la transformación recíproca. Por ejemplo,
--   ghci> let p = foldr apila vacia [3,8,-1,0,9]
--   ghci> p
--   3|8|-1|0|9|-
--   ghci> let c = pilaAcola p
--   ghci> c
--   C [3,8,-1,0,9]
--   ghci> colaApila c
--   3|8|-1|0|9|-
--   ghci> colaApila (pilaAcola p) == p
--   True
--   ghci> pilaAcola (colaApila c) == c
--   True
-- ---------------------------------------------------------------------

colaApila:: C.Cola a -> Pila a
colaApila c | C.esVacia c = vacia
            | otherwise   = apila x (colaApila q)
    where x = C.primero c
          q = C.resto c

pilaAcola:: Pila a -> C.Cola a
pilaAcola p = aux p C.vacia
    where aux q c | esVacia q = c
                  | otherwise = aux (desapila q) (C.inserta (cima q) c)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función 
--    polDiagonal :: M.Matrix Int -> Polinomio Int
-- tal que (polDiagonal p) es el polinomio cuyas raíces son los
-- elementos de la diagonal de la matriz cuadrada p. Por ejemplo,
--    ghci> polDiagonal (M.fromLists[[1,2,3],[4,5,6],[7,8,9]])
--    x^3 + -15*x^2 + 59*x + -45
-- ---------------------------------------------------------------------

polDiagonal :: M.Matrix Int -> Polinomio Int
polDiagonal m = multListaPol (map f (diagonal m))
    where f a = consPol 1 1 (consPol 0 (-a) polCero)

-- (diagonal p) es la lista de los elementos de la diagonal de la matriz
-- p. Por ejemplo,
--    diagonal (M.fromLists[[1,2,3],[4,5,6],[7,8,9]])  ==  [1,5,9]
diagonal :: Num a => M.Matrix a -> [a]
diagonal p = [p M.! (i,i) | i <- [1..min m n]]
    where m = M.nrows p
          n = M.ncols p

-- (multListaPol ps) es el producto de los polinomios de la lista ps.
multListaPol :: [Polinomio Int] -> Polinomio Int
multListaPol []     = polUnidad
multListaPol (p:ps) = multPol p (multListaPol ps)

-- 2ª definición de multListaPol
multListaPol2 :: [Polinomio Int] -> Polinomio Int
multListaPol2 = foldr multPol polUnidad

-- ---------------------------------------------------------------------
-- Ejercicio 4. El algoritmo de Damm (http://bit.ly/1SyWhFZ) se usa en
-- la detección de errores en códigos numéricos. Es un procedimiento
-- para obtener un dígito de control, usando la siguiente matriz, como
-- describimos en los ejemplos 
-- 
--   |  0   1   2   3   4   5   6   7   8   9
-- --+---------------------------------------
-- 0 |  0   3   1   7   5   9   8   6   4   2
-- 1 |  7   0   9   2   1   5   4   8   6   3
-- 2 |  4   2   0   6   8   7   1   3   5   9
-- 3 |  1   7   5   0   9   8   3   4   2   6
-- 4 |  6   1   2   3   0   4   5   9   7   8
-- 5 |  3   6   7   4   2   0   9   5   8   1
-- 6 |  5   8   6   9   7   2   0   1   3   4
-- 7 |  8   9   4   5   3   6   2   0   1   7
-- 8 |  9   4   3   8   6   1   7   2   0   9
-- 9 |  2   5   8   1   4   3   6   7   9   0
-- 
-- Ejemplo 1: cálculo del dígito de control de 572
--    + se comienza con la fila 0 y columna 5 de la matriz -> 9
--    + a continuación, la fila 9 y columna 7 de la matriz -> 7
--    + a continuación, la fila 7 y columna 2 de la matriz -> 4
-- con lo que se llega al final del proceso. Entonces, el dígito de
-- control de 572 es 4.
-- 
-- Ejemplo 2: cálculo del dígito de control de 57260
--    + se comienza con la fila 0 y columna 5 de la matriz -> 9
--    + a continuación, la fila 9 y columna 7 de la matriz -> 7
--    + a continuación, la fila 9 y columna 2 de la matriz -> 4
--    + a continuación, la fila 6 y columna 4 de la matriz -> 5
--    + a continuación, la fila 5 y columna 0 de la matriz -> 3
-- con lo que se llega al final del proceso. Entonces, el dígito de
-- control de 57260 es 3.
-- 
-- Representamos las matrices como tablas cuyos índices son pares de
-- números naturales.
--    type Matriz a = Array (Int,Int) a
-- 
-- Definimos la matriz:
--    mDamm :: Matriz Int
--    mDamm = listArray ((0,0),(9,9)) [0,3,1,7,5,9,8,6,4,2,
--                                     7,0,9,2,1,5,4,8,6,3,
--                                     4,2,0,6,8,7,1,3,5,9,
--                                     1,7,5,0,9,8,3,4,2,6,
--                                     6,1,2,3,0,4,5,9,7,8,
--                                     3,6,7,4,2,0,9,5,8,1,
--                                     5,8,6,9,7,2,0,1,3,4,
--                                     8,9,4,5,3,6,2,0,1,7,
--                                     9,4,3,8,6,1,7,2,0,9,
--                                     2,5,8,1,4,3,6,7,9,0]
-- 
-- Definir la función
--    digitoControl :: Int -> Int
-- tal que (digitoControl n) es el dígito de control de n. Por ejemplo:
--    digitoControl 572          == 4
--    digitoControl 57260        == 3
--    digitoControl 12345689012  == 6
--    digitoControl 5724         == 0
--    digitoControl 572603       == 0
--    digitoControl 123456890126 == 0
-- ---------------------------------------------------------------------

type Matriz a = Array (Int,Int) a

mDamm :: Matriz Int
mDamm = listArray ((0,0),(9,9)) [0,3,1,7,5,9,8,6,4,2,
                                 7,0,9,2,1,5,4,8,6,3,
                                 4,2,0,6,8,7,1,3,5,9,
                                 1,7,5,0,9,8,3,4,2,6,
                                 6,1,2,3,0,4,5,9,7,8,
                                 3,6,7,4,2,0,9,5,8,1,
                                 5,8,6,9,7,2,0,1,3,4,
                                 8,9,4,5,3,6,2,0,1,7,
                                 9,4,3,8,6,1,7,2,0,9,
                                 2,5,8,1,4,3,6,7,9,0]

-- 1ª solución
digitoControl :: Int -> Int
digitoControl n = aux (digitos n) 0
    where aux [] d = d
          aux (x:xs) d = aux xs (mDamm ! (d,x))

-- 2ª solución:
digitoControl2 :: Int -> Int
digitoControl2 n = last (scanl f 0 (digitos n))
    where f d x = mDamm ! (d,x)

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Comprobar con QuickCheck que si añadimos al final de
-- un número n su dígito de control, el dígito de control del número que
-- resulta siempre es 0.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_DC :: Int -> Property
prop_DC n = n >= 0 ==> digitoControl m == 0
    where m = read (show n ++ show (digitoControl n))

-- La comprobación es
--    ghci> quickCheck prop_DC
--    +++ OK, passed 100 tests.

-- 2ª expresión de la propiedad
prop_DC2 :: Int -> Bool
prop_DC2 n = digitoControl m == 0
    where m = read (show (abs n) ++ show (digitoControl (abs n)))

-- La comprobación es
--    ghci> quickCheck prop_DC2
--    +++ OK, passed 100 tests.

-- 3ª expresión de la propiedad
prop_DC3 :: Int -> Bool
prop_DC3 n = digitoControl (10 * n1 + digitoControl n1) == 0
    where n1 = abs n

-- La comprobación es
--    ghci> quickCheck prop_DC3
--    +++ OK, passed 100 tests.

-- 4ª expresión de la propiedad
prop_DC4 :: (Positive Int) -> Bool
prop_DC4 (Positive n) = 
    digitoControl2 (10 * n + digitoControl2 n) == 0

-- La comprobación es
--    ghci > quickCheck prop_DC4
--    +++ OK, passed 100 tests.
