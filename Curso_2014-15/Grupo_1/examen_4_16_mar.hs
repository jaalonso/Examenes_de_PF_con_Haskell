-- Informática (1º del Grado en Matemáticas)
-- 4º examen de evaluación continua (16 de marzo de 2015)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Char
import Data.Array
import I1M.Pol
import Data.Numbers.Primes
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Se dice que dos números naturales son parientes si
-- tienen exactamente un factor primo en común, independientemente de su
-- multiplicidad. Por ejemplo, 
-- + Los números 12 (2^2*3) y 40 (2^3*5) son parientes, pues tienen al 2 
--   como único factor primo en común.
-- + Los números 49 (7^2) y 63 (3^2*7) son parientes, pues tienen al 7
--   como único factor primo en común.
-- + Los números 12 (2^2*3) y 30 (2*3*5) no son parientes, pues tienen
--   dos factores primos en común.
-- + Los números 49 (7^2) y 25 (5^2) no son parientes, pues no tienen
--   factores primos en común.
--
-- Se dice que una lista de números naturales es una secuencia de
-- parientes si cada par de números consecutivos son parientes. Por ejemplo, 
-- + La lista [12,40,35,28] es una secuencia de parientes.
-- + La lista [12,30,21,49] no es una secuencia de parientes.
-- 
-- Definir la función
--    secuenciaParientes :: [Int] -> Bool
-- tal que (secuenciaParientes xs) se verifica si xs es una secuencia de
-- parientes. Por ejemplo,
--    secuenciaParientes [12,40,35,28]  ==  True
--    secuenciaParientes [12,30,21,49]  ==  False
-- ----------------------------------------------------------------------------

parientes :: Int -> Int -> Bool
parientes x y =
    length [p | p <- takeWhile (<= d) primes, d `mod` p == 0] == 1 
    where d = gcd x y

-- Definiciones de secuenciaParientes 
-- ==================================

-- 1ª definición (por recursión)
secuenciaParientes :: [Int] -> Bool
secuenciaParientes []         = True
secuenciaParientes [x]        = True
secuenciaParientes (x1:x2:xs) =
    parientes x1 x2 && secuenciaParientes (x2:xs)

-- 2ª definición (por recursión con 2 ecuaciones)
secuenciaParientes2 :: [Int] -> Bool
secuenciaParientes2 (x1:x2:xs) =
    parientes x1 x2 && secuenciaParientes2 (x2:xs)
secuenciaParientes2 _         = True

-- 3ª definición (sin recursión):
secuenciaParientes3 :: [Int] -> Bool
secuenciaParientes3 xs = all (\(x,y) -> parientes x y) (zip xs (tail xs)) 

-- 4ª definición
secuenciaParientes4 :: [Int] -> Bool
secuenciaParientes4 xs = all (uncurry parientes) (zip xs (tail xs)) 

-- Equivalencia de las 4 definiciones
prop_secuenciaParientes :: [Int] -> Bool
prop_secuenciaParientes xs =
    secuenciaParientes2 xs == ys &&
    secuenciaParientes3 xs == ys &&
    secuenciaParientes4 xs == ys 
    where ys = secuenciaParientes xs

-- La comprobación es
--    ghci> quickCheck prop_secuenciaParientes
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2. En lógica temporal la expresión AFp significa que en
-- algún momento en el futuro se cumple la propiedad p. Trasladado a
-- su interpretación en forma de árbol lo que quiere decir es que en
-- todas las ramas (desde la raíz hasta una hoja) hay un nodo que cumple
-- la propiedad p.  
--
-- Consideramos el siguiente tipo algebraico de los árboles binarios:
--    data Arbol a = H a
--                 | N a (Arbol a) (Arbol a)
--                   deriving (Show,Eq)
-- y el siguiente árbol
--    a1 :: Arbol Int
--    a1 = N 9 (N 3 (H 2) (N 4 (H 1) (H 5))) (H 8)
-- En este árbol se cumple (AF par); es decir, en todas las ramas hay un
-- número  par; pero no se cumple (AF primo); es decir, hay ramas en las
-- que no hay ningún número primo. Donde una rama es la secuencia de
-- nodos desde el nodo inicial o raíz hasta una hoja.
--
-- Definir la función 
--    propiedadAF :: (a -> Bool) -> Arbol a -> Bool
-- tal que (propiedadAF p a) se verifica si se cumple (AF p) en el árbol
-- a; es decir, si en todas las ramas hay un nodo (interno u hoja) que
-- cumple la propiedad p. Por ejemplo
--    propiedadAF even a1     ==  True
--    propiedadAF isPrime a1  ==  False
-- ---------------------------------------------------------------------

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
             deriving (Show,Eq)

a1 :: Arbol Int
a1 = N 9 (N 3 (H 2) (N 4 (H 1) (H 5))) (H 8)

propiedadAF :: (a -> Bool) -> Arbol a -> Bool
propiedadAF p (H a)     = p a
propiedadAF p (N a i d) = p a || (propiedadAF p i && propiedadAF p d)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Consideramos las matrices representadas como tablas
-- cuyos índices son pares de números naturales.
--    type Matriz a = Array (Int,Int) a
--    
-- Una matriz cruzada es una matriz cuadrada en la que sólo hay elementos
-- distintos de 0 en las diagonales principal y secundaria. Por ejemplo
--    | 1 0 0 0 3 |     | 1 0 0 3 |
--    | 0 2 0 1 0 |     | 0 2 3 0 |
--    | 0 0 3 0 0 |     | 0 4 5 0 |
--    | 0 2 0 1 0 |     | 2 0 0 3 |
--    | 1 0 0 0 3 |
--
-- Definir la función
--    creaCruzada :: Int -> Matriz Int
-- tal que (creaCruzada n) es la siguiente matriz cruzada con n filas y n
-- columnas:
--    | 1  0   0  ...  0   0  1 |
--    | 0  2   0  ...  0   2  0 |
--    | 0  0   3  ...  3   0  0 |
--    | ....................... |
--    | 0  0  n-2 ... n-2  0  0 |
--    | 0 n-1  0  ...  0  n-1 0 |
--    | n  0   0  ...  0   0  n |
-- Es decir, los elementos de la diagonal principal son [1,...,n], en
-- orden desde la primera fila hasta la última; y los elementos de la
-- diagonal secundaria son [1,...,n], en orden desde la primera fila
-- hasta la última. 
-- ---------------------------------------------------------------------

type Matriz a = Array (Int,Int) a

creaCruzada :: Int -> Matriz Int
creaCruzada n =
    array ((1,1),(n,n))
          [((i,j),valores n i j) | i <- [1..n], j <- [1..n]]
    where valores n i j | i == j     = i
                        | i+j == n+1 = i
                        | otherwise  = 0

-- ---------------------------------------------------------------------
-- Ejercicio 4. Consideramos el TAD de los polinomios y los siguientes
-- ejemplos de polinomios
--    p1  =  4*x^4 + 6*x^3 + 7*x^2 + 5*x + 2
--    p2  =  6*x^5 + 2*x^4 + 8*x^3 + 5*x^2 + 8*x + 4
-- En Haskell,
--    p1, p2 :: Polinomio Int
--    p1 = consPol 4 4 
--                 (consPol 3 6 
--                          (consPol 2 7
--                                   (consPol 1 5 (consPol 0 2 polCero))))
--    p2 = consPol 5 6 
--                 (consPol 4 2
--                          (consPol 3 8
--                                   (consPol 2 5
--                                            (consPol 1 8 
--                                                     (consPol 0 4 polCero)))))
-- 
-- El cociente entero de un polinomio P(x) por un monomio ax^n es el
-- polinomio que se obtiene a partir de los términos de P(x) con un
-- grado mayor o igual que n, realizando la división entera entre sus
-- coeficientes y el coeficiente del monomio divisor y restando el valor
-- de n al de sus grados. Por ejemplo,
-- + El cociente entero de 4x^4 + 6x^3 + 7x^2 + 5x + 2 por el monomio
--   3x^2 se obtiene a partir de los términos 4x^4 + 6x^3 + 7x^2
--   realizando la división entera entre sus coeficientes y el número 3
--   y restando 2 a sus grados. De esta forma se obtiene 1x^2 + 2x + 2
-- + El cociente entero de 6x^5 + 2x^4 + 8x^3 + 5x^2 + 8x + 4 por el
--   monomio 4x^3 se obtiene a partir de los términos 6x^5 + 2x^4 + 8x^3
--   realizando la división entera entre sus coeficientes y el número 4
--   y restando 3 a sus grados. De esta forma se obtiene 1x^2 + 2
-- 
-- Definir la función
--    cocienteEntero :: Polinomio Int -> Int -> Int -> Polinomio Int
-- tal que (cocienteEntero p a n) es el cociente entero del polinomio p
-- por el monomio de grado n y coeficiente a. Por ejemplo,
--    cocienteEntero p1 3 2  =>  x^2 + 2*x + 2
--    cocienteEntero p2 4 3  =>  x^2 + 2
-- Nota: Este ejercicio debe realizarse usando únicamente las funciones
-- de la signatura del tipo abstracto de dato Polinomio.
-- ---------------------------------------------------------------------

p1, p2 :: Polinomio Int
p1 = consPol 4 4 
             (consPol 3 6 
                      (consPol 2 7
                               (consPol 1 5 (consPol 0 2 polCero))))
p2 = consPol 5 6 
             (consPol 4 2
                      (consPol 3 8
                               (consPol 2 5
                                        (consPol 1 8 
                                                 (consPol 0 4 polCero)))))

cocienteEntero :: Polinomio Int -> Int -> Int -> Polinomio Int
cocienteEntero p a n
    | grado p < n = polCero
    | otherwise   = consPol (grado p - n) (coefLider p `div` a)
                            (cocienteEntero (restoPol p) a n)

-- ---------------------------------------------------------------------
-- Ejercicio 5. Decimos que un número es de suma prima si la suma de
-- todos sus dígitos es un número primo. Por ejemplo el número 562 es de
-- suma prima pues la suma de sus dígitos es el número primo 13; sin
-- embargo, el número 514 no es de suma prima pues la suma de sus
-- dígitos es 10, que no es primo. 
--
-- Decimos que un número es de suma prima hereditario por la derecha si
-- es de suma prima y los números que se obtienen eliminando sus últimas
-- cifras también son de suma prima. Por ejemplo 7426 es de suma prima
-- hereditario por la derecha pues 7426, 742, 74 y 7 son todos números
-- de suma prima.
-- 
-- Definir la constante (función sin argumentos)
--    listaSumaPrimaHD :: [Integer]
-- cuyo valor es la lista infinita de los números de suma prima
-- hereditarios por la derecha. Por ejemplo,
--    take 10 listaSumaPrimaHD  ==  [2,3,5,7,20,21,23,25,29,30]
-- ----------------------------------------------------------------------------

-- (digitos n) es la lista de los dígitos de n. Por ejemplo,
--    digitos 325  ==  [3,2,5]

-- 1ª definición de digitos
digitos1 :: Integer -> [Integer]
digitos1 n = map (read . (:[])) (show n)

-- 2ª definición de digitos
digitos2 :: Integer -> [Integer]
digitos2 = map (read . (:[])) . show

-- 3ª definición de digitos
digitos3 :: Integer -> [Integer]
digitos3 n = [read [d] | d <- show n]

-- 4ª definición de digitos
digitos4 :: Integer -> [Integer]
digitos4 n = reverse (aux n)
    where aux n | n < 10    = [n]
                | otherwise = rem n 10 : aux (div n 10)

-- 5ª definición de digitos
digitos5 :: Integer -> [Integer]
digitos5 = map (fromIntegral . digitToInt) . show

-- Se usará la 1ª definición
digitos :: Integer -> [Integer]
digitos = digitos1

-- (sumaPrima n) se verifica si n es un número de suma prima. Por
-- ejemplo, 
--    sumaPrima 562  ==  True
--    sumaPrima 514  ==  False

-- 1ª definición de sumaPrima
sumaPrima :: Integer -> Bool
sumaPrima n = isPrime (sum (digitos n))

-- 2ª definición de sumaPrima
sumaPrima2 :: Integer -> Bool
sumaPrima2 = isPrime . sum . digitos

-- (sumaPrimaHD n) se verifica si n es de suma prima hereditario por la
-- derecha. Por ejemplo,
--    sumaPrimaHD 7426  ==  True
--    sumaPrimaHD 7427  ==  False
sumaPrimaHD n
    | n < 10    = isPrime n
    | otherwise = sumaPrima n && sumaPrimaHD (n `div` 10)

-- 1ª definición de listaSumaPrimaHD
listaSumaPrimaHD1 :: [Integer]
listaSumaPrimaHD1 = filter sumaPrimaHD [1..]

-- 2ª definición de listaSumaPrimaHD
-- =================================

listaSumaPrimaHD2 :: [Integer]
listaSumaPrimaHD2 = map fst paresSumaPrimaHDDigitos

paresSumaPrimaHDDigitos :: [(Integer, Integer)]
paresSumaPrimaHDDigitos =
    paresSumaPrimaHDDigitosAux 1 [(2,2),(3,3),(5,5),(7,7)]

paresSumaPrimaHDDigitosAux :: Integer -> [(Integer,Integer)] -> 
                              [(Integer,Integer)]
paresSumaPrimaHDDigitosAux n ac =
    ac ++ paresSumaPrimaHDDigitosAux (n+1) 
                                     (concatMap extiendeSumaPrimaHD ac)

extiendeSumaPrimaHD :: (Integer,Integer) -> [(Integer,Integer)]
extiendeSumaPrimaHD (n,s) = [(n*10+k,s+k) | k <- [0..9], isPrime (s+k)]

-- 3ª definición de listaSumaPrimaHD
-- =================================

listaSumaPrimaHD3 :: [Integer]
listaSumaPrimaHD3 = 
    map fst (concat (iterate (concatMap extiendeSumaPrimaHD3) 
                             [(2,2),(3,3),(5,5),(7,7)]))

extiendeSumaPrimaHD3 :: (Integer,Integer) -> [(Integer,Integer)]
extiendeSumaPrimaHD3 (n,s) = [(n*10+k,s+k) | k <- extensiones ! s]

extensiones :: Array Integer [Integer]
extensiones = array (1,1000) 
              [(n,[k | k <- [0..9], isPrime (n+k)]) | n <- [1..1000]]

-- Comparación de eficiencia
--    ghci> listaSumaPrimaHD1 !! 600
--    34004
--    (2.47 secs, 1565301720 bytes)
--    ghci> listaSumaPrimaHD2 !! 600
--    34004
--    (0.02 secs, 7209000 bytes)
--    ghci> listaSumaPrimaHD3 !! 600
--    34004
--    (0.01 secs, 1579920 bytes)
-- 
--    ghci> listaSumaPrimaHD2 !! 2000000
--    3800024668046
--    (45.41 secs, 29056613824 bytes)
--    ghci> listaSumaPrimaHD3 !! 2000000
--    3800024668046
--    (4.29 secs, 973265400 bytes)
