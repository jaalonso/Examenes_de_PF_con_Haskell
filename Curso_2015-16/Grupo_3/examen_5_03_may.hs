-- Informática (1º del Grado en Matemáticas)
-- 5º examen de evaluación continua (3 de mayo de 2016)
-- ============================================================================

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Numbers.Primes
import I1M.Cola
import I1M.Pol
import qualified Data.Matrix as M

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Consideramos la secuencia infinita
--    0, 0, 1, 0, 2, 1, 3, 0, 4, 2, 5, 1, 6, 3, 7, 0, 8, 4, 9, 2, 
--    10, 5, 11, 1, 12, 6, 13, 3, 14, 7, 15, ...
-- construida de la siguiente forma:
-- + Los términos pares forman la secuencia infinita de los números
--   naturales 
--    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, ...
-- + Los términos impares forman la misma secuencia infinita original
--    0, 0, 1, 0, 2, 1, 3, 0, 4, 2, 5, 1, 6, 3, 7, ...
--
-- Definir la función
--   termino :: Integer -> Integer
-- tal que (termino n) es el término n de la secuencia anterior. Por
-- ejemplo,
--   termino 0            ==  0
--   termino 1            ==  0
--   map termino [0..10]  ==  [0,0,1,0,2,1,3,0,4,2,5]
-- ---------------------------------------------------------------------

termino :: Integer -> Integer
termino 0 = 0
termino n 
    | even n    = n `div` 2
    | otherwise = termino (n `div` 2)

-- ----------------------------------------------------------------------------
-- Ejercicio 1.2. Definir la constante
--   secuencia :: [Integer]
-- cuyo valor es la secuencia infinita descrita en el enunciado. Por ejemplo,
--   take 10 secuencia  ==  [0,0,1,0,2,1,3,0,4,2]
--   take 20 secuencia  ==  [0,0,1,0,2,1,3,0,4,2,5,1,6,3,7,0,8,4,9,2]
-- ----------------------------------------------------------------------------

-- 1ª definición (basada en termino)
-- =================================

secuencia1 :: [Integer]
secuencia1 = 
    map termino [0..]

-- 2ª definición
-- =============

secuencia2 :: [Integer]
secuencia2 =
    0 : 0 : mezcla [1..] (tail secuencia2)

-- (mezcla xs ys) es la lista obtenida intercalando las listas infinitas
-- xs e ys. Por ejemplo,
--    take 10 (mezcla [0,2..] [0,-2..])  ==  [0,0,2,-2,4,-4,6,-6,8,-8]
mezcla :: [Integer] -> [Integer] -> [Integer]
mezcla (x:xs) (y:ys) =
    x:y:mezcla xs ys

-- Comparación de eficiencia
-- =========================

--    λ> sum (take (10^6) secuencia1)
--    166666169612
--    (5.56 secs, 842,863,264 bytes)
--    λ> sum (take (10^6) secuencia2)
--    166666169612
--    (1.81 secs, 306,262,616 bytes)

-- En lo que sigue usaremos la 2ª definición
secuencia :: [Integer]
secuencia = secuencia2

-- ----------------------------------------------------------------------------
-- Ejercicio 1.3. Definir la función
--    sumaSecuencia :: Integer -> Integer
-- tal que (sumaSecuencia n) es la suma de los n primeros términos de la
-- secuencia infinita descrita en el enunciado. Por ejemplo,
--    sumaSecuencia 10      ==  13
--    sumaSecuencia (10^4)  ==  16661783
-- ¿Cuánto es (sumaSecuencia (10^20))?
-- ----------------------------------------------------------------------------

-- 1ª definición
sumaSecuencia1 :: Integer -> Integer
sumaSecuencia1 n =
    sum (map termino [0..n-1])

-- 2ª definición
sumaSecuencia2 :: Integer -> Integer
sumaSecuencia2 n =
    sum (take (fromIntegral n) secuencia)

-- 3ª definición
sumaSecuencia3 :: Integer -> Integer
sumaSecuencia3 0 = 0
sumaSecuencia3 1 = 0
sumaSecuencia3 n
    | even n    = sumaN (n `div` 2) + sumaSecuencia3 (n `div` 2)
    | otherwise = sumaN ((n+1) `div` 2) + sumaSecuencia3 (n `div` 2)
    where sumaN n = (n*(n-1)) `div` 2

-- Comparación de eficiencia
-- =========================
--    λ> sumaSecuencia1 (10^6)
--    166666169612
--    (5.25 secs, 810,622,504 bytes)
--    λ> sumaSecuencia2 (10^6)
--    166666169612
--    (1.72 secs, 286,444,048 bytes)
--    λ> sumaSecuencia3 (10^6)
--    166666169612
--    (0.01 secs, 0 bytes)
--    
--    λ> sumaSecuencia2 (10^7)
--    16666661685034
--    (17.49 secs, 3,021,580,920 bytes)
--    λ> sumaSecuencia3 (10^7)
--    16666661685034
--    (0.01 secs, 0 bytes)

-- El cálculo de (sumaSecuencia (10^20)) es
--    λ> sumaSecuencia3 (10^20)
--    1666666666666666666616666684103392376198
--    (0.01 secs, 0 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Ulises, en sus ratos libres, juega a un pasatiempo que
-- consiste en, dada una serie de números naturales positivos en una
-- cola, sacar un elemento y, si es distinto de 1, volver a meter el
-- mayor de sus divisores propios. Si el número que saca es el 1,
-- entonces lo deja fuera y no mete ningún otro. El pasatiempo continúa
-- hasta que la cola queda vacía.  
-- 
-- Por ejemplo, a partir de una cola con los números 10, 20 y 30, el
-- pasatiempo se desarrollaría como sigue:
--   C [30,20,10]
--   C [20,10,15]
--   C [10,15,10]
--   C [15,10,5]
--   C [10,5,5]
--   C [5,5,5]
--   C [5,5,1]
--   C [5,1,1]
--   C [1,1,1]
--   C [1,1]
--   C [1]
--   C []
--
-- Definir la función
--    numeroPasos :: Cola Int -> Int
-- tal que (numeroPasos c) es el número de veces que Ulises saca algún
-- número de la cola c al utilizarla en su pasatiempo. Por ejemplo,
--    numeroPasos (foldr inserta vacia [30])        ==  4
--    numeroPasos (foldr inserta vacia [20])        ==  4
--    numeroPasos (foldr inserta vacia [10])        ==  3
--    numeroPasos (foldr inserta vacia [10,20,30])  ==  11
-- ----------------------------------------------------------------------------

numeroPasos :: Cola Int -> Int
numeroPasos c
    | esVacia c = 0
    | pc == 1   = 1 + numeroPasos rc
    | otherwise = 1 + numeroPasos (inserta (mayorDivisorPropio pc) rc)
    where pc = primero c
          rc = resto c

-- (mayorDivisorPropio n) es el mayor divisor propio de n. Por ejemplo,
--    mayorDivisorPropio 30  ==  15

-- 1ª definición de mayorDivisorPropio
mayorDivisorPropio1 :: Int -> Int
mayorDivisorPropio1 n = 
    head [x | x <- [n-1,n-2..], n `mod` x == 0]

-- 2ª definición de mayorDivisorPropio
mayorDivisorPropio2 :: Int -> Int
mayorDivisorPropio2 n =
    n `div` head (primeFactors n)

-- Comparación de eficiencia:
--    λ> sum (map mayorDivisorPropio1 [2..3000])
--    1485659
--    (3.91 secs, 618,271,360 bytes)
--    λ> sum (map mayorDivisorPropio2 [2..3000])
--    1485659
--    (0.04 secs, 22,726,600 bytes)

-- Usaremos la 2ª definición de mayorDivisorPropio
mayorDivisorPropio :: Int -> Int
mayorDivisorPropio = mayorDivisorPropio2

-- ---------------------------------------------------------------------
-- Ejercicio 3. Dados dos polinomios P y Q, su producto de grado, que
-- notaremos P (o) Q, es el polinomio que se construye de la siguiente 
-- forma: para cada par de términos de P y Q con el mismo grado se
-- considera un término con dicho grado cuyo coeficiente es el producto
-- de los coeficientes de dichos términos. Se muestran algunos ejemplos
-- de producto de grado entre polinomios:
-- + Dos polinomios con un único término con el mismo grado
--      2*x^2 (o) 3*x^2 = 6*x^2 
-- + Dos polinomios entre los que no hay términos con el mismo grado
--      2*x^2 + 1 (o) 3*x^3 - x = 0
-- + Dos polinomios entre los que hay dos pares de términos con el mismo grado
--      2*x^2 + x + 2 (o) 3*x^2 - 4*x = 6*x^2 - 4*x
-- 
-- Definir la función 
--    productoDeGrado :: Polinomio Int -> Polinomio Int -> Polinomio Int
-- tal que (productoDeGrado p q) es el producto de grado de los polinomios
-- p y q. Por ejemplo, dados los polinomios
--    pol1  =  4*x^4 + 5*x^3 + 1
--    pol2  =  6*x^5 + 5*x^4 + 4*x^3 + 3*x^2 + 2*x + 1
--    pol3  =  -3*x^7 + 3*x^6 + -2*x^4 + 2*x^3 + -1*x + 1
--    pol4  =  -1*x^6 + 3*x^4 + -3*x^2 + 1
-- entonces
--    productoDeGrado pol1 pol1  =>  16*x^4 + 25*x^3 + 1
--    productoDeGrado pol1 pol2  =>  20*x^4 + 20*x^3 + 1
--    productoDeGrado pol1 pol3  =>  -8*x^4 + 10*x^3 + 1
--    productoDeGrado pol3 pol4  =>  -3*x^6 + -6*x^4 + 1
-- ----------------------------------------------------------------------------

listaApol :: [Int] -> Polinomio Int
listaApol xs = foldr (\ (n,c) p -> consPol n c p) 
                     polCero 
                     (zip [0..] xs)

pol1, pol2, pol3, pol4 :: Polinomio Int
pol1 = listaApol [1,0,0,5,4,0]
pol2 = listaApol [1,2,3,4,5,6]
pol3 = listaApol [1,-1,0,2,-2,0,3,-3]
pol4 = listaApol [1,0,-3,0,3,0,-1]

productoDeGrado :: Polinomio Int -> Polinomio Int -> Polinomio Int
productoDeGrado p q
    | esPolCero p = polCero
    | esPolCero q = polCero
    | gp < gq     = productoDeGrado p rq
    | gq < gp     = productoDeGrado rp q
    | otherwise   = consPol gp (cp*cq) (productoDeGrado rp rq)
    where gp = grado p
          gq = grado q
          cp = coefLider p
          cq = coefLider q
          rp = restoPol p
          rq = restoPol q

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Una posición (i,j) de una matriz p es un cruce si
-- todos los elementos de p que están fuera de la fila i y fuera de la
-- columna j son nulos. Una matriz cruz es una matriz que tiene algún
-- cruce. Por ejemplo, las siguientes matrices cruces
--    ( 0 0 2 0 )      ( 0 0 1 0 0 )      ( 0 0 6 0 0 )
--    ( 3 3 4 2 )      ( 0 0 2 0 0 )      ( 0 0 2 0 0 )
--    ( 0 0 0 0 )      ( 3 3 1 2 0 )      ( 3 5 1 2 5 )
--    ( 0 0 9 0 )      ( 0 0 9 0 0 )
-- ya que la 1ª tiene un cruce en (2,3), la 2ª en (3,3) y la 3ª en
-- (3,3).
--
-- Utilizaremos la librería Matrix para desarrollar este ejercicio.
--
-- Definir la función
--    esMatrizCruz :: Matrix Int -> Bool
-- tal que (esMatrizCruz m) comprueba si la matriz m es una matriz cruz.
-- Por ejemplo, dadas las matrices
--    p1 = M.matrix 3 3 (\ (i,j) -> (if any even [i,j] then 1 else 0))
--    p2 = M.matrix 3 4 (\ (i,j) -> (i+j))
-- entonces
--    esMatrizCruz p1  ==  True
--    esMatrizCruz p2  ==  False
-- ---------------------------------------------------------------------

p1, p2 :: M.Matrix Int
p1 = M.matrix 3 3 (\ (i,j) -> (if any even [i,j] then 1 else 0))
p2 = M.matrix 3 4 (uncurry (+))

-- 1ª definición
-- =============

esMatrizCruz :: M.Matrix Int -> Bool
esMatrizCruz p =
    or [esCruce p m n (i,j) | i <- [1..m], j <- [1..n]]
    where m = M.nrows p
          n = M.ncols p

-- (esCruce p m n (i,j)) se verifica si (i,j) es un cruce de p (que
-- tiene m filas y n columnas)
esCruce :: M.Matrix Int -> Int -> Int -> (Int,Int) -> Bool
esCruce p m n (i,j) =
    all (== 0) [p M.! (x,y) | x <- [1..i-1]++[i+1..m], 
                              y <- [1..j-1]++[j+1..n]]

-- 2ª definición de esCruce
esCruce2 :: M.Matrix Int -> Int -> Int -> (Int,Int) -> Bool
esCruce2 p m n (i,j) =
    M.minorMatrix i j p == M.zero (m-1) (n-1)

-- ----------------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función 
--    matrizCruz :: Int -> Int -> Int -> Int -> M.Matrix Int
-- tal que (matrizCruz m n i j) es la matriz cruz de dimensión (m,n) con
-- respecto a la posición (i,j), en la que el valor de cada elemento no nulo
-- es la distancia en línea recta a la posición (i,j), contando también esta
-- última. Por ejemplo,
--    λ> matrizCruz 3 3 (2,2)
--    ( 0 2 0 )
--    ( 2 1 2 )
--    ( 0 2 0 )
--    
--    λ> matrizCruz 4 5 (2,3)
--    ( 0 0 2 0 0 )
--    ( 3 2 1 2 3 )
--    ( 0 0 2 0 0 )
-


-    ( 0 0 3 0 0 )
--    
--    λ> matrizCruz 5 3 (2,3)
--    ( 0 0 2 )
--    ( 3 2 1 )
--    ( 0 0 2 )
--    ( 0 0 3 )
--    ( 0 0 4 )
-- ---------------------------------------------------------------------

matrizCruz :: Int -> Int -> (Int,Int) -> M.Matrix Int
matrizCruz m n (i,j) =
    M.matrix m n generador
    where generador (a,b) 
              | a == i    = 1 + abs (j - b)
              | b == j    = 1 + abs (i - a)
              | otherwise = 0


