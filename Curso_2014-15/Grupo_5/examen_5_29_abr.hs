-- Informática (1º del Grado en Matemáticas)
-- 5º examen de evaluación continua (29 de abril de 2015)
-- ---------------------------------------------------------------------

import Data.Array 
import Data.List
import Data.Numbers.Primes
import I1M.Grafo
import I1M.Monticulo
import I1M.PolOperaciones
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Una propiedad del 2015 es que la suma de sus dígitos
-- coincide con el número de sus divisores; en efecto, la suma de sus
-- dígitos es 2+0+1+5=8 y tiene 8 divisores (1, 5, 13, 31, 65, 155, 403
-- y 2015). 
--    
-- Definir la sucesión
--    especiales :: [Int]
-- formada por los números n tales que la suma de los dígitos de n
-- coincide con el número de divisores de n. Por ejemplo,
--    take 12 especiales == [1,2,11,22,36,84,101,152,156,170,202,208]
--
-- Calcular la posición de 2015 en la sucesión de especiales.
-- ---------------------------------------------------------------------

especiales :: [Int]
especiales = [n | n <- [1..], sum (digitos n) == length (divisores n)]

digitos :: Int -> [Int]
digitos n = [read [d] | d <- show n]

divisores :: Int -> [Int]
divisores n = n : [x | x <- [1..n `div` 2], n `mod` x == 0]

-- El cálculo de número de años hasta el 2015 inclusive que han cumplido
-- la propiedad es
--    ghci> length (takeWhile (<=2015) especiales)
--    59

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función 
--    posicion :: Array Int Bool -> Maybe Int
-- tal que (posicion v) es la menor posición del vector de booleanos v
-- cuyo valor es falso y es Nothing si todos los valores son
-- verdaderos. Por ejemplo,
--    posicion (listArray (0,4) [True,True,False,True,False]) == Just 2
--    posicion (listArray (0,4) [i <= 2 | i <- [0..4]])       == Just 3
--    posicion (listArray (0,4) [i <= 7 | i <- [0..4]])       == Nothing
-- ---------------------------------------------------------------------

-- 1ª solución
posicion :: Array Int Bool -> Maybe Int
posicion v | p > n     = Nothing
           | otherwise = Just p
    where p = (length . takeWhile id . elems) v
          (_,n) = bounds v 

-- 2ª solución:
posicion2 :: Array Int Bool -> Maybe Int
posicion2 v | null xs   = Nothing
            | otherwise = Just (head xs)
    where xs = [i | i <- indices v, v!i]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    todos :: Ord a => (a -> Bool) -> Monticulo a -> Bool
-- tal que (todos p m) se verifica si todos los elementos del montículo
-- m cumple la propiedad p, Por ejemplo,
--    todos (>2) (foldr inserta vacio [6,3,4,8])  ==  True
--    todos even (foldr inserta vacio [6,3,4,8])  ==  False
-- ---------------------------------------------------------------------

todos :: Ord a => (a -> Bool) -> Monticulo a -> Bool
todos p m
    | esVacio m = True
    | otherwise = p (menor m) && todos p (resto m)

-- ---------------------------------------------------------------------
-- Ejercicio 4. El complementario del grafo G es un grafo G' del mismo
-- tipo que G (dirigido o no dirigido), con el mismo conjunto de nodos y
-- tal que dos nodos de G' son adyacentes si y sólo si no son adyacentes
-- en G. Los pesos de todas las aristas del complementario es igual a 0.
--
-- Definir la función
--    complementario :: Grafo Int Int -> Grafo Int Int
-- tal que (complementario g) es el complementario de g. Por ejemplo,
--    ghci> complementario (creaGrafo D (1,3) [(1,3,0),(3,2,0),(2,2,0),(2,1,0)])
--    G D (array (1,3) [(1,[(1,0),(2,0)]),(2,[(3,0)]),(3,[(1,0),(3,0)])])
--    ghci> complementario (creaGrafo D (1,3) [(3,2,0),(2,2,0),(2,1,0)])
--    G D (array (1,3) [(1,[(1,0),(2,0),(3,0)]),(2,[(3,0)]),(3,[(1,0),(3,0)])])
-- ---------------------------------------------------------------------

complementario :: Grafo Int Int -> Grafo Int Int
complementario g = 
    creaGrafo d (1,n) [(x,y,0) | x <- xs, y <- xs, not (aristaEn g (x,y))]
    where d  = if dirigido g then D else ND
          xs = nodos g
          n  = length xs

-- ---------------------------------------------------------------------
-- Ejercicio 5. En 1772, Euler publicó que el polinomio n² + n + 41
-- genera 40 números primos para todos los valores de n entre 0 y
-- 39. Sin embargo, cuando n=40, 40²+40+41 = 40(40+1)+41 es divisible
-- por 41.  
-- 
-- Definir la función
--    generadoresMaximales :: Integer -> (Int,[(Integer,Integer)])
-- tal que (generadoresMaximales n) es el par (m,xs) donde 
--    + xs es la lista de pares (x,y) tales que n²+xn+y es uno de los
--      polinomios que genera un número máximo de números primos
--      consecutivos a partir de cero entre todos los polinomios de la
--      forma n²+an+b, con |a| ≤ n y |b| ≤ n y
--    + m es dicho número máximo.
-- Por ejemplo,
--    generadoresMaximales    4  ==  ( 3,[(-2,3),(-1,3),(3,3)])
--    generadoresMaximales    6  ==  ( 5,[(-1,5),(5,5)])
--    generadoresMaximales   50  ==  (43,[(-5,47)])
--    generadoresMaximales  100  ==  (48,[(-15,97)])
--    generadoresMaximales  200  ==  (53,[(-25,197)])
--    generadoresMaximales 1650  ==  (80,[(-79,1601)])
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

generadoresMaximales1 :: Integer -> (Int,[(Integer,Integer)])
generadoresMaximales1 n = 
    (m,[((a,b)) | a <- [-n..n], b <- [-n..n], nPrimos a b == m])
    where m = maximum $ [nPrimos a b | a <- [-n..n], b <- [-n..n]]

-- (nPrimos a b) es el número de primos consecutivos generados por el
-- polinomio n² + an + b a partir de n=0. Por ejemplo,
--    nPrimos 1 41        ==  40
--    nPrimos (-79) 1601  ==  80
nPrimos :: Integer -> Integer -> Int
nPrimos a b =
    length $ takeWhile isPrime [n*n+a*n+b | n <- [0..]]

-- 2ª solución (reduciendo las cotas)
-- ==================================

-- Notas: 
-- 1. Se tiene que b es primo, ya que para n=0, se tiene que 0²+a*0+b =
--    b es primo. 
-- 2. Se tiene que 1+a+b es primo, ya que es el valor del polinomio para
--    n=1. 

generadoresMaximales2 :: Integer -> (Int,[(Integer,Integer)])
generadoresMaximales2 n = (m,map snd zs)
    where xs = [(nPrimos a b,(a,b)) | b <- takeWhile (<=n) primes,
                                      a <- [-n..n],
                                      isPrime(1+a+b)]
          ys = reverse (sort xs)
          m  = fst (head ys)
          zs = takeWhile (\(k,_) -> k == m) ys

-- 3ª solución (con la librería de polinomios)
-- ===========================================

generadoresMaximales3 :: Integer -> (Int,[(Integer,Integer)])
generadoresMaximales3 n = (m,map snd zs)
    where xs = [(nPrimos2 a b,(a,b)) | b <- takeWhile (<=n) primes,
                                      a <- [-n..n],
                                      isPrime(1+a+b)]
          ys = reverse (sort xs)
          m  = fst (head ys)
          zs = takeWhile (\(k,_) -> k == m) ys

-- (nPrimos2 a b) es el número de primos consecutivos generados por el
-- polinomio n² + an + b a partir de n=0. Por ejemplo,
--    nPrimos2 1 41        ==  40
--    nPrimos2 (-79) 1601  ==  80
nPrimos2 :: Integer -> Integer -> Int
nPrimos2 a b =
    length $ takeWhile isPrime [valor p n | n <- [0..]]
    where p = consPol 2 1 (consPol 1 a (consPol 0 b polCero))

-- Comparación de eficiencia
--    ghci> generadoresMaximales1 200
--    (53,[(-25,197)])
--    (3.06 secs, 720683776 bytes)
--    ghci> generadoresMaximales1 300
--    (56,[(-31,281)])
--    (6.65 secs, 1649274220 bytes)
--    
--    ghci> generadoresMaximales2 200
--    (53,[(-25,197)])
--    (0.25 secs, 94783464 bytes)
--    ghci> generadoresMaximales2 300
--    (56,[(-31,281)])
--    (0.51 secs, 194776708 bytes)
--
--    ghci> generadoresMaximales3 200
--    (53,[(-25,197)])
--    (0.20 secs, 105941096 bytes)
--    ghci> generadoresMaximales3 300
--    (56,[(-31,281)])
--    (0.35 secs, 194858344 bytes)
