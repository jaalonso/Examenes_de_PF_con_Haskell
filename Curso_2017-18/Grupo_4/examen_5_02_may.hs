-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 5º examen de evaluación continua (2 de mayo de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías                                                        --
-- ---------------------------------------------------------------------

import Data.List 
import Data.Numbers.Primes
import Data.Array
import I1M.PolOperaciones
import qualified Data.Map as M

-- ---------------------------------------------------------------------
-- Ejercicio 1. Se considera una enumeración de los números primos:
--     p(1) = 2, p(2) = 3, p(3) = 5, p(4) = 7, p(5) = 11, ...
-- 
-- Dado un entero x > 1, su altura prima es el mayor i tal que el
-- primo p(i) aparece en la factorización de x en números primos. Por
-- ejemplo, la altura prima de 3500 es 4, pues 3500=2^2*5^3*7^1 y la de
-- 34 tiene es 7, pues 34 = 2*17. Ademaś, la altura prima de 1 es 0.
--
-- Definir la función
--    alturasPrimas      :: Integer -> [Integer]
-- tal que (alturasPrimas n) es la lista de las alturas primas de los
-- primeros n números enteros positivos. Por ejemplo,
--    alturasPrimas 15  ==  [0,1,2,1,3,2,4,1,2,3,5,2,6,4,3]
--    maximum (alturasPrimas 10000)  ==  1229
--    maximum (alturasPrimas 20000)  ==  2262
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

alturasPrimas :: Integer -> [Integer]
alturasPrimas n = map alturaPrima [1..n]

-- (alturaPrima x) es la altura prima de x. Por ejemplo,
--    alturaPrima 3500  ==  4
--    alturaPrima 34    ==  7
alturaPrima :: Integer -> Integer
alturaPrima 1 = 0
alturaPrima n = indice (mayorFactorPrimo n)

-- (mayorFactorPrimo n) es el mayor factor primo de n. Por ejemplo,
--    mayorFactorPrimo 3500  ==  7
--    mayorFactorPrimo 34    ==  17
mayorFactorPrimo :: Integer -> Integer
mayorFactorPrimo = last . primeFactors

-- (indice p) es el índice de p en la sucesión de los números
-- primos. Por ejemplo,
--    indice 7   ==  4
--    indice 17  ==  7
indice :: Integer -> Integer
indice p = genericLength (takeWhile (<=p) primes)

-- 2ª definición
-- =============

alturasPrimas2 :: Integer -> [Integer]
alturasPrimas2 n = map alturaPrima2 [1..n]

alturaPrima2 :: Integer -> Integer
alturaPrima2 n = v ! n
  where v = array (1,n) [(i,f i) | i <- [1..n]]
        f 1 = 0
        f k | isPrime k = indice2 k
            | otherwise = v ! k `div` head (primeFactors k)

indice2 :: Integer -> Integer
indice2 p = head [n | (x,n) <- indicesPrimos, x == p]

-- indicesPrimos es la suceción formada por los números primos y sus
-- índices. Por ejemplo,
--    λ> take 10 indicesPrimos
--    [(2,1),(3,2),(5,3),(7,4),(11,5),(13,6),(17,7),(19,8),(23,9),(29,10)]
indicesPrimos :: [(Integer,Integer)]
indicesPrimos = zip primes [1..]

-- 3ª definición
-- =============

alturasPrimas3 :: Integer -> [Integer]
alturasPrimas3 n = elems v 
  where v = array (1,n) [(i,f i) | i <- [1..n]]
        f 1 = 0
        f k | isPrime k = indice2 k
            | otherwise = v ! k `div` head (primeFactors k)

-- Comparación de eficiencia
-- =========================

--    λ> maximum (alturasPrimas 5000)
--    669
--    (2.30 secs, 1,136,533,912 bytes)
--    λ> maximum (alturasPrimas2 5000)
--    669
--    (12.51 secs, 3,595,318,584 bytes)
--    λ> maximum (alturasPrimas3 5000)
--    669
--    (0.27 secs, 75,110,896 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Una partición prima de un número natural n es un
-- conjunto de primos cuya suma es n. Por ejemplo, el número 7 tiene 7
-- particiones primas ya que 
--    7 = 7 = 5 + 2 = 3 + 2 + 2
--
-- Definir la función
--    particiones :: Int -> [[Int]]
-- tal que (particiones n) es el conjunto de las particiones primas de
-- n. Por ejemplo,
--    particiones 7             ==  [[7],[5,2],[3,2,2]]
--    particiones 8             ==  [[5,3],[3,3,2],[2,2,2,2]]
--    particiones 9             ==  [[7,2],[5,2,2],[3,3,3],[3,2,2,2]]
--    length (particiones 90)   ==  20636
--    length (particiones 100)  ==  40899
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

particiones1 :: Int -> [[Int]]
particiones1 0 = [[]]
particiones1 n = [x:y | x <- xs, 
                        y <- particiones1 (n-x), 
                        [x] >= take 1 y]
  where xs = reverse (takeWhile (<= n) primes)

-- 2ª solución (con programación dinámica)
-- =======================================

particiones2 :: Int -> [[Int]]
particiones2 n = vectorParticiones n ! n

-- (vectorParticiones n) es el vector con índices de 0 a n tal que el
-- valor del índice k es la lista de las particiones primas de k. Por
-- ejemplo, 
--    λ> mapM_ print (elems (vectorParticiones 9))
--    [[]]
--    []
--    [[2]]
--    [[3]]
--    [[2,2]]
--    [[5],[3,2]]
--    [[3,3],[2,2,2]]
--    [[7],[5,2],[3,2,2]]
--    [[5,3],[3,3,2],[2,2,2,2]]
--    [[7,2],[5,2,2],[3,3,3],[3,2,2,2]]
--    λ> elems (vectorParticiones 9) == map particiones1 [0..9]
--    True
vectorParticiones :: Int -> Array Int [[Int]]
vectorParticiones n = v where
  v = array (0,n) [(i,f i) | i <- [0..n]]
    where f 0 = [[]]
          f m = [x:y | x <- xs, 
                       y <- v ! (m-x), 
                       [x] >= take 1 y]
            where xs = reverse (takeWhile (<= m) primes)

-- Comparación de eficiencia
-- =========================

--    λ> length (particiones1 35)
--    175
--    (5.88 secs, 2,264,266,040 bytes)
--    λ> length (particiones2 35)
--    175
--    (0.02 secs, 1,521,560 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función 
--    polDiagonal :: Array (Int,Int) Int  -> Polinomio Int
-- tal que (polDiagonal p) es el polinomio cuyas raíces son los
-- elementos de la diagonal de la matriz cuadrada p. Por ejemplo,
--    λ> polDiagonal (listArray ((1,1),(2,2)) [1..])
--    x^2 + -5*x + 4
-- ya que los elementos de la diagonal son 1 y 4 y
--    (x - 1) * (x - 4) = x^2 + -5*x + 4
-- Otros ejemplos
--    λ> polDiagonal (listArray ((1,1),(3,4)) [-12,-11..1])
--    x^3 + 21*x^2 + 122*x + 168
--    λ> polDiagonal (listArray ((1,1),(4,3)) [-12,-11..1])
--    x^3 + 24*x^2 + 176*x + 384
-- ---------------------------------------------------------------------

polDiagonal :: Array (Int,Int) Int -> Polinomio Int
polDiagonal m = multListaPol (map f (diagonal m))
    where f a = consPol 1 1 (consPol 0 (-a) polCero)

-- (diagonal p) es la lista de los elementos de la diagonal de la matriz
-- p. Por ejemplo,
--    diagonal (listArray ((1,1),(3,3)) [1..])  ==  [1,5,9]
diagonal :: Num a => Array (Int,Int) a -> [a]
diagonal p = [p ! (i,i) | i <- [1..min m n]]
    where (_,(m,n)) = bounds p

-- (multListaPol ps) es el producto de los polinomios de la lista ps.
multListaPol :: [Polinomio Int] -> Polinomio Int
multListaPol []     = polUnidad
multListaPol (p:ps) = multPol p (multListaPol ps)

-- 2ª definición de multListaPol
multListaPol2 :: [Polinomio Int] -> Polinomio Int
multListaPol2 = foldr multPol polUnidad

-- ---------------------------------------------------------------------
-- Ejercicio 4. El inverso de un diccionario d es el diccionario que a
-- cada valor x le asigna la lista de claves cuyo valor en d es x. Por
-- ejemplo, el inverso de
--    [("a",3),("b",2),("c",3),("d",2),("e",1)])
-- es
--    [(1,["e"]),(2,["d","b"]),(3,["c","a"])]
-- 
-- Definir la función
--    inverso :: (Ord k, Ord v) => M.Map k v -> M.Map v [k]
-- tal que (inverso d) es el inverso del diccionario d. Por ejemplo,
--    λ> inverso (M.fromList [("a",3),("b",2),("c",3),("d",2),("e",1)])
--    fromList [(1,["e"]),(2,["d","b"]),(3,["c","a"])]
--    λ> inverso (M.fromList [(x,x^2) | x <- [-3,-2..3]])
--    fromList [(0,[0]),(1,[1,-1]),(4,[2,-2]),(9,[3,-3])]
-- ---------------------------------------------------------------------

-- 1ª definición
inverso :: (Ord k, Ord v) => M.Map k v -> M.Map v [k]
inverso d = M.fromListWith (++) [(y,[x]) | (x,y) <- M.assocs d]

-- 2ª definición
inverso2 :: (Ord k, Ord v) => M.Map k v -> M.Map v [k]
inverso2 d
  | M.null d  = M.empty
  | otherwise = M.insertWith (++) y [x] (inverso2 e)
  where ((x,y),e) = M.deleteFindMin d
