-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 5º examen de evaluación continua (4 de mayo de 2016)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Numbers.Primes
import Data.Matrix
import Test.QuickCheck
import qualified Data.Set as S
import qualified Data.Map as M

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Un número entero positivo n es balanceado si n = 1 o
-- se puede escribir como un producto de un número par de factores
-- primos (no necesariamente distintos). 
--
-- Definir la función
--    balanceado :: Int -> Bool
-- tal que (balanceado n) se verifica si n es balanceado. Por ejemplo,
--    balanceado 34  ==  True
--    balanceado 35  ==  True
--    balanceado 44  ==  False
-- ---------------------------------------------------------------------

balanceado :: Int -> Bool
balanceado 1 = True
balanceado n = n > 1 && even (length (primeFactors n))

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Un par (a,b) de enteros positivos es un balanceador
-- del número x si el producto (x+a)*(x+b) es balanceado.
--
-- Definir la función
--    balanceadores :: Int -> [(Int,Int)]
-- tal que (balanceadores x) es la lista de los balanceadores de x. Por
-- ejemplo, 
--    take 5 (balanceadores 3)  ==  [(1,1),(1,3),(2,2),(3,1),(2,4)]
--    take 5 (balanceadores 5)  ==  [(1,1),(2,2),(1,4),(2,3),(3,2)]
-- ---------------------------------------------------------------------

balanceadores :: Int -> [(Int,Int)]
balanceadores x = 
    [(a,b) | (a,b) <- enteros, 
             balanceado ((x+a)*(x+b))]

--    ghci> take 10 enteros
--    [(1,1),(1,2),(2,1),(1,3),(2,2),(3,1),(1,4),(2,3),(3,2),(4,1)]
enteros :: [(Int,Int)]
enteros = [(a,n-a) | n <- [1..],
                     a <- [1..n-1]]

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comprobar con QuickCheck si para cualquier entero
-- positivo x, la lista (balanceadores x) contiene a todos los pares 
-- (a,a) con a mayor que 0.
-- --------------------------------------------------------------------- 

prop_balanceadores :: Positive Int-> Int -> Property
prop_balanceadores (Positive x) a =
    a > 0 ==> balanceado ((x+a)*(x+a))

--    ghci> quickCheck prop_balanceadores
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2. Un sistema de ecuaciones lineales Ax = b es triangular
-- inferior si todos los elementos de la matriz A que están por encima
-- de la diagonal principal son nulos; es decir, es de la forma
--    a(1,1)*x(1)                                               = b(1)
--    a(2,1)*x(1) + a(2,2)*x(2)                                 = b(2)
--    a(3,1)*x(1) + a(3,2)*x(2) + a(3,3)*x(3)                   = b(3)
--    ...
--    a(n,1)*x(1) + a(n,2)*x(2) + a(n,3)*x(3) +...+ a(x,x)*x(n) = b(n)
--
-- El sistema es compatible si, y sólo si, el producto de los elementos
-- de la diagonal principal es distinto de cero. En este caso, la
-- solución se puede calcular mediante el algoritmo de bajada: 
--    x(1) = b(1) / a(1,1)
--    x(2) = (b(2) - a(2,1)*x(1)) / a(2,2)
--    x(3) = (b(3) - a(3,1)*x(1) - a(3,2)*x(2)) / a(3,3)
--    ...
--    x(n) = (b(n) - a(n,1)*x(1) - a(n,2)*x(2) -...- a(n,n-1)*x(n-1)) / a(n,n)
-- 
-- Definir la función 
--    bajada :: Matrix Double -> Matrix Double -> Matrix Double
-- tal que (bajada a b) es la solución, mediante el algoritmo de bajada,
-- del sistema compatible triangular superior ax = b. Por ejemplo, 
--    ghci> let a = fromLists [[2,0,0],[3,1,0],[4,2,5.0]]
--    ghci> let b = fromLists [[3],[6.5],[10]]
--    ghci> bajada a b
--    ( 1.5 )
--    ( 2.0 )
--    ( 0.0 )
-- Es decir, la solución del sistema
--    2x            = 3
--    3x + y        = 6.5
--    4x + 2y + 5 z = 10
-- es x = 1.5, y = 2 y z = 0.
-- ---------------------------------------------------------------------

bajada :: Matrix Double -> Matrix Double -> Matrix Double
bajada a b = fromLists [[x i] | i <- [1..m]]
    where m = nrows a
          x k = (b!(k,1) - sum [a!(k,j) * x j | j <- [1..k-1]]) / a!(k,k)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    clasesEquivalencia :: Ord a => 
--                          S.Set a -> (a -> a -> Bool) -> S.Set (S.Set a)
-- tal que (clasesEquivalencia xs r) es la lista de las clases de
-- equivalencia de xs respecto de la relación de equivalencia r. Por
-- ejemplo,
--    ghci> let c = S.fromList [-3..3]
--    ghci> clasesEquivalencia c (\x y -> x `mod` 3 == y `mod` 3)
--    fromList [fromList [-3,0,3],fromList [-2,1],fromList [-1,2]]
--    ghci> clasesEquivalencia c (\x y -> (x - y) `mod` 2 == 0)
--    fromList [fromList [-3,-1,1,3],fromList [-2,0,2]]
-- ---------------------------------------------------------------------

clasesEquivalencia :: Ord a => 
                      S.Set a -> (a -> a -> Bool) -> S.Set (S.Set a)
clasesEquivalencia xs r 
    | S.null xs =  S.empty
    | otherwise =  us `S.insert` clasesEquivalencia vs r
    where (y,ys)  = S.deleteFindMin xs
          (us,vs) = S.partition (r y) xs

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Los polinomios se pueden representar mediante
-- diccionarios con los exponentes como claves y los coeficientes como 
-- valores. El tipo de los polinomios con coeficientes de tipo a se
-- define por 
--    type Polinomio a = M.Map Int a 
-- Dos ejemplos de polinomios (que usaremos en los ejemplos) son
--    3 + 7x - 5x^3
--    4 + 5x^3 + x^5
-- se definen por
--   ejPol1, ejPol2 :: Polinomio Int
--   ejPol1 = M.fromList [(0,3),(1,7),(3,-5)]
--   ejPol2 = M.fromList [(0,4),(3,5),(5,1)]
-- 
-- Definir la función
--    sumaPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
-- tal que (sumaPol p q) es la suma de los polinomios p y q. Por ejemplo,
--   ghci> sumaPol ejPol1 ejPol2
--   fromList [(0,7),(1,7),(5,1)]
--   ghci> sumaPol ejPol1 ejPol1
--   fromList [(0,6),(1,14),(3,-10)]
-- ---------------------------------------------------------------------

type Polinomio a = M.Map Int a 

ejPol1, ejPol2 :: Polinomio Int
ejPol1 = M.fromList [(0,3),(1,7),(3,-5)]
ejPol2 = M.fromList [(0,4),(3,5),(5,1)]

sumaPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
sumaPol p q = 
    M.filter (/=0) (M.unionWith (+) p q)

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función
--   multPorTerm :: Num a => (Int,a) -> Polinomio a -> Polinomio a
-- tal que (multPorTerm (n,a) p) es el producto del término ax^n por
-- p. Por ejemplo,
--   ghci> multPorTerm (2,3) (M.fromList [(0,4),(2,1)])
--   fromList [(2,12),(4,3)]
-- ---------------------------------------------------------------------

multPorTerm :: Num a => (Int,a) -> Polinomio a -> Polinomio a
multPorTerm (n,a) p =
    M.map (*a) (M.mapKeys (+n) p)

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Definir la función
--   multPol :: (Eq a, Num a) => Polinomio a -> Polinomio a -> Polinomio a
-- tal que (multPol p q) es el producto de los polinomios p y q. Por
-- ejemplo, 
--   ghci> multPol ejPol1 ejPol2
--   fromList [(0,12),(1,28),(3,-5),(4,35),(5,3),(6,-18),(8,-5)]
--   ghci> multPol ejPol1 ejPol1
--   fromList [(0,9),(1,42),(2,49),(3,-30),(4,-70),(6,25)]
--   ghci> multPol ejPol2 ejPol2
--   fromList [(0,16),(3,40),(5,8),(6,25),(8,10),(10,1)]
-- ---------------------------------------------------------------------

multPol :: (Eq a, Num a) => Polinomio a -> Polinomio a -> Polinomio a
multPol p q
    | M.null p  = M.empty
    | otherwise = sumaPol (multPorTerm t q) (multPol r q)
    where (t,r) = M.deleteFindMin p
