-- Informática (1º del Grado en Matemáticas)
-- Examen de la convocatoria de Septiembre (8 de septiembre de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Librerías auxiliares
-- ---------------------------------------------------------------------

import Data.Array
import Data.List
import Data.Numbers.Primes
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. La órbita prima de un número n es la sucesión construida
-- de la siguiente forma:
-- + si n es compuesto su órbita no tiene elementos 
-- + si n es primo, entonces su órbita está formada por n y la órbita del
--   número obtenido sumando n y sus dígitos. 
-- Por ejemplo, para el número 11 tenemos:
--   11 es primo, consideramos 11+1+1 = 13
--   13 es primo, consideramos 13+1+3 = 17
--   17 es primo, consideramos 17+1+7 = 25
--   25 es compuesto, su órbita está vacía
-- Así, la órbita prima de 11 es [11, 13, 17].
-- 
-- Definir la función
--    orbita :: Integer -> [Integer]
-- tal que (orbita n) es la órbita prima de n. Por ejemplo,
--    orbita 11 == [11,13,17]
--    orbita 59 == [59,73,83]
-- 
-- Calcular el menor número cuya órbita prima tiene más de 3 elementos.
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión)
-- =============================

orbita :: Integer -> [Integer]
orbita n | not (isPrime n) = []
         | otherwise       = n : orbita (n + sum (cifras n))

-- (cifras n) es la lista de las cifras de n. Por ejemplo,
--    cifras 32542 == [3,2,5,4,2]
cifras :: Integer -> [Integer]
cifras n = [read [x]| x <- show n]

-- 2ª definición (con iterate)
-- ===========================

orbita2 :: Integer -> [Integer]
orbita2 n = takeWhile isPrime (iterate f n)
  where f x = x + sum (cifras x)

-- Comprobación de la equivalencia de las definiciones de 'orbita'
-- ===============================================================

--    > quickCheck prop_equiv_orbita
--    +++ OK, passed 100 tests.
prop_equiv_orbita :: Integer -> Bool
prop_equiv_orbita n =
  orbita n == orbita2 n

-- El cálculo es
--    > head [x | x <- [1,3..], length (orbita x) > 3]
--    277
-- 
--    > orbita 277
--    [277,293,307,317]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    sumas :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
-- tal que (sumas n ns x) es la lista de las descomposiciones de x como
-- sumas de n sumandos de la lista ns. Por ejemplo,
--    sumas 2 [1,2] 3   == [[1,2]]
--    sumas 2 [-1] (-2) == [[-1,-1]]
--    sumas 2 [-1,3,-1] 2 == [[-1,3]]
--    sumas 2 [1,2] 4     == [[2,2]]
--    sumas 2 [1,2] 5     == []
--    sumas 3 [1,2] 5     == [[1,2,2]]
--    sumas 3 [1,2] 6     == [[2,2,2]]
--    sumas 2 [1,2,5] 6   == [[1,5]]
--    sumas 2 [1,2,3,5] 4 == [[1,3],[2,2]]
--    sumas 2 [1..5] 6    == [[1,5],[2,4],[3,3]]
--    sumas 3 [1..5] 7    == [[1,1,5],[1,2,4],[1,3,3],[2,2,3]]
--    sumas 3 [1..200] 4  == [[1,1,2]]
-- ---------------------------------------------------------------------

-- 1ª definición (fuerza bruta)
-- ============================

sumas1 :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
sumas1 n ns x = 
  [xs | xs <- combinacionesR n (nub (sort ns))
      , sum xs == x]

-- (combinacionesR k xs) es la lista de las combinaciones orden
-- k de los elementos de xs con repeticiones. Por ejemplo,
--    combinacionesR 2 "abc" == ["aa","ab","ac","bb","bc","cc"]
--    combinacionesR 3 "bc"  == ["bbb","bbc","bcc","ccc"]
--    combinacionesR 3 "abc" == ["aaa","aab","aac","abb","abc","acc",
--                               "bbb","bbc","bcc","ccc"]
combinacionesR :: Int -> [a] -> [[a]]
combinacionesR _ [] = []
combinacionesR 0 _  = [[]]
combinacionesR k (x:xs) =
  [x:ys | ys <- combinacionesR (k-1) (x:xs)] ++ combinacionesR k xs

-- 2ª definición (divide y vencerás)
-- =================================

sumas2 :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
sumas2 n ns x = nub (sumasAux n ns x)
  where sumasAux :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
        sumasAux 1 ns' x'
          | x' `elem` ns' = [[x']]
          | otherwise   = []
        sumasAux n' ns' x' = 
          concat [[y:zs | zs <- sumasAux (n'-1) ns' (x'-y)
                        , y <= head zs]
                 | y <- ns']

-- 3ª definición
-- =============

sumas3 :: (Num a, Ord a) => Int -> [a] -> a -> [[a]]
sumas3 n ns x = nub $ aux n (sort ns) x
  where aux 0 _ _  = []
        aux _ [] _ = []
        aux 1 ys x | x `elem` ys = [[x]]
                   | otherwise   = []
        aux n (y:ys) x = aux n ys x ++
                         map (y:) (aux (n - 1) (y : ys) (x - y))

-- Equivalencia de las definiciones
-- ================================

-- (prop_equiv_sumas n ns x) se verifica si las definiciones de
-- 'sumas' son equivalentes para n, ns y x. Por ejemplo,
--    > quickCheckWith (stdArgs {maxSize=7}) prop_equiv_sumas
--    +++ OK, passed 100 tests.
prop_equiv_sumas :: Positive Int -> [Int] -> Int -> Bool
prop_equiv_sumas (Positive n) ns x =
  all (== normal (sumas1 n ns x))
      [ normal (sumas2 n ns x)
      , normal (sumas3 n ns x)]
  where normal = sort . map sort

-- Comparación de eficiencia
-- =========================

--    > sumas1 3 [1..200] 4
--    [[1,1,2]]
--    (2.52 secs, 1,914,773,472 bytes)
--    > sumas2 3 [1..200] 4
--    [[1,1,2]]
--    (0.17 secs, 25,189,688 bytes)
--    ghci> sumas3 3 [1..200] 4
--    [[1,1,2]]
--    (0.08 secs, 21,091,368 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    diagonalesPrincipales :: Array (Int,Int) a -> [[a]]
-- tal que (diagonalesPrincipales p) es la lista de las diagonales
-- principales de la matriz p. Por ejemplo, para la matriz
--    1  2  3  4
--    5  6  7  8
--    9 10 11 12
-- la lista de sus diagonales principales es 
--    [[9],[5,10],[1,6,11],[2,7,12],[3,8],[4]]
-- En Haskell,
--    > diagonalesPrincipales (listArray ((1,1),(3,4)) [1..12])
--    [[9],[5,10],[1,6,11],[2,7,12],[3,8],[4]]
-- ---------------------------------------------------------------------

diagonalesPrincipales :: Array (Int,Int) a -> [[a]]
diagonalesPrincipales p = 
    [[p!ij1 | ij1 <- extension ij] | ij <- iniciales] 
    where (_,(m,n)) = bounds p
          iniciales = [(i,1) | i <- [m,m-1..2]] ++ [(1,j) | j <- [1..n]] 
          extension (i,j) = [(i+k,j+k) | k <- [0..min (m-i) (n-j)]]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Las expresiones aritméticas formadas por sumas de
-- números y variables se pueden representar mediante el siguiente tipo
-- de datos 
--    data Exp = N Int
--             | V String
--             | S Exp Exp
--      deriving Show
-- Por ejemplo, la expresión "(x + 3) + (2 + y)" se representa por
--    S (S (V "x") (N 3)) (S (N 2) (V "y"))
--
-- Definir la función
--    simplificada :: Exp -> Exp
-- tal que (simplificada e) es una expresión equivalente a la expresión
-- e pero sólo con un número como máximo. Por ejemplo,
--    > simplificada (S (S (V "x") (N 3)) (S (N 2) (V "y")))
--    S (N 5) (S (V "x") (V "y"))
--    > simplificada (S (S (S (V "x") (N 3)) (S (N 2) (V "y"))) (N (-5)))
--    S (V "x") (V "y")
--    ghci> simplificada (S (V "x") (V "y"))
--    S (V "x") (V "y")
--    ghci> simplificada (S (N 2) (N 3))
--    N 5
-- ---------------------------------------------------------------------

data Exp = N Int
         | V String
         | S Exp Exp
  deriving Show

simplificada :: Exp -> Exp
simplificada e
  | null vs   = N x
  | x == 0    = e1
  | otherwise = S (N x) e1
  where x  = numero e
        vs = variables e
        e1 = sumaVariables vs
        
-- (numero e) es el número obtenido sumando los números de la expresión
-- e. Por ejemplo,
--    numero (S (S (V "x") (N 3)) (S (N 2) (V "y")))              == 5
--    numero (S (S (S (V "x") (N 3)) (S (N 2) (V "y"))) (N (-5))) == 0
--    numero (S (V "x") (V "y"))                                  == 0
numero :: Exp -> Int
numero (N x)     = x
numero (V _)     = 0
numero (S e1 e2) = numero e1 + numero e2

-- (variables e) es la lista de las variables de la expresión e. Por
-- ejemplo, 
--    variables (S (S (V "x") (N 3)) (S (N 2) (V "y")))   == ["x","y"]
--    variables (S (S (V "x") (N 3)) (S (V "y") (V "x"))) == ["x","y","x"]
--    variables (S (N 2) (N 3))                           == []
variables :: Exp -> [String]
variables (N _)     = []
variables (V x)     = [x]
variables (S e1 e2) = variables e1 ++ variables e2

-- (sumaVariables xs) es la expresión obtenida sumando las variables
-- xs. Por ejemplo,
--    ghci> sumaVariables ["x","y","z"]
--    S (V "x") (S (V "y") (V "z"))
--    ghci> sumaVariables ["x"]
--    V "x"
sumaVariables :: [String] -> Exp
sumaVariables [x]    = V x
sumaVariables (x:xs) = S (V x) (sumaVariables xs)
