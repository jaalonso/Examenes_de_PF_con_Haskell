-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 4º examen de evaluación continua (15 de marzo de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Array
import Graphics.Gnuplot.Simple

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Sea a(0), a(1), a(2), ... una sucesión de enteros
-- definida por: 
-- + a(0) = 1 y
-- + a(n) es la suma de los dígitos de todos los términos anteriores,
--   para n ≥ 1. 
-- 
-- Los primeros términos de la sucesión son:
--      1, 1, 2, 4, 8, 16, 23, 28, 38, 49, ...
-- 
-- Definir la constante
--    sucSumaDigitos :: [Integer]
-- tal que sucSumaDigitos es la sucesión anterior. Por ejemplo, 
--    take 10 sucSumaDigitos   == [1,1,2,4,8,16,23,28,38,49]
--    sucSumaDigitos !! (10^3) == 16577
--    sucSumaDigitos !! (10^4) == 213677
--    sucSumaDigitos !! (10^5) == 2609882
--    sucSumaDigitos !! (10^6) == 31054319
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

sucSumaDigitos1 :: [Integer]
sucSumaDigitos1 = map termino [0..]

termino :: Integer -> Integer
termino 0 = 1
termino n = sum [sumaDigitos (termino k) | k <- [0..n-1]]

-- 2ª definición
-- =============

sucSumaDigitos2 :: [Integer]
sucSumaDigitos2 = 1 : iterate f 1
  where f x = x + sumaDigitos x

sumaDigitos :: Integer -> Integer
sumaDigitos x | x < 10 = x
              | otherwise = x `mod` 10 + sumaDigitos (x `div` 10)

-- 3ª definición
-- =============

sucSumaDigitos3 :: [Integer]
sucSumaDigitos3 = 1 : unfoldr (\x -> Just (x, f x)) 1
  where f x = x + sumaDigitos x


-- Comparación de eficiencia
-- =========================

--    λ> maximum (take 23 sucSumaDigitos1)
--    161
--    (11.52 secs, 1,626,531,216 bytes)
--    λ> maximum (take 23 sucSumaDigitos2)
--    161
--    (0.01 secs, 153,832 bytes)
--    λ> maximum (take 23 sucSumaDigitos3)
--    161
--    (0.01 secs, 153,072 bytes)
--
--    λ> maximum (take 200000 sucSumaDigitos2)
--    5564872
--    (1.72 secs, 233,208,200 bytes)
--    λ> maximum (take 200000 sucSumaDigitos3)
--    5564872
--    (3.46 secs, 438,250,928 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función
--    grafica :: Integer -> IO ()
-- tal que (grafica n) es la gráfica de los primeros n términos de la
-- sucesión anterior.
-- ---------------------------------------------------------------------

grafica :: Integer -> IO ()
grafica n =
  plotList [Key Nothing] (genericTake n sucSumaDigitos2)

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. La suma de las sumas de los cuadrados de los
-- divisores de los 6 primeros números enteros positivos es
--      1² + (1²+2²) + (1²+3²) + (1²+2²+4²) + (1²+5²) + (1²+2²+3²+6²)
--    = 1  + 5       + 10      + 21         + 26      + 50
--    = 113
-- 
-- Definir la función
--    sumaSumasCuadradosDivisores :: Integer -> Integer
-- tal que (sumaSumasCuadradosDivisores n) es la suma de las sumas de
-- los cuadrados de los divisores de los n primeros números enteros
-- positivos. Por ejemplo,
--    sumaSumasCuadradosDivisores 6       ==  113
--    sumaSumasCuadradosDivisores (10^3)  ==  401382971
--    sumaSumasCuadradosDivisores (10^6)  ==  400686363385965077
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

sumaSumasCuadradosDivisores1 :: Integer -> Integer
sumaSumasCuadradosDivisores1 n =
  sum (map (^2) (concatMap divisores [1..n]))
 
-- (divisores x) es la lista de divisores de n. Por ejemplo, 
--    divisores 6 == [1,2,3,6]
divisores :: Integer -> [Integer]
divisores n = [y | y <- [1..n], n `mod` y == 0]
 
-- 2ª definición 
sumaSumasCuadradosDivisores2 :: Integer -> Integer
sumaSumasCuadradosDivisores2 x =
  sum (zipWith (*) (map (x `div`) xs) (map (^2) xs))
  where xs = [1..x]

-- 3ª definición              
sumaSumasCuadradosDivisores3 :: Integer -> Integer
sumaSumasCuadradosDivisores3 n =
  sum $ zipWith (*) ((map (^2)  xs)) (zipWith div (repeat n) xs) 
  where xs = takeWhile (<= n) [1..]

-- 4ª definición
sumaSumasCuadradosDivisores4 :: Integer -> Integer
sumaSumasCuadradosDivisores4 n =
  sum [k^2 * (n `div` k) | k <- [1..n]]

-- Comparación de eficiencia
-- =========================

--    λ> sumaSumasCuadradosDivisores1 (2*10^3)
--    3208172389
--    (3.10 secs, 412,873,104 bytes)
--    λ> sumaSumasCuadradosDivisores2 (2*10^3)
--    3208172389
--    (0.03 secs, 2,033,352 bytes)
--    λ> sumaSumasCuadradosDivisores3 (2*10^3)
--    3208172389
--    (0.03 secs, 2,178,496 bytes)
--    λ> sumaSumasCuadradosDivisores4 (2*10^3)
--    3208172389
--    (0.03 secs, 1,924,072 bytes)
--
--    λ> sumaSumasCuadradosDivisores2 (4*10^5)
--    25643993117893355
--    (1.93 secs, 378,385,664 bytes)
--    λ> sumaSumasCuadradosDivisores3 (4*10^5)
--    25643993117893355
--    (2.08 secs, 407,185,792 bytes)
--    λ> sumaSumasCuadradosDivisores4 (4*10^5)
--    25643993117893355

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función
--    sumaSumasCuadradosDivisoresInter :: IO ()
-- que realice lo mismo que la función sumaSumasCuadradosDivisores, pero
-- de forma interactiva. Por ejemplo,
--    λ> sumaSumasCuadradosDivisoresInter
--       Escribe un número: 
--       1234
--       La suma de los cuadrados de sus divisores es: 753899047
-- ---------------------------------------------------------------------

sumaSumasCuadradosDivisoresInter :: IO ()
sumaSumasCuadradosDivisoresInter = do
  putStrLn "Escribe un número: "
  c <- getLine
  let n = read c
  putStrLn ("La suma de los cuadrados de sus divisores es: "
            ++ show (sumaSumasCuadradosDivisores4 n))

-- ---------------------------------------------------------------------
-- Ejercicio 3. Dados los vectores v = [1,2,3,4] y w = [1,-2,5,0] con
-- el primer elemento común, construimos una matriz 4x4, cuya primera
-- fila es v, su primera columna es w, y de forma que cada elemento es
-- la suma de sus tres vecinos que ya tienen un valor.
--
-- La matriz se construye de forma incremental como sigue:
--     1  2  3  4       1  2  3  4      1  2  3  4       1  2   3   4
--    -2               -2  1           -2  1  6         -2  1   6  13
--     5                5               5  4             5  4  11
--     0                0               0                0  9
-- 
-- Definir la función
--    matrizG :: Array Int Int -> Array Int Int -> Array (Int,Int) Int
-- tal que (matrizG v w) es la matriz cuadrada generada por los vectores
-- v y w (que se supone que tienen la misma dimensión). Por ejemplo,
--    λ> matrizG (listArray (1,4) [1..4]) (listArray (1,4) [1,-2,5,0])
--    array ((1,1),(4,4)) [((1,1), 1),((1,2),2),((1,3), 3),((1,4), 4),
--                         ((2,1),-2),((2,2),1),((2,3), 6),((2,4),13),
--                         ((3,1), 5),((3,2),4),((3,3),11),((3,4),30),
--                         ((4,1), 0),((4,2),9),((4,3),24),((4,4),65)]
-- ---------------------------------------------------------------------

matrizG :: Array Int Int -> Array Int Int -> Array (Int,Int) Int
matrizG v w = p
  where p = array ((1,1), (n,n))
                  [((i,j), f i j) | i <- [1..n], j <- [1..n]]
        n = snd $ bounds v
        f 1 j = v ! j
        f i 1 = w ! i
        f i j = p ! (i-1,j-1) + p!(i-1,j) + p!(i,j-1)

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Los árboles se pueden representar mediante
-- el siguiente tipo de datos 
--    data Arbol a = N a [Arbol a]
--                   deriving Show
-- Por ejemplo, los árboles
--      1           1            1
--     / \         / \          /|\
--    2   3       2   3        / | \  
--   / \          |           2  7 3  
--  4   5         4          / \      
--                          4   5     
--                  
-- se representan por
--    ej1, ej2, ej3 :: Arbol Int
--    ej1 = N 1 [N 2 [N 4 [], N 5 []], N 3 []]
--    ej2 = N 1 [N 2 [N 4 []], N 3 []]
--    ej3 = N 1 [N 2 [N 4 [], N 5 []], N 7 [], N 3 []]
--
-- Definir la función
--    descomposicion :: Arbol t -> [(t, [t])]
-- tal que (descomposicion ar) es una lista de pares  (x,xs) donde x es
-- un nodo y xs es la lista de hijos de x. Por ejemplo,
--    descomposicion ej1 == [(1,[2,3]),(2,[4,5]),(4,[]),(5,[]),(3,[])]
--    descomposicion ej2 == [(1,[2,3]),(2,[4]),(4,[]),(3,[])]
--    descomposicion ej3 == [(1,[2,7,3]),(2,[4,5]),(4,[]),(5,[]),
--                           (7,[]),(3,[])]
-- ---------------------------------------------------------------------

data Arbol a = N a [Arbol a]
  deriving (Show,Eq)
 
ej1, ej2, ej3 :: Arbol Int
ej1 = N 1 [N 2 [N 4 [], N 5 []], N 3 []]
ej2 = N 1 [N 2 [N 4 []], N 3 []]
ej3 = N 1 [N 2 [N 4 [], N 5 []], N 7 [], N 3 []]

descomposicion :: Arbol t -> [(t, [t])]
descomposicion (N r []) = [(r,[])]
descomposicion (N r as) = (r, map raiz as) : concatMap descomposicion as

-- (raiz t) es la raíz del árbol t. Por ejemplo,
--    raiz (N 8 [N 2 [N 4 []], N 3 []])  ==  8
raiz :: Arbol t -> t
raiz (N r _) = r

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función recíproca
--    descAarbol :: Eq t => [(t, [t])] -> Arbol t
-- tal que (descAarbol ps) es el árbol correspondiente a ps. Ejemplos,
--    (descAarbol (descomposicion ej1) == ej1) == True
--    (descAarbol (descomposicion ej2) == ej2) == True
--    (descAarbol (descomposicion ej3) == ej3) == True
-- ---------------------------------------------------------------------

descAarbol :: Eq t => [(t, [t])] -> Arbol t
descAarbol ps@((r,xs):qs) = aux r ps
  where aux r [] = N r []
        aux r ps = N r [aux h qs | h <- hs]
          where hs = head [xs | (x,xs) <- ps, x == r]
                qs = delete (r,hs) ps

