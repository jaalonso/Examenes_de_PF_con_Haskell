-- Informática (1º del Grado en Matemáticas)
-- 6º examen de evaluación continua (12 de junio de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Librerías auxiliares
-- ---------------------------------------------------------------------

import Data.List
import Data.Array
import Data.Numbers.Primes
import Test.QuickCheck
import Graphics.Gnuplot.Simple

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. La función de Möebius,  está definida para todos
-- los enteros positivos ncomo sigue:
--    mu(n) =  1 si n es libre de cuadrados y tiene un número par de
--              factores primos distintos.
--    mu(n) = -1 si n es libre de cuadrados y tiene un número impar de
--              factores primos distintos.
--    mu(n) =  0 si n es divisible por algún cuadrado.
-- 
-- Definir la función
--    mu :: Int -> Int
-- tal que (mu n) es el valor mu(n). Por ejemplo:
--    mu 1    == 1
--    mu 1000 == 0
--    mu 3426 == -1
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

mu1 :: Int -> Int
mu1 n | divisibleAlgunCuadrado n = 0
      | even k                   = 1
      | otherwise                = -1
  where k = length (map fst (factorizacion n))

factorizacion :: (Integral t) => t -> [(t,t)]
factorizacion n = [(head ps, genericLength ps) | ps <- pss]
  where pss = group (primeFactors n)

divisibleAlgunCuadrado :: Int -> Bool
divisibleAlgunCuadrado = any (>1) . map snd . factorizacion

libreDeCuadrados :: Int -> Bool
libreDeCuadrados = not . divisibleAlgunCuadrado

-- 2ª definición
-- =============
  
mu2 :: Int -> Int
mu2 n | any (>1) es = 0
      | even k      = 1
      | otherwise   = -1
  where ps = factorizacion n
        k = length (map fst ps)
        es = map snd ps
        
-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que se verifica la siguiente
-- propiedad: la suma de mu(d), siendo d los divisores positivos de n, es
-- cero excepto cuando n = 1.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_mu :: Int -> Property
prop_mu n = abs n /= 1 ==> sum (map mu2 (divisores n)) == 0 
  where divisores x = [y | y <- [1..abs x], x `mod` y == 0]

-- La comprobación es
--    ghci> quickCheck prop_mu
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la función
--    graficaMu :: [Int] -> IO ()
-- tal que (graficaMu ns) dibuje la gráfica de la función tomando los
-- valores en la lista ns. 
-- ---------------------------------------------------------------------

graficaMu :: [Int] -> IO ()
graficaMu ns = plotList [] $ map mu2 ns

-- ---------------------------------------------------------------------
-- Ejercicio 2. Se dice que un número es generable si se puede escribir
-- como una sucesión (quizá vacía) de multiplicaciones por 3 y sumas de
-- 5 al número 1.
-- 
-- Definir las siguientes funciones
--    generables     :: [Integer]
--    generable      :: Integer -> Bool
-- tales que
-- + generables es la sucesión de los números generables. Por ejemplo,
--      ghci> take 20 generables
--      [1,3,6,8,9,11,13,14,16,18,19,21,23,24,26,27,28,29,31,32]
--      ghci> generables !! (10^6)
--      1250008
-- + (generable x) se verifica si x es generable. Por ejemplo,
--      generable 23       ==  True
--      generable 77       ==  True
--      generable 15       ==  False
--      generable 1250008  ==  True
--      generable 1250010  ==  False
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

generables :: [Integer]
generables = 1 : mezcla [3 * x | x <- generables]
                        [5 + x | x <- generables]
 
-- (mezcla xs ys) es la lista ordenada obtenida mezclando las dos listas
-- ordenadas xs e ys, suponiendo que ambas son infinitas. Por ejemplo,
--    take 10 (mezcla [2,12..] [5,15..])  ==  [2,5,12,15,22,25,32,35,42,45]
--    take 10 (mezcla [2,22..] [5,15..])  ==  [2,5,15,22,25,35,42,45,55,62]
mezcla :: Ord a => [a] -> [a] -> [a]
mezcla us@(x:xs) vs@(y:ys)
  | x < y     = x : mezcla xs vs
  | x == y    = x : mezcla xs ys
  | otherwise = y : mezcla us ys
 
generable :: Integer -> Bool
generable x =
  x == head (dropWhile (<x) generables)
 
-- 2ª definición
-- =============

generable2 :: Integer -> Bool
generable2 1 = True
generable2 x =    (x `mod` 3 == 0 && generable2 (x `div` 3))
               || (x > 5 && generable2 (x - 5))
 
generables2 :: [Integer] 
generables2 = filter generable2 [1..]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Consideramos el siguiente tipo algebraico de los árboles
--    data Arbol a = N a [Arbol a]
--                   deriving Show
-- Por ejemplo, el árbol
--                     1
--                  /  |  \
--                 4   5   2
--                / \      |
--               3   7     6
-- se define por
--    ej1:: Arbol Int
--    ej1 = N 1 [N 4 [N 3 [], N 7 []],
--               N 5 [],
--               N 2 [N 6 []]]
--
-- A partir de él, podemos construir otro árbol en el que, para cada
-- nodo de ej1, calculamos el número de nodos del subárbol que lo tiene
-- como raíz. Obtenemos el árbol siguiente:
--
--                     7
--                  /  |  \
--                 3   1   2
--                / \      |
--               1   1     1
-- 
-- Definir la función
--     arbolNN :: Arbol a -> Arbol Int
-- tal que (arbolNN x) es un árbol con la misma estructura que x, de
-- forma que en cada nodo de (arbolNN x) aparece el número de nodos del
-- subárbol correspondiente en x. Por ejemplo,
--     arbolNN ej1 == N 7 [N 3 [N 1 [],N 1 []],
--                         N 1 [],
--                         N 2 [N 1 []]]
-- ---------------------------------------------------------------------

data Arbol a = N a [Arbol a]
             deriving Show

ej1 :: Arbol Int
ej1 = N 1 [N 4 [N 3 [], N 7 []],
           N 5 [],
           N 2 [N 6 []]]

raiz :: Arbol a -> a
raiz (N r _) = r

arbolNN :: Arbol a -> Arbol Int
arbolNN (N r []) = N 1 []
arbolNN (N r as) = N (s+1) bs
  where bs = map arbolNN as
        rs = map raiz bs
        s = sum rs

-- ---------------------------------------------------------------------
-- Ejercicio 4. El procedimiento de codificación matricial se puede
-- entender siguiendo la codificación del mensaje "todoparanada" como se
-- muestra a continuación:
--     Se calcula la longitud L del mensaje. En el ejemplo es L es 12.
--     Se calcula el menor entero positivo N cuyo cuadrado es mayor o
--     igual que L. En el ejemplo N es 4.  Se extiende el mensaje con
--     N²-L asteriscos. En el ejemplo, el mensaje extendido es
--     "todoparanada****" Con el mensaje extendido se forma una matriz
--     cuadrada NxN. En el ejemplo la matriz es
--      | t o d o |
--      | p a r a |
--      | n a d a |
--      | * * * * |
-- 
--     Se rota 90º la matriz del mensaje extendido. En el ejemplo, la
--     matriz rotada es
-- 
--      | * n p t |
--      | * a a o |
--      | * d r d |
--      | * a a o |
-- 
--     Se calculan los elementos de la matriz rotada. En el ejemplo, los
--     elementos son "*npt*aap*drd*aao" El mensaje codificado se obtiene
--     eliminando los asteriscos de los elementos de la matriz
--     rotada. En el ejemplo, "nptaapdrdaao".
-- 
-- Definir la función
--    codificado :: String -> String
-- tal que (codificado cs) es el mensaje obtenido aplicando la
-- codificación matricial al mensaje cs. Por ejemplo,
--    codificado "todoparanada"    ==  "nptaaodrdaao"
--    codificado "nptaaodrdaao"    ==  "danaopadtora"
--    codificado "danaopadtora"    ==  "todoparanada"
--    codificado "entodolamedida"  ==  "dmdeaeondltiao"
-- ---------------------------------------------------------------------

codificado :: String -> String
codificado cs =
  filter (/='*') (elems (rota p))
  where n = ceiling (sqrt (genericLength cs))
        p = listArray ((1,1),(n,n)) (cs ++ repeat '*')
 
rota :: Array (Int,Int) Char -> Array (Int,Int) Char
rota p = array d [((i,j),p!(n+1-j,i)) | (i,j) <- indices p]
  where d = bounds p
        n = fst (snd d)
