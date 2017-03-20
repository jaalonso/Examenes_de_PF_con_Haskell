-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 4º examen de evaluación continua (13 de marzo de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
--   Librerías auxiliares                                            --
-- ---------------------------------------------------------------------

import Data.List
import Data.Numbers.Primes 
import Data.Char
import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Un número primo se dice que es doble primo si se le
-- puede anteponer otro primo con igual número de dígitos obteniéndose 
-- un número primo. Por ejemplo, el 3 es dobleprimo ya que 23,53,73 son
-- primos. También lo es 19 ya que, por ejemplo, 1319 es primo. 
--
-- Definir la función
--    prefijoPrimo :: Integer -> [Integer]  
-- tal que (prefijoPrimo x) es la lista de los primos de igual longitud
-- que x tales que al anteponerlos a x se obtiene un primo. Por ejemplo,
--    prefijoPrimo  3 = [2,5,7]
--    prefijoPrimo 19 = [13,31,37,67,79,97] 
--    prefijoPrimo  2 = []
-- ---------------------------------------------------------------------

prefijoPrimo :: Integer -> [Integer]  
prefijoPrimo x =
  [y | y <- [10^(n-1)..10^n]
     , isPrime y
     , isPrime (pega y x)]
  where n = numDigitos x 

pega :: Integer -> Integer -> Integer
pega a b = read (show a ++ show b)

numDigitos :: Integer -> Int
numDigitos y = length (show y)

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la sucesión
--    doblesprimos :: [Integer]
-- tal que sus elementos son los dobles primos. Por ejemplo, 
--    take 15 doblesprimos == [3,7,11,13,17,19,23,29,31,37,41,43,47,53,59]
--    doblesprimos !! 500  == 3593
-- ---------------------------------------------------------------------

doblesPrimos :: [Integer]
doblesPrimos = [x | x <- primes, esDoblePrimo x]

esDoblePrimo :: Integer -> Bool
esDoblePrimo x = isPrime x && not (null (prefijoPrimo x))

-- ---------------------------------------------------------------------
-- Ejercicio 2. Representamos los árboles binarios con
-- elementos en las hojas y en los nodos mediante el tipo de dato 
--    data Arbol a = H a
--                 | N a (Arbol a) (Arbol a) 
--      deriving Show
-- Por ejemplo,
--    ej1 :: Arbol Integer
--    ej1 = N 5 (N 2 (H 1) (H 2)) (N 3 (H 4) (H 2))
-- 
-- Definir la función
--    numeros :: Arbol Integer-> Array Integer String -> Arbol String
-- tal que (numeros a v) es el árbol obtenido al sustituir los números
-- del árbol a por su valor en el vector v. Por ejemplo, para
--    v1 = listArray (0,10)
--                   ["cero","uno","dos","tres","cuatro","cinco","seis",
--                    "siete","ocho","nueve", " diez"]
--    v2 = listArray (0,20)
--                   [if even x then "PAR" else "IMPAR" | x <- [0..19]]
-- tenemos:
--    numeros ej1 v1 = N "cinco" 
--                       (N "dos" 
--                          (H "uno") 
--                          (H "dos")) 
--                       (N "tres" 
--                          (H "cuatro") 
--                          (H "dos"))
--    numeros ej1 v2 = N "IMPAR" 
--                       (N "PAR" (H "IMPAR") (H "PAR")) 
--                       (N "IMPAR" (H "PAR") (H "PAR"))
-- ---------------------------------------------------------------------

data Arbol a = H a
             | N a (Arbol a) (Arbol a) 
  deriving Show

ej1 :: Arbol Integer
ej1 = N 5 (N 2 (H 1) (H 2)) (N 3 (H 4) (H 2))

v1 = listArray (0,10)
               ["cero","uno","dos","tres","cuatro","cinco","seis",
                "siete","ocho","nueve", " diez"]
v2 = listArray (0,20)
               [if even x then "PAR" else "IMPAR" | x <- [0..19]]

numeros :: Arbol Integer -> Array Integer String -> Arbol String
numeros (H x) v     = H (v!x)
numeros (N x i d) v = N (v!x) (numeros i v) (numeros d v)

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Observemos la siguiente secuencia de matrices:
-- 
-- (1 2) pos (1,1) (9 2) pos (1,2) (9 16) pos (2,1) (9 16) pos(2,2)(10 16)
-- (3 4)  ->       (3 4)   ->      (3  4)  ->       (29 4)    ->   (29 54)
--   m0              m1              m2               m3             m4
--
-- Una matriz en el paso n se obtiene cambiando en la matriz del
-- paso anterior el elemento n-ésimo por la suma de sus vecinos. 
-- La idea es
--    m0 = m
--    m1 = es m  pero en (1,1) tiene la suma de sus vecinos en m, 
--    m2 = es m1 pero en (1,2) tiene la suma de sus vecinos en m1,
--    m3 = es m2 pero en (2,1) tiene la suma de sus vecinos en m2,
--    m4 = es m3 pero en (2,2) tiene la suma de sus vecinos en m3.
--
-- Definir la función 
--    sumaVecinos :: (Integer,Integer) -> 
--                   Array (Integer, Integer) Integer -> 
--                   Integer
-- tal que (sumaVecinos x p) es la suma de los vecinos del elemento de p
-- que está en la posición x. En el ejemplo,
--    sumaVecinos (2,1) m2 == 29 
-- ---------------------------------------------------------------------

sumaVecinos :: (Int,Int) -> Array (Int, Int) Int -> Int
sumaVecinos (i,j) p =
  sum [p!(i+a,j+b) | a <- [-1..1]
                   , b <- [-1..1]
                   , a /= 0 || b /= 0
                   , inRange (bounds p) (i+a,j+b)]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    transformada :: Array (Integer, Integer) Integer 
--                    -> Int 
--                    -> Array (Integer, Integer) Integer
-- tal que (transformada p n) es la matriz que se obtiene en el paso n a
-- partir de la matriz inicial p. Es decir, en el ejemplo de arriba,  
--    transformada m 0 = m0
--    transformada m 1 = m1
-- ---------------------------------------------------------------------

transformada :: Array (Int, Int) Int -> Int -> Array (Int, Int) Int
transformada p 0 = p
transformada p n =
  transformada p (n-1)
  // [(indices p !! (n-1),
       sumaVecinos (indices p !! (n-1)) (transformada p (n-1)))]

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir la función
--    secuencia :: Array (Integer, Integer) Integer 
--                 -> [Array (Integer, Integer) Integer]          
-- tal que (secuencia p) es la lista que contiene todas las matrices que
-- se obtienen a partir de la matriz inicial p. En el ejemplo,
-- (secuencia m) será la lista [m0, m1, m2, m3, m4]
--    ghci> map elems (secuencia (listArray ((1,1),(2,2))[1,2,3,4]))
--    [[1,2,3,4],[9,2,3,4],[9,16,3,4],[9,16,29,4],[9,16,29,54]]
-- ---------------------------------------------------------------------

secuencia :: Array (Int, Int) Int -> [Array (Int, Int) Int]
secuencia p = [transformada p n | n <- [0 .. rangeSize (bounds p)]]
