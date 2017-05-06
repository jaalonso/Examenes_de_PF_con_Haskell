-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 2º examen de evaluación continua (29 de noviembre de 2016)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Debug.Trace

import Data.List
import Data.Numbers.Primes 

-- ---------------------------------------------------------------------
-- Ejercicio 1. [2 puntos] La persistencia multiplicativa de un número 
-- es la cantidad de pasos requeridos para reducirlo a una dígito
-- multiplicando sus dígitos. Por ejemplo, la persistencia de 39 es 3
-- porque 3*9 = 27, 2*7 = 14 y 1*4 = 4. 
--
-- Definir la función
--    persistencia     :: Integer -> Integer
-- tale que (persistencia x) es la persistencia de x. Por ejemplo,
--      persistencia 39                             ==   3
--      persistencia 2677889                        ==   8
--      persistencia 26888999                       ==   9
--      persistencia 3778888999                     ==  10
--      persistencia 277777788888899                ==  11
--      persistencia 77777733332222222222222222222  ==  11
-- ---------------------------------------------------------------------

persistencia :: Integer -> Integer
persistencia x
  | x < 10    = 0
  | otherwise = 1 + persistencia (productoDigitos x)

productoDigitos :: Integer -> Integer
productoDigitos x 
  | x < 10    = x
  | otherwise = r * productoDigitos y
  where (y,r) = quotRem x 10

-- ---------------------------------------------------------------------
-- Ejercicio 2. [2 puntos] Definir la función
--    intercala :: [a] -> [a] -> [[a]]
-- tal que (intercala xs ys) es la lista obtenida intercalando los
-- elementos de ys entre los de xs. Por ejemplo, 
--    ghci> intercala "79" "15"
--    ["719","759"]
--    ghci> intercala "79" "154"
--    ["719","759","749"]
--    ghci> intercala "796" "15"
--    ["71916","71956","75916","75956"]
--    ghci> intercala "796" "154"
--    ["71916","71956","71946",
--     "75916","75956","75946",
--     "74916","74956","74946"]
-- ---------------------------------------------------------------------

intercala :: [a] -> [a] -> [[a]]
intercala []     _  = []
intercala [x]    _  = [[x]]
intercala (x:xs) ys = [x:y:zs | y <- ys
                              , zs <- intercala xs ys] 

-- ---------------------------------------------------------------------
-- Ejercicio 2. [2 puntos] Un número primo se dice que es un primo de
-- Kamenetsky  si al anteponerlo cualquier dígito se obtiene un número
-- compuesto. Por ejemplo, el 5 es un primo de Kamenetsky ya que 15, 25,
-- 35, 45, 55, 65, 75, 85 y 95 son compuestos. También lo es 149 ya que
-- 1149, 2149, 3149, 4149, 5149, 6149, 7149, 8149 y 9149 son compuestos. 
--
-- Definir la sucesión
--    primosKamenetsky :: [Integer]
-- tal que sus elementos son los números primos de Kamenetsky. Por
-- ejemplo, 
--    take 5 primosKamenetsky  ==  [2,5,149,401,509]
-- ---------------------------------------------------------------------

primosKamenetsky :: [Integer]
primosKamenetsky =
  [x | x <- primes
     , esKamenetsky x] 

esKamenetsky :: Integer -> Bool
esKamenetsky x =
  all (not . isPrime) [read (d:xs) | d <- "123456789"]
  where xs = show x

-- ---------------------------------------------------------------------
-- Ejercicio 4. [2 puntos] Representamos los árboles binarios con
-- elementos en las hojas y en los nodos mediante el tipo de dato 
--    data Arbol a = H a
--                 | N a (Arbol a) (Arbol a) 
--                 deriving Show
-- Por ejemplo,
--    ej1 :: Arbol Int
--    ej1 = N 5 (N 2 (H 1) (H 2)) (N 3 (H 4) (H 2))
-- 
-- Definir la función
--    ramasCon :: Eq a => Arbol a -> a -> [[a]]
-- tal que (ramasCon a x) es la lista de las ramas del árbol a en las
-- que aparece el elemento x. Por ejemplo,
--   ramasCon ej1 2 ==  [[5,2,1],[5,2,2],[5,3,2]]
-- ---------------------------------------------------------------------

data Arbol a = H a
             | N a (Arbol a) (Arbol a) 
             deriving Show
 
ej1 :: Arbol Int
ej1 = N 5 (N 2 (H 1) (H 2)) (N 3 (H 4) (H 2))
 
ramasCon :: Eq a => Arbol a -> a -> [[a]]
ramasCon a x = [ys | ys <- ramas a, x `elem` ys]
 
ramas :: Arbol a -> [[a]]
ramas (H x)     = [[x]]
ramas (N x i d) = [x:ys | ys <- ramas i ++ ramas d]

-- ---------------------------------------------------------------------
-- Ejercicio 5. [2 puntos] El valor máximo de la suma de elementos
-- consecutivos de la lista [3,-4,4,1] es 5 obtenido sumando los
-- elementos del segmento [4,1] y para la lista [-2,1,4,-3,5,-7,6] es 7
-- obtenido sumando los elementos del segmento [1,4,-3,5].
-- 
-- Definir la función
--    sumaMaxima :: [Integer] -> Integer
-- tal que (sumaMaxima xs) es el valor máximo de la suma de elementos
-- consecutivos de la lista xs. Por ejemplo,
--    sumaMaxima1 [3,-4,4,1]          ==  5
--    sumaMaxima1 [-2,1,4,-3,5,-7,6]  ==  7
--    sumaMaxima []                   ==  0
--    sumaMaxima [2,-2,3,-3,4]        ==  4
--    sumaMaxima [-1,-2,-3]           ==  0
--    sumaMaxima [2,-1,3,-2,3]        ==  5
--    sumaMaxima [1,-1,3,-2,4]        ==  5
--    sumaMaxima [2,-1,3,-2,4]        ==  6
--    sumaMaxima [1..10^6]            ==  500000500000
-- ----------------------------------------------------------------------------

-- 1ª definición
-- =============

sumaMaxima1 :: [Integer] -> Integer
sumaMaxima1 [] = 0
sumaMaxima1 xs =
    maximum (0 : map sum [sublista xs i j | i <- [0..length xs - 1],
                                            j <- [i..length xs - 1]])

sublista :: [Integer] -> Int -> Int -> [Integer]
sublista xs i j =
    [xs!!k | k <- [i..j]]

-- 2ª definición
-- =============

sumaMaxima2 :: [Integer] -> Integer
sumaMaxima2 [] = 0
sumaMaxima2 xs = sumaMaximaAux 0 0 xs
    where m = maximum xs

sumaMaximaAux :: Integer -> Integer -> [Integer] -> Integer
sumaMaximaAux m v xs | trace ("aux " ++ show m ++ " " ++ show v ++ " " ++ show xs) False = undefined
sumaMaximaAux m v [] = max m v
sumaMaximaAux m v (x:xs)
    | x >= 0    = sumaMaximaAux m (v+x) xs
    | v+x > 0   = sumaMaximaAux (max m v) (v+x) xs
    | otherwise = sumaMaximaAux (max m v) 0 xs

-- 3ª definición
-- =============

sumaMaxima3 :: [Integer] -> Integer
sumaMaxima3 [] = 0
sumaMaxima3 xs = maximum (map sum (segmentos xs))

-- (segmentos xs) es la lista de los segmentos de xs. Por ejemplo 
--    segmentos "abc"  ==  ["", "a","ab","abc","b","bc","c"]
segmentos :: [a] -> [[a]]
segmentos xs =
    [] : concat [tail (inits ys) | ys <- init (tails xs)]

-- 4ª definición
-- =============

sumaMaxima4 :: [Integer] -> Integer
sumaMaxima4 [] = 0
sumaMaxima4 xs = 
    maximum (concat [scanl (+) 0 ys | ys <- tails xs])

-- Comparación de eficiencia
-- =========================

--    ghci> let n = 10^2 in sumaMaxima1 [-n..n] 
--    5050
--    (2.10 secs, 390,399,104 bytes)
--    ghci> let n = 10^2 in sumaMaxima2 [-n..n] 
--    5050
--    (0.02 secs, 0 bytes)
--    ghci> let n = 10^2 in sumaMaxima3 [-n..n] 
--    5050
--    (0.27 secs, 147,705,184 bytes)
--    ghci> let n = 10^2 in sumaMaxima4 [-n..n] 
--    5050
--    (0.04 secs, 11,582,520 bytes)

