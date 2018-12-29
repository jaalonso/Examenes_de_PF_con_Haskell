-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 2º examen de evaluación continua (14 de diciembre de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías                                                        --
-- ---------------------------------------------------------------------

import Data.List
import Data.Numbers.Primes

-- ---------------------------------------------------------------------
-- Ejercicio 1. Representamos una matriz como una lista de listas, en la
-- que cada lista es una fila de la matriz. Por ejemplo, las matrices
--    ( 1, 2, 3, 4 )      ( 1, 2, 3, 4, 5 )
--    ( 0, 9, 2, 5 )      ( 0, 9, 2, 5, 6 )
--    ( 7, 8,-1, 3 )      ( 7, 8,-1, 3, 7 )
--    ( 4, 7, 1, 0 )      ( 4, 7, 1, 0, 8 )
--                        ( 0, 1, 0, 2, 0 )
-- las representamos mediante
--    m1 = [[1, 2, 3, 4],
--          [0, 9, 2, 5],
--          [7, 8,-1, 3],
--          [4, 7, 1, 0]]
--      
--    m2 = [[1,2,3,4,5],
--          [0,9,2,5,6],
--          [7,8,-1,3,7],
--          [4,7,1,0,8],
--          [0,1,0,2,0]]
-- 
-- Definir la función
--    columnas :: [[a]] -> [[a]]
-- tal que (columnas m) es la lista de las columnas de la matriz. Por
-- ejemplo, 
--    λ> columnas m1
--    [[1,0,7,4],[2,9,8,7],[3,2,-1,1],[4,5,3,0]]
--    λ> columnas m2
--    [[1,0,7,4,0],[2,9,8,7,1],[3,2,-1,1,0],[4,5,3,0,2],[5,6,7,8,0]]
-- ---------------------------------------------------------------------

m1, m2 :: [[Int]]
m1 = [[1, 2, 3, 4],
      [0, 9, 2, 5],
      [7, 8,-1, 3],
      [4, 7, 1, 0]]
m2 = [[1,2,3,4,5],
      [0,9,2,5,6],
      [7,8,-1,3,7],
      [4,7,1,0,8],
      [0,1,0,2,0]]

-- 1ª solución
columnas :: [[a]] -> [[a]]
columnas []     = []
columnas ([]:_) = []
columnas xss    = map head xss : columnas (map tail xss)

-- 2ª solución
columnas2 :: [[a]] -> [[a]]
columnas2 = transpose

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir las funciones
--    esPrimoSumaDeDosPrimos :: Integer -> Bool
--    primosSumaDeDosPrimos :: [Integer]
-- tales que
-- + (esPrimoSumaDeDosPrimos x) se verifica si x es un número primo que
--   se puede escribir como la suma de dos números primos. Por ejemplo,
--      esPrimoSumaDeDosPrimos 19        ==  True
--      esPrimoSumaDeDosPrimos 20        ==  False
--      esPrimoSumaDeDosPrimos 23        ==  False
--      esPrimoSumaDeDosPrimos 18409541  ==  False
-- + primosSumaDeDosPrimos es la lista de los números primos que se
--   pueden escribir como la suma de dos números primos. Por ejemplo, 
--      take 7 primosSumaDeDosPrimos    == [5,7,13,19,31,43,61]
--      primosSumaDeDosPrimos !! (10^4) == 1261081
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

esPrimoSumaDeDosPrimos :: Integer -> Bool
esPrimoSumaDeDosPrimos x =
  (x == last ps) && not (null [p | p <- ps, isPrime (x-p)])
  where ps = takeWhile (<=x) primes

primosSumaDeDosPrimos :: [Integer]
primosSumaDeDosPrimos = filter esPrimoSumaDeDosPrimos primes

-- 2ª solución
-- ===========

-- Teniendo en cuenta que si se suman dos primos ambos distintos de 2
-- nunca puede ser primo (porque sería par).

esPrimoSumaDeDosPrimos2 :: Integer -> Bool
esPrimoSumaDeDosPrimos2 x = isPrime x && isPrime (x - 2)
 
primosSumaDeDosPrimos2 :: [Integer]
primosSumaDeDosPrimos2 = [x | x <- primes, isPrime (x - 2)]
 
-- 3ª solución
-- ===========
 
primosSumaDeDosPrimos3 :: [Integer]
primosSumaDeDosPrimos3 =
  [y | (x,y) <- zip primes (tail primes), y == x + 2]
 
esPrimoSumaDeDosPrimos3 :: Integer -> Bool
esPrimoSumaDeDosPrimos3 x = 
  x == head (dropWhile (<x) primosSumaDeDosPrimos3)

-- Comparación de eficiencia
-- =========================

--    λ> primosSumaDeDosPrimos2 !! 300
--    17293
--    (0.10 secs, 30,061,168 bytes)
--    λ> primosSumaDeDosPrimos3 !! 300
--    17293
--    (0.06 secs, 10,797,448 bytes)
--    
--    λ> primosSumaDeDosPrimos2 !! (5*10^3)
--    557731
--    (2.46 secs, 1,180,627,296 bytes)
--    λ> primosSumaDeDosPrimos3 !! (5*10^3)
--    557731
--    (0.99 secs, 362,595,072 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Los árboles binarios se puede representar mediante el
-- siguiente tipo
--    data Arbol a = H a 
--                 | N a (Arbol a) (Arbol a)
--      deriving (Show, Eq)
-- 
-- Definir la función
--    nodosNivelesImpar :: Arbol a -> [a]
-- tal que (nodosNivelesImpar ar) es la lista de los nodos de nivel
-- impar de ar. Por ejemplo, si 
--   ejA = N 10 (N (-2) (N 8 (H 0) (H 1))
--                      (H (-4)))
--              (N 6 (H 7) (H 5))
-- entonces  
--   nodosNivelesImpar ejA == [-2,6,0,1]
-- ---------------------------------------------------------------------

data Arbol a = H a 
             | N a (Arbol a) (Arbol a)
  deriving (Show, Eq)

nivel :: Int -> Arbol a -> [a]
nivel 0 (H x)      = [x]
nivel 0 (N x _  _) = [x]
nivel k (H _ )     = []
nivel k (N _ i d)  = nivel (k-1) i ++ nivel (k-1) d

nodosNivelesImpar :: Arbol a -> [a]
nodosNivelesImpar ar =
  concat $ takeWhile (not . null) [nivel k ar | k <-[1,3..]]

-- ---------------------------------------------------------------------
-- Ejercicio 4. La sucesión de Loomis generada por un número entero
-- positivo x es la sucesión cuyos términos se definen por
-- + sucL(0) es x
-- + sucL(n) es la suma de sucL(n-1) y el producto de los dígitos no
--   nulos de sucL(n-1) 
-- 
-- Los primeros términos de las primeras sucesiones de Loomis son
-- + Generada por 1: 1, 2, 4, 8, 16, 22, 26, 38, 62, 74, 102, 104, 108, 
-- + Generada por 2: 2, 4, 8, 16, 22, 26, 38, 62, 74, 102, 104, 108, 
-- + Generada por 3: 3, 6, 12, 14, 18, 26, 38, 62, 74, 102, 104, 108, 
-- + Generada por 4: 4, 8, 16, 22, 26, 38, 62, 74, 102, 104, 108, 116, 
-- + Generada por 5: 5, 10, 11, 12, 14, 18, 26, 38, 62, 74, 102, 104, 
-- 
-- Definir la función
--    sucL :: Integer -> [Integer]
-- tal que (sucL x) es la sucesión de Loomis generada por x. Por
-- ejemplo, 
--    λ> take 15 (sucL 1)
--    [1,2,4,8,16,22,26,38,62,74,102,104,108,116,122]
--    λ> take 15 (sucL 2)
--    [2,4,8,16,22,26,38,62,74,102,104,108,116,122,126]
--    λ> take 15 (sucL 3)
--    [3,6,12,14,18,26,38,62,74,102,104,108,116,122,126]
--    λ> take 15 (sucL 20)
--    [20,22,26,38,62,74,102,104,108,116,122,126,138,162,174]
--    λ> take 15 (sucL 100)
--    [100,101,102,104,108,116,122,126,138,162,174,202,206,218,234]
--    λ> sucL 1 !! (2*10^5)
--    235180736652
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============
 
sucL :: Integer -> [Integer]
sucL x = map (loomis x) [0..]

-- (loomis x n) es el n-ésimo término de la sucesión de Loomis generada
-- por x. Por ejemplo, 
--    loomis 2 3  ==  16
--    loomis 3 2  ==  12
loomis :: Integer -> Integer -> Integer
loomis x 0 = x
loomis x n = y + productoDigitosNoNulos y
  where y = loomis x (n-1)

-- (productoDigitosNoNulos n) es el producto de los dígitos no nulos de
-- n. Por ejemplo, 
--    productoDigitosNoNulos 3005040  ==  60
productoDigitosNoNulos :: Integer -> Integer
productoDigitosNoNulos = product . digitosNoNulos

-- (digitosNoNulos x) es la lista de los dígitos no nulos de x. Por
-- ejemplo,   
--    digitosNoNulos 3005040  ==  [3,5,4]
digitosNoNulos :: Integer -> [Integer]
digitosNoNulos x = [read [c] | c <- show x, c /= '0']
 
-- 2ª definición
-- =============
 
sucL2 :: Integer -> [Integer]
sucL2 = iterate siguienteL
  where siguienteL y = y + productoDigitosNoNulos y
 
-- 3ª definición
-- =============
 
sucL3 :: Integer -> [Integer]
sucL3 = iterate (\y -> y + productoDigitosNoNulos y)
 
-- 4ª definición
-- =============
 
sucL4 :: Integer -> [Integer]
sucL4 = iterate ((+) <*> productoDigitosNoNulos)
