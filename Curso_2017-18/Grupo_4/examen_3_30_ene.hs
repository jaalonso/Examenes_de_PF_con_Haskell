-- Informática (1º del Grado en Matemáticas, Grupos 4 y 5)
-- 3º examen de evaluación continua (30 de enero de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Char 
import Data.List 

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    alternadas :: String -> (String,String)
-- tal que (alternadas cs) es el par de cadenas (xs,ys) donde xs es la
-- cadena obtenida escribiendo alternativamente en mayúscula o minúscula
-- las letras de la palabra cs (que se supone que es una cadena de
-- letras minúsculas) e ys se obtiene análogamente pero empezando en
-- minúscula. Por ejemplo,
--    λ> alternadas "salamandra"
--    ("SaLaMaNdRa","sAlAmAnDrA")
--    λ> alternadas "solosequenosenada"
--    ("SoLoSeQuEnOsEnAdA","sOlOsEqUeNoSeNaDa")
--    λ> alternadas (replicate 30 'a')
--    ("AaAaAaAaAaAaAaAaAaAaAaAaAaAaAa","aAaAaAaAaAaAaAaAaAaAaAaAaAaAaA")
-- ---------------------------------------------------------------------

-- 1ª solución
alternadas :: String -> (String,String)
alternadas []     = ([],[])
alternadas (x:xs) = (toUpper x : zs, x : ys)
  where (ys,zs) = alternadas xs

-- 2ª solución
alternadas2 :: String -> (String,String)
alternadas2 xs =
  ( [f x | (f,x) <- zip (cycle [toUpper,id]) xs]
  , [f x | (f,x) <- zip (cycle [id,toUpper]) xs]
  )
 
-- 3ª solución
alternadas3 :: String -> (String,String)
alternadas3 xs =
  ( zipWith ($) (cycle [toUpper,id]) xs
  , zipWith ($) (cycle [id,toUpper]) xs
  )
  
-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función 
--    biparticiones :: Integer -> [(Integer,Integer)]
-- tal que (biparticiones n) es la lista de pares de números formados
-- por las primeras cifras de n y las restantes. Por ejemplo, 
--    biparticiones  2025  ==  [(202,5),(20,25),(2,25)]
--    biparticiones 10000  ==  [(1000,0),(100,0),(10,0),(1,0)]
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

biparticiones :: Integer -> [(Integer,Integer)]
biparticiones x = [(read y, read z) | (y,z) <- biparticionesL1 xs]
  where xs = show x

-- (biparticionesL1 xs) es la lista de los pares formados por los
-- prefijos no vacío de xs y su resto. Por ejemplo,
--    biparticionesL1 "2025" == [("2","025"),("20","25"),("202","5")]
biparticionesL1 :: [a] -> [([a],[a])]
biparticionesL1 xs = [splitAt k xs | k <- [1..length xs - 1]]

-- 2ª solución
-- ===========

biparticiones2 :: Integer -> [(Integer,Integer)]
biparticiones2 x = [(read y, read z) | (y,z) <- biparticionesL2 xs]
  where xs = show x

-- (biparticionesL2 xs) es la lista de los pares formados por los
-- prefijos no vacío de xs y su resto. Por ejemplo,
--    biparticionesL2 "2025" == [("2","025"),("20","25"),("202","5")]
biparticionesL2 :: [a] -> [([a],[a])]
biparticionesL2 xs =
  takeWhile (not . null . snd) [splitAt n xs | n <- [1..]]

-- 3ª solución
-- ===========

biparticiones3 :: Integer -> [(Integer,Integer)]
biparticiones3 a =
  takeWhile ((>0) . fst) [divMod a (10^n) | n <- [1..]] 

-- 4ª solución
-- ===========

biparticiones4 :: Integer -> [(Integer,Integer)]
biparticiones4 n =
  [quotRem n (10^x) | x <- [1..length (show n) -1]]

-- 5ª solución
-- ===========

biparticiones5 :: Integer -> [(Integer,Integer)]
biparticiones5 n =
  takeWhile (/= (0,n)) [divMod n (10^x) | x <- [1..]]

-- Comparación de eficiencia
-- =========================

--    λ> numero n = (read (replicate n '2')) :: Integer
--    (0.00 secs, 0 bytes)
--    λ> length (biparticiones (numero 10000))
--    9999
--    (0.03 secs, 10,753,192 bytes)
--    λ> length (biparticiones2 (numero 10000))
--    9999
--    (1.89 secs, 6,410,513,136 bytes)
--    λ> length (biparticiones3 (numero 10000))
--    9999
--    (0.54 secs, 152,777,680 bytes)
--    λ> length (biparticiones4 (numero 10000))
--    9999
--    (0.01 secs, 7,382,816 bytes)
--    λ> length (biparticiones5 (numero 10000))
--    9999
--    (2.11 secs, 152,131,136 bytes)
--    
--    λ> length (biparticiones1 (numero (10^7)))
--    9999999
--    (14.23 secs, 10,401,100,848 bytes)
--    λ> length (biparticiones4 (numero (10^7)))
--    9999999
--    (11.43 secs, 7,361,097,856 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Un número x es construible a partir de a y b si se
-- puede escribir como una suma cuyos sumandos son a o b (donde se
-- supone que a y b son números enteros mayores que 0). Por ejemplo, 7 y
-- 9 son construibles a partir de 2 y 3 ya que 7 = 2+2+3 y 9 = 3+3+3.
--
-- Definir la función
--    construibles  :: Integer -> Integer -> [Integer]
-- tal que (construibles a b) es la lista de los números construibles a
-- partir de a y b. Por ejemplo,
--    take 5 (construibles 2 9)  ==  [2,4,6,8,9]
--    take 5 (construibles 6 4)  ==  [4,6,8,10,12]
--    take 5 (construibles 9 7)  ==  [7,9,14,16,18]
-- ---------------------------------------------------------------------

-- 1ª definición
construibles :: Integer -> Integer -> [Integer]
construibles a b = tail aux
  where aux = 0 : mezcla [a + x | x <- aux]
                         [b + x | x <- aux]

mezcla :: [Integer] -> [Integer] -> [Integer]
mezcla p@(x:xs) q@(y:ys) | x < y     = x : mezcla xs q
                         | x > y     = y : mezcla p  ys  
                         | otherwise = x : mezcla xs ys
mezcla []       ys                   = ys
mezcla xs       []                   = xs
              
-- 2ª definición
construibles2 :: Integer -> Integer -> [Integer]
construibles2 a b = filter (esConstruible2 a b) [1..]

-- Comparación de eficiencia
--    λ> construibles 2 9 !! 2000
--    2005
--    (0.02 secs, 1,133,464 bytes)
--    λ> construibles2 2 9 !! 2000
--    2005
--    (3.70 secs, 639,138,544 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    esConstruible :: Integer -> Integer -> Integer -> Bool
-- tal que (esConstruible a b x) se verifica si x es construible a
-- partir de a y b. Por ejemplo,
--    esConstruible 2 3 7   ==  True
--    esConstruible 9 7 15  ==  False
-- ---------------------------------------------------------------------

-- 1ª definición
esConstruible :: Integer -> Integer -> Integer -> Bool
esConstruible a b x = x == y
  where (y:_) = dropWhile (<x) (construibles a b)

-- 2ª definición
esConstruible2 :: Integer -> Integer -> Integer -> Bool
esConstruible2 a b = aux 
  where aux x
          | x < a && x < b  = False
          | otherwise       = x == a || x == b || aux (x-a) || aux (x-b)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Los árboles binarios con valores en las hojas y en los
-- nodos se definen por  
--    data Arbol a = H a
--                 | N a (Arbol a) (Arbol a) 
--      deriving (Eq, Show)
-- 
-- Por ejemplo, el árbol
--          10
--         /  \
--        /    \
--       8      2
--      / \    / \
--     3   5  2   0
-- se pueden representar por
--    ejArbol :: Arbol Int
--    ejArbol = N 10 (N 8 (H 3) (H 5))
--                   (N 2 (H 2) (H 0))
--
-- Un árbol cumple la propiedad de la suma si el valor de cada nodo es
-- igual a la suma de los valores de sus hijos. Por ejemplo, el árbol
-- anterior cumple la propiedad de la suma.
--
-- Definir la función
--    propSuma :: Arbol Int -> Bool
-- tal que (propSuma a) se verifica si el árbol a cumple la propiedad de
-- la suma. Por ejemplo,
--    λ> propSuma (N 10 (N 8 (H 3) (H 5)) (N 2 (H 2) (H 0)))
--    True
--    λ> propSuma (N 10 (N 8 (H 4) (H 5)) (N 2 (H 2) (H 0)))
--    False
--    λ> propSuma (N 10 (N 8 (H 3) (H 5)) (N 2 (H 2) (H 1)))
--    False
-- ---------------------------------------------------------------------

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
  deriving Show

ejArbol :: Arbol Int
ejArbol = N 10 (N 8 (H 3) (H 5))
               (N 2 (H 2) (H 0))

propSuma :: Arbol Int -> Bool
propSuma (H _)     = True
propSuma (N x i d) = x == raiz i + raiz d && propSuma i && propSuma d
             
raiz :: Arbol Int -> Int
raiz (H x)     = x
raiz (N x _ _) = x
