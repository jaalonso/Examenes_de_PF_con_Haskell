-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 4º examen de evaluación continua (15 de marzo de 2019)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Maybe
import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1. El problema del número perdido consiste en, dada una
-- lista de números consecutivos (creciente o decreciente) en la que
-- puede faltar algún número, hacer lo siguiente: 
-- + si falta un único número z, devolver Just z
-- + si no falta ninguno, devolver Nothing
--
-- Definir la función 
--    numeroPerdido :: [Int] -> Maybe Int
-- tal que (numeroPerdido xs) es la solución del problema del número
-- perdido en la lista xs. Por ejemplo,
--    numeroPerdido [7,6,4,3]                     == Just 5
--    numeroPerdido [1,2,4,5,6]                   == Just 3
--    numeroPerdido [6,5..3]                      == Nothing
--    numeroPerdido [1..6]                        == Nothing
--    numeroPerdido ([5..10^6] ++ [10^6+2..10^7]) == Just 1000001
-- ---------------------------------------------------------------------

-- 1ª solución
numeroPerdido :: [Int] -> Maybe Int
numeroPerdido (x:y:xs)
  | abs (y - x) == 1 = numeroPerdido (y:xs)
  | otherwise        = Just (div (x+y) 2)
numeroPerdido _      = Nothing

-- 2ª solución
numeroPerdido2 :: [Int] -> Maybe Int
numeroPerdido2 xs = aux z (z:zs) 
  where (z:zs) = sort xs
        aux _ [] = Nothing
        aux y (x:xs) | y == x    = aux (y+1) xs
                     | otherwise = Just y

-- 3ª solución
-- ===========

numeroPerdido3 :: [Int] -> Maybe Int
numeroPerdido3 xs =
  listToMaybe [(a+b) `div` 2 | (a:b:_) <- tails xs, abs(a-b) /= 1]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Los árboles se pueden representar mediante el siguiente
-- tipo de dato  
--    data Arbol a = N a [Arbol a]
--      deriving Show
-- Por ejemplo, los árboles
--       -1           1            1
--       / \         / \          /|\
--      2   3      -2   3        / | \  
--     / \          |          -2  7  3  
--    4   5        -4          / \      
--                            4   5     
-- se representan por
--    ej1, ej2, ej3 :: Arbol Int
--    ej1 = N (-1) [N 2 [N 4 [], N 5 []], N 3 []]
--    ej2 = N 1 [N (-2) [N (-4) []], N 3 []]
--    ej3 = N 1 [N (-2) [N 4 [], N 5 []], N 7 [], N 3 []]
--
-- Definir la función
--    todasDesdeAlguno :: (a -> Bool) -> Arbol a -> Bool
-- tal que (todasDesdeAlguno p ar) se verifica si para toda rama existe un
-- elemento a partir del cual todos los elementos de la rama verifican
-- la propiedad p. Por ejemplo,
--    todasDesdeAlguno (>0) ej1 == True
--    todasDesdeAlguno (>0) ej2 == False
--    todasDesdeAlguno (>0) ej3 == True
-- ---------------------------------------------------------------------

data Arbol a = N a [Arbol a]
  deriving Show

ej1, ej2, ej3 :: Arbol Int
ej1 = N (-1) [N 2 [N 4 [], N 5 []], N 3 []]
ej2 = N 1 [N (-2) [N (-4) []], N 3 []]
ej3 = N 1 [N (-2) [N 4 [], N 5 []], N 7 [], N 3 []]

-- 1ª solución
-- ===========

todasDesdeAlguno :: (b -> Bool) -> Arbol b -> Bool
todasDesdeAlguno p a = all (desdeAlguno p) (ramas a)

-- (desdeAlguno p xs) se verifica si la propiedad xs tiene un elementemo
-- a partir del cual todos los siguientes cumplen la propiedad p. Por
-- ejemplo, 
--    desdeAlguno (>0) [-1,2,4]   ==  True
--    desdeAlguno (>0) [1,-2,-4]  ==  False
--    desdeAlguno (>0) [1,-2,4]   ==  True

-- 1ª definición de desdeAlguno
desdeAlguno1 :: (a -> Bool) -> [a] -> Bool
desdeAlguno1 p xs =
  not (null (takeWhile p (reverse xs)))

-- 2ª definición de desdeAlguno
desdeAlguno2 :: (a -> Bool) -> [a] -> Bool
desdeAlguno2 p xs = any (all p) (init (tails xs))

-- Comparación de eficiencia:
--    λ> desdeAlguno1 (>10^7) [1..1+10^7]
--    True
--    (4.36 secs, 960,101,896 bytes)
--    λ> desdeAlguno2 (>10^7) [1..1+10^7]
--    True
--    (5.62 secs, 3,600,101,424 bytes)

-- Usaremos la 1ª definición de desdeAlguno
desdeAlguno :: (a -> Bool) -> [a] -> Bool
desdeAlguno = desdeAlguno1

-- (ramas a) es la lista de las ramas de a. Por ejemplo,
--    ramas ej1  ==  [[-1,2,4],[-1,2,5],[-1,3]]
--    ramas ej2  ==  [[1,-2,-4],[1,3]]
--    ramas ej3  ==  [[1,-2,4],[1,-2,5],[1,7],[1,3]]
ramas :: Arbol a -> [[a]]
ramas (N x []) = [[x]]
ramas (N x as) = map (x:) (concatMap ramas as)

-- 2ª solución
-- ===========

todasDesdeAlguno2 :: (b -> Bool) -> Arbol b -> Bool
todasDesdeAlguno2 p (N x []) = p x
todasDesdeAlguno2 p (N _ as) = all (todasDesdeAlguno2 p) as 

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. El fichero sucPrimos.txt http://bit.ly/2J49Qjl
-- contiene una sucesión de números primos escritos uno a continuación
-- de otro. Por ejemplo, 
--    λ> xs <- readFile "sucPrimos.txt"
--    λ> take 50 xs
--    "23571113171923293137414347535961677173798389971011"
--    λ> take 60 xs
--    "235711131719232931374143475359616771737983899710110310710911"
-- 
-- Definir la función
--    posicion :: String -> IO (Maybe Int)
-- tal que (posicion n) es (Just k) si k es la posición de n en la
-- sucesión almacenada en el fichero sucPrimos.txt y Nothing si n no
-- ocurre en dicha sucesión. Por ejemplo,
--    posicion 1959  == Just 5740
--    posicion 19590 == Nothing
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

posicion :: Int -> IO (Maybe Int)
posicion n = do
  ds <- readFile "sucPrimos.txt"
  return (posicionEnLista ds (show n))
 
posicionEnLista :: Eq a => [a] -> [a] -> Maybe Int
posicionEnLista xs ys = aux xs 0
  where aux [] _ = Nothing
        aux (x:xs) n | ys `isPrefixOf` (x:xs) = Just n
                     | otherwise              = aux xs (n+1)

-- 2ª definición
-- =============

posicion2 :: Int -> IO (Maybe Int)
posicion2 n = do
  ds <- readFile "sucPrimos.txt"
  return (findIndex (show n `isPrefixOf`) (tails ds))

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    posicionInteractivo ::  IO ()
-- tal que solicite que se introduzca por teclado un número sin ceros y
-- escriba como respuesta la posición de dicho número. Por ejemplo,
--    λ> posicionInteractivo
--    Escribe un número sin ceros: 
--    1959
--    La posición es: 5740
--    λ> posicionInteractivo
--    Escribe un número sin ceros: 
--    12345
--    No aparece
-- ---------------------------------------------------------------------

posicionInteractivo ::  IO ()
posicionInteractivo = do
  putStrLn "Escribe un número sin ceros: "
  ds <- readFile "sucPrimos.txt"
  c <- getLine
  let n = read c :: Int
  p <- posicion n
  if isJust p then putStrLn ("La posición es: " ++ show (fromJust p))
              else putStrLn "No aparece"

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. El triángulo de Euler se construye a partir de las
-- siguientes relaciones
--    A(n,1) = A(n,n) = 1
--    A(n,m) = (n-m)A(n-1,m-1) + (m+1)A(n-1,m).
-- Sus primeros términos son
--    1 
--    1 1                                                       
--    1 4   1                                            
--    1 11  11    1                                    
--    1 26  66    26    1                             
--    1 57  302   302   57     1                    
--    1 120 1191  2416  1191   120   1            
--    1 247 4293  15619 15619  4293  247   1   
--    1 502 14608 88234 156190 88234 14608 502 1 
-- 
-- Definir las siguientes funciones:
--   numeroEuler        :: Integer -> Integer -> Integer
--   filaTrianguloEuler :: Integer -> [Integer]
--   trianguloEuler     :: [[Integer]]
-- tales que
-- + (numeroEuler n k) es el número de Euler A(n,k). Por ejemplo, 
--      numeroEuler 8 3  == 15619
--      numeroEuler 20 6 == 21598596303099900
--      length (show (numeroEuler 1000 500)) == 2567
-- + (filaTrianguloEuler n) es la n-ésima fila del triángulo de
--   Euler. Por ejemplo,
--      filaTrianguloEuler 7  ==  [1,120,1191,2416,1191,120,1]
--      filaTrianguloEuler 8  ==  [1,247,4293,15619,15619,4293,247,1]
--      length (show (maximum (filaTrianguloEuler 1000)))  ==  2567
-- + trianguloEuler es la lista con las filas del triángulo de Euler
--      λ> take 6 trianguloEuler
--      [[1],[1,1],[1,4,1],[1,11,11,1],[1,26,66,26,1],[1,57,302,302,57,1]]
--      λ> length (show (maximum (trianguloEuler !! 999)))
--      2567
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

trianguloEuler :: [[Integer]]
trianguloEuler = iterate siguiente [1]

-- (siguiente xs) es la fila siguiente a la xs en el triángulo de
-- Euler. Por ejemplo,
--    λ> siguiente [1]
--    [1,1]
--    λ> siguiente it
--    [1,4,1]
--    λ> siguiente it
--    [1,11,11,1]
siguiente :: [Integer] -> [Integer]
siguiente xs = zipWith (+) us vs
  where n = genericLength xs
        us = zipWith (*) (0:xs) [n+1,n..1]
        vs = zipWith (*) (xs++[0]) [1..n+1]

filaTrianguloEuler :: Integer -> [Integer]
filaTrianguloEuler n = trianguloEuler `genericIndex` (n-1)

numeroEuler :: Integer -> Integer -> Integer
numeroEuler n k = filaTrianguloEuler n `genericIndex` k

-- 2ª solución
-- ===========

numeroEuler2 :: Integer -> Integer -> Integer
numeroEuler2 n 0 = 1
numeroEuler2 n m 
  | n == m    = 0
  | otherwise = (n-m) * numeroEuler2 (n-1) (m-1) + (m+1) * numeroEuler2 (n-1) m

filaTrianguloEuler2 :: Integer -> [Integer]
filaTrianguloEuler2 n = map (numeroEuler2 n) [0..n-1]

trianguloEuler2 :: [[Integer]]
trianguloEuler2 = map filaTrianguloEuler2 [1..]
                  
-- 3ª solución
-- ===========

numeroEuler3 :: Integer -> Integer -> Integer
numeroEuler3 n k = matrizEuler n k ! (n,k)

-- (matrizEuler n m) es la matriz de n+1 filas y m+1 columnsa formada
-- por los números de Euler. Por ejemplo,
--   λ> [[matrizEuler 6 6 ! (i,j) | j <- [0..i-1]] | i <- [1..6]]
--   [[1],[1,1],[1,4,1],[1,11,11,1],[1,26,66,26,1],[1,57,302,302,57,1]]
matrizEuler :: Integer -> Integer -> Array (Integer,Integer) Integer
matrizEuler n m = q
  where q = array ((0,0),(n,m)) [((i,j), f i j) | i <- [0..n], j <- [0..m]]
        f i 0 = 1
        f i j
          | i == j    = 0
          | otherwise = (i-j) * q!(i-1,j-1) + (j+1)* q!(i-1,j)

filaTrianguloEuler3 :: Integer -> [Integer]
filaTrianguloEuler3 n = map (numeroEuler3 n) [0..n-1]

trianguloEuler3 :: [[Integer]]
trianguloEuler3 = map filaTrianguloEuler3 [1..]

-- Comparación de eficiencia
-- =========================

-- La comparación es
--   λ> numeroEuler 22 11
--   301958232385734088196
--   (0.01 secs, 118,760 bytes)
--   λ> numeroEuler2 22 11
--   301958232385734088196
--   (3.96 secs, 524,955,384 bytes)
--   λ> numeroEuler3 22 11
--   301958232385734088196
--   (0.01 secs, 356,296 bytes)
--   
--   λ> length (show (numeroEuler 800 400))
--   1976
--   (0.01 secs, 383,080 bytes)
--   λ> length (show (numeroEuler3 800 400))
--   1976
--   (2.13 secs, 508,780,696 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 4.2 Definir la función
--    propFactorial:: Integer -> Bool
-- para expresar que la suma de la n-ésima fila del triángulo de Eules es
-- el factorial de n. Y calcular (propFactorial 50).
-- ---------------------------------------------------------------------

propFactorial :: Integer -> Bool
propFactorial n = 
  and [sum (filaTrianguloEuler m) == product [1..m] | m <- [1..n]]

-- El cálculo es
--    λ> propFactorial 50
--    True

-- ---------------------------------------------------------------------
-- Ejercicio 5. El valor de a^b módulo c es el resto de la división
-- entera de a^b entre c. 
-- 
-- Por ejemplo, si a = 179, b = 12, c = 13, una forma de calcular la
-- exponenciación modular sería directamente: (179^12) `mod` 13, con
-- resultado 1.  Ahora bien, el cálculo directo no es eficiente para
-- valores grandes, por ejemplo cuando a = 4, b = 411728896 y
-- c = 1000000000. 
--
-- Un algoritmo más eficiente para calcular la exponenciación modular de
-- a^b módulo c es el siguiente:
-- + Se comienza con x(0) = 1, y(0) = a y z(0) = b.
-- + Hasta que z(n) sea 0 se realiza lo siguiente:
--   + Si z(n) es par,   entonces x(n+1) = resto de x(n) entre c
--                                y(n+1) = resto de y(n)^2 entre c
--                                z(n+1) = z(n)/2
--   + Si z(n) es impar, entonces x(n+1) = resto de (x(n) * y(n)) entre c
--                                y(n+1) = y(n)
--                                z(n+1) = z(n) - 1
-- + Finalmente, el resultado es el valor de x(n).
--
-- El cálculo de 179^12 (mod 13) mediante el algoritmo anterior es:
--    +---+-----+-----+
--    | x |  y  |  z  |
--    +---+-----+-----+
--    | 1 | 179 | 12  |
--    | 1 |   9 |  6  |
--    | 1 |   3 |  3  |
--    | 3 |   3 |  2  |
--    | 3 |   9 |  1  |
--    | 1 |   9 |  0  |
--    +---+-----+-----+
-- 
-- Definir la función 
--    expModulo :: Integer -> Integer -> Integer -> Integer
-- tal que (expModulo a b c) sea el exponente modular de a^b, módulo c,
-- con el algoritmo anterior. Por ejemplo,
--    expModulo 179 12 13              == 1
--    expModulo 179 13 10              == 9
--    expModulo 13789 722341 2345      == 2029
--    expModulo 4 411728896 1000000000 == 411728896
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

expModulo :: Integer -> Integer -> Integer -> Integer
expModulo a b c = aux 1 a b
  where
    aux x y z | z == 0    = x
              | even z    = aux (rem x c) (rem (y^2) c) (z `div` 2)
              | otherwise = aux (rem (x*y) c) y (z-1)
                                
-- 2ª definición
-- =============

expModulo2 :: Integer -> Integer -> Integer -> Integer
expModulo2 a b c = x1
  where
    (x1,y1,z1) = until p f (1,a,b)
    p (x,y,z) = z == 0
    f (x,y,z) | even z    = (rem x c, rem (y^2) c, z `div` 2)
              | otherwise = (rem (x*y) c, y, z-1)

-- 3ª definición
-- =============

expModulo3 :: Integer -> Integer -> Integer -> Integer
expModulo3 a b c = x1
  where
    (x1,y1,z1) = head (dropWhile p (iterate f (1,a,b)))
    p (x,y,z) = z /= 0
    f (x,y,z) | even z    = (rem x c, rem (y^2) c, z `div` 2)
              | otherwise = (rem (x*y) c, y, z-1)

-- Comparación de eficiencia
-- =========================

--    λ> let a = 10000^10000 in expModulo a a a 
--    0
--    (1.12 secs, 1,580,023,160 bytes)
--    λ> let a = 10000^10000 in expModulo2 a a a 
--    0
--    (1.28 secs, 1,604,449,976 bytes)
--    λ> let a = 10000^10000 in expModulo3 a a a 
--    0
--    (1.18 secs, 1,614,503,344 bytes)


