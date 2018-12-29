-- Informática (1º del Grado en Matemáticas, Grupo 5)
-- 2º examen de evaluación continua (13 de diciembre de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. La sucesión de Hasler se define como sigue. Para cada
-- n >= 0, el término n-ésimo de la sucesión se calcula restando la suma
-- de los dígitos de n y el número de dígitos de n. Por ejemplo,
--    s(0)   = 0 - 1 = -1
--    s(10)  = 1 - 2 = -1
--    s(711) = 9 - 3 = 6 
--
-- Definir la función
--    sucHasler :: [Int]
-- tal que (sucHasler) es la lista infinita que representa dicha
-- sucesión. Por ejemplo,
--    λ> take 30 sucHasler
--    [-1,0,1,2,3,4,5,6,7,8,-1,0,1,2,3,4,5,6,7,8,0,1,2,3,4,5,6,7,8,9]
-- --------------------------------------------------------------------

-- 1ª definición
-- =============

sucHasler :: [Int]
sucHasler =
  [sum (digitos x) - length (digitos x) | x <- [0..]]

-- (digitos x) es la lista de los dígitos de x. Por ejemplo, 
--    digitos 235  ==  [2,3,5]
digitos :: Int -> [Int]
digitos x = [read [c] | c <- show x] 

-- 2ª definición
-- =============

sucHasler2 :: [Int]
sucHasler2 =
  map (\x -> sum (digitos x) - length (digitos x)) [0..]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función 
--    posicionHasler :: Int -> Int
-- tal que (posicionHasler x) es la posición (empezando a contar por
-- cero) de la primera ocurrencia del elemento x en la sucesión de
-- Hasler. Por ejemplo, 
--    posicionHasler 3    == 4
--    posicionHasler (-2) == 100
--    posicionHasler (-3) == 1000
-- ---------------------------------------------------------------------

-- 1ª definición
posicionHasler :: Int -> Int
posicionHasler x = head [b | (a,b) <- zip sucHasler [0..], a == x] 

-- 2ª definición
posicionHasler2 :: Int -> Int
posicionHasler2 x = length (takeWhile (/= x) sucHasler)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    separa :: (a -> Bool) -> [a] -> ([a],[a])
-- tal que (separa p xs) es el par cuya primera componente son los
-- elementos de xs que cumplen la propiedad p y la segunda es la de los
-- que no la cumplen. Por ejemplo,
--    separa even [1,2,5,4,7] == ([2,4],[1,5,7])
--    separa (>3) [1..7]      == ([4,5,6,7],[1,2,3])
-- ----------------------------------------------------------

-- 1ª definición
separa :: (a -> Bool) -> [a] -> ([a],[a])
separa = partition

-- 2ª definición
separa2 :: (a -> Bool) -> [a] -> ([a],[a])
separa2 _ []                 = ([],[])
separa2 p (x:xs) | p x       = (x:ys,zs)
                 | otherwise = (ys,x:zs)
  where (ys,zs) = separa2 p xs

-- 3ª definición
separa3 :: (a -> Bool) -> [a] -> ([a],[a])
separa3 p xs = (filter p xs, filter (not . p) xs)

-- 4ª definición
separa4 :: (a -> Bool) -> [a] -> ([a],[a])
separa4 p = foldr (aux p) ([],[]) 
  where aux p x (ys,zs) | p x       = (x:ys,zs)
                        | otherwise = (ys,x:zs)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Una lista de listas xss se dirá encadenada si
-- + todos sus elementos son listas de dos o más elementos, y
-- + los dos últimos elementos de cada lista de xss coinciden con los
--   dos primeros elementos de la siguiente.
-- 
-- Definir la función
--    encadenada :: Ord a => [[a]] -> Bool
-- tal que (encadenadaC xss) se verifica si xss es encadenada. Por
-- ejemplo, 
--    encadenada [[1,2,3],[2,3,1,0],[1,0,1],[0,1,7]] == True
--    encadenada [[1,2,3],[2,3,3,3],[3,5,6]]         == False
--    encadenada [[1,2,3],[2,3,5,7],[7]]             == False
-- ---------------------------------------------------------------------

-- 1ª definición
encadenada :: Eq a => [[a]] -> Bool
encadenada xss =
  and [length xs >= 2 | xs <- xss] &&
  and [drop (length xs - 2) xs == take 2 ys
      | (xs,ys) <- zip xss (tail xss)]

-- 2ª definición
encadenada2 :: Eq a => [[a]] -> Bool
encadenada2 []   = True
encadenada2 [xs] = length xs >= 2
encadenada2 (xs:ys:zss) =
  length xs >= 2 &&
  length ys >= 2 &&
  drop (length xs - 2) xs == take 2 ys &&
  encadenada2 (ys:zss)
  
-- ---------------------------------------------------------------------
-- Ejercicio 4. Una cadena de texto se dirá normalizada si:
-- + se han eliminado los espacios  iniciales,
-- + se han eliminado los espacios finales, y
-- + cada palabra está separada de la siguiente por un único espacio.
-- 
-- Definir la función
--    normaliza :: String -> String
-- tal que (normaliza xs) devuelve es la normalización de la cadena
-- xs. Por ejemplo,  
--    normaliza "   Hoy   es   jueves   " == "Hoy es jueves"
-- ------------------------------------------------------------------------

-- 1ª definición
-- =============

normaliza :: String -> String
normaliza = unwords . words

-- 2ª definición
-- =============

normaliza2 :: String -> String
normaliza2 = frase . palabras

-- (palabras xs) es la lista de las palabras de cs. Por ejemplo,
--    λ> palabras "   Hoy   es   jueves   "
--    ["Hoy","es","jueves"]
palabras :: String -> [String]
palabras [] = []
palabras cs
  | null cs'  = []
  | otherwise = p : palabras cs''
  where cs'      = dropWhile (== ' ') cs
        (p,cs'') = break (== ' ') cs'

-- (frase ps) es la frase formada por las palabras de ps. Por ejemplo,
--    λ> frase ["Hoy","es","jueves"]
--    "Hoy es jueves"
frase :: [String] -> String
frase []     = ""
frase (p:ps) = p ++ aux ps
  where aux []       = ""
        aux (p':ps') = ' ' : (p' ++ aux ps')

