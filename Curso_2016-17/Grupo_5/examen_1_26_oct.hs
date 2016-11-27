-- Informática (1º del Grado en Matemáticas, Grupo 5)
-- 1º examen de evaluación continua (26 de octubre de 2016)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. La sombra de un número x es el que se obtiene borrando
-- las cifras de x que ocupan lugares impares. Por ejemplo, la sombra de
-- 123 es 13 ya que borrando el 2, que ocupa la posición 1, se obtiene
-- el 13.
-- 
-- Definir la función
--    sombra :: Int -> Int
-- tal que (sombra x) es la sombra de x. Por ejemplo,
--    sombra 4736  ==  43
--    sombra 473   ==  43
--    sombra 47    ==  4
--    sombra 4     ==  4
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión)
-- ===============================

sombra :: Int -> Int
sombra n = read [x | (x,n) <- zip (show n) [0..], even n]

-- 2ª definición (por recursión)
-- =============================

sombra2 :: Int -> Int
sombra2 n = read (elementosEnPares (show n))

-- (elementosEnPares xs) es la lita de los elementos de xs en posiciones
-- pares. Por ejemplo,
--    elementosEnPares [4,7,3,6]  ==  [4,3]
--    elementosEnPares [4,7,3]    ==  [4,3]
--    elementosEnPares [4,7]      ==  [4]
--    elementosEnPares [4]        ==  [4]
--    elementosEnPares []         ==  []
elementosEnPares :: [a] -> [a]
elementosEnPares (x:y:zs) = x : elementosEnPares zs
elementosEnPares xs       = xs

-- 3ª definición (por recursión y composición)
-- ===========================================

sombra3 :: Int -> Int
sombra3 = read . elementosEnPares . show

prop_sombra :: Int -> Property
prop_sombra x =
  x >= 0 ==> 
  nDigitos (sombra x) == ceiling (fromIntegral (nDigitos x) / 2)

nDigitos n = length (show n)

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función
--    esSombra :: Int -> Int -> Bool
-- tal que (esSombra x y) se verifica si y es sombra de x. Por ejemplo,
--    esSombra 72941 791  ==  True
--    esSombra 72941 741  ==  False
-- ---------------------------------------------------------------------

esSombra :: Int -> Int -> Bool
esSombra x y = sombra x == y

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la función
--    conSombra :: Int -> [Int]
-- tal que (conSombra x n) es la lista de números con el menor número
-- posible de cifras cuya sombra es x. Por ejemplo,
--    ghci> conSombra 2
--    [2]
--    ghci> conSombra 23
--    [203,213,223,233,243,253,263,273,283,293]
--    ghci> conSombra 234
--    [20304,20314,20324,20334,20344,20354,20364,20374,20384,20394,
--     21304,21314,21324,21334,21344,21354,21364,21374,21384,21394,
--     22304,22314,22324,22334,22344,22354,22364,22374,22384,22394,
--     23304,23314,23324,23334,23344,23354,23364,23374,23384,23394,
--     24304,24314,24324,24334,24344,24354,24364,24374,24384,24394,
--     25304,25314,25324,25334,25344,25354,25364,25374,25384,25394,
--     26304,26314,26324,26334,26344,26354,26364,26374,26384,26394,
--     27304,27314,27324,27334,27344,27354,27364,27374,27384,27394,
--     28304,28314,28324,28334,28344,28354,28364,28374,28384,28394,
--     29304,29314,29324,29334,29344,29354,29364,29374,29384,29394]
-- ---------------------------------------------------------------------

conSombra :: Int -> [Int]
conSombra x =
  [read ys | ys <- intercala (show x) ['0'..'9']]

-- (intercala xs ys) es la lista obtenida intercalando los elementos de
-- xs entre los de ys. Por ejemplo, 
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
intercala :: [a] -> [a] -> [[a]]
intercala []     _  = []
intercala [x]    _  = [[x]]
intercala (x:xs) ys = [x:y:zs | y <- ys
                              , zs <- intercala xs ys] 
 
-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    mezcla :: [a] -> [a] -> [a]
-- tal que (mezcla xs ys) es la lista obtenida alternando cada uno de
-- los elementos de xs con los de ys. Por ejemplo, 
--    mezcla [1,2,3] [4,7,3,2,4]        == [1,4,2,7,3,3,2,4]
--    mezcla [4,7,3,2,4] [1,2,3]        == [4,1,7,2,3,3,2,4]
--    mezcla [1,2,3] [4,7]              == [1,4,2,7,3]
--    mezcla "E er eSnRqe" "lprod a ou" == "El perro de San Roque"
--    mezcla "et" "so sobra"            == "esto sobra"
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión)
mezcla :: [a] -> [a] -> [a]
mezcla xs ys =
  concat [[x,y] | (x,y) <- zip xs ys] 
  ++ drop (length xs) ys 
  ++ drop (length ys) xs

-- 2ª definición (por recursión)
mezcla2 :: [a] -> [a] -> [a]
mezcla2 [] ys = ys
mezcla2 xs [] = xs
mezcla2 (x:xs) (y:ys) = x : y : mezcla2 xs ys

-- ---------------------------------------------------------------------
-- Ejercicio 3. Un elemento de una lista es una cresta si es mayor que
-- todos los anteriores a ese elemento en la lista.
-- 
-- Definir la función 
--    crestas :: Ord a => [a] -> [a]
-- tal que (crestas xs) es la lista de las crestas de xs. Por
-- ejemplo, 
--    crestas [80,1,7,8,4]  ==  [80]
--    crestas [1,7,8,4]     ==  [1,7,8]
--    crestas [3,2,6,1,5,9] ==  [3,6,9]
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

crestas :: Ord a => [a] -> [a]
crestas xs = [x | (x,n) <- zip xs [1 ..]
                , esMayor x (take (n-1) xs)] 

esMayor :: Ord a => a -> [a] -> Bool
esMayor x xs = and [x > y | y <- xs]

-- 2ª definición
-- =============

crestas2 :: Ord a => [a] -> [a]
crestas2 xs = [x | (x,ys) <- zip xs (inits xs)
                 , esMayor x ys]
             
-- 3ª definición 
-- =============

crestas3 :: Ord a => [a] -> [a]
crestas3 xs = aux xs []
  where aux [] _ = []
        aux (x:xs) ys | esMayor x ys = x : aux xs (x:ys)
                      | otherwise    = aux xs (x:ys)
             
-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    posicion :: (Float,Float) -> Float -> String
-- tal que (posicion p z) es la posición del punto p respecto del
-- cuadrado de lado l y centro (0,0). Por ejemplo,
--    posicion (-1, 1) 6 == "Interior"
--    posicion (-1, 2) 6 == "Interior"
--    posicion ( 0,-3) 6 == "Borde"
--    posicion ( 3,-3) 6 == "Borde"
--    posicion ( 3, 1) 6 == "Borde"
--    posicion (-1, 7) 6 == "Exterior"
--    posicion (-1, 4) 6 == "Exterior"
-- ---------------------------------------------------------------------

posicion :: (Float,Float) -> Float -> String
posicion (x,y) z 
    | abs x < z/2 && abs y < z/2 = "Interior"
    | abs x > z/2 || abs y > z/2 = "Exterior"
    | otherwise                  = "Borde"



