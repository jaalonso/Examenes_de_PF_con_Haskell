-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 1º examen de evaluación continua (4 de diciembre de 2009)
-- ---------------------------------------------------------------------
 
import Test.QuickCheck
import Data.Char
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir, por composición, la función 
--    insertaEnposicion :: a -> Int -> [a] -> [a]
-- tal que (insertaEnposicion x n xs) es la lista obtenida insertando x
-- en xs en la posición n. Por ejemplo,
--    insertaEnposicion 80 4 [1,2,3,4,5,6,7,8] == [1,2,3,80,4,5,6,7,8]
--    insertaEnposicion 'a' 1 "hola"             == "ahola"
-- ---------------------------------------------------------------------

insertaEnposicion :: a -> Int -> [a] -> [a]
insertaEnposicion x n xs = take (n-1) xs ++ [x] ++ drop (n-1) xs

-- ---------------------------------------------------------------------
-- Ejercicio 2. El algoritmo de Euclides para calcular el máximo común
-- divisor de dos números naturales a y b es el siguiente: 
--    Si b = 0, entonces mcd(a,b) = a
--    Si b > 0, entonces mcd(a,b) = mcd(b,c), donde c es el resto de 
--              dividir a entre b
-- 
-- Definir la función 
--    mcd :: Int -> Int -> Int
-- tal que (mcd a b) es el máximo común divisor de a y b calculado
-- usando el algoritmo de Euclides. Por ejemplo,
--    mcd 2 3      ==  1
--    mcd 12 30    ==  6
--    mcd 700 300  ==  100
-- ---------------------------------------------------------------------

mcd :: Int -> Int -> Int
mcd a 0 = a
mcd a b = mcd b (a `mod` b)

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función 
--    esSubconjunto :: Eq a => [a] -> [a] -> Bool
-- tal que (esSubconjunto xs ys) se verifica si todos los elementos de
-- xs son también elementos de ys. Por ejemplo,
--    esSubconjunto [3,5,2,1,1,1,6,3] [1,2,3,5,6,7]   == True
--    esSubconjunto [3,2,1,1,1,6,3] [1,2,3,5,6,7]     == True
--    esSubconjunto [3,2,1,8,1,6,3] [1,2,3,5,6,7]     == False
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión):
esSubconjunto :: Eq a => [a] -> [a] -> Bool
esSubconjunto [] ys     = True
esSubconjunto (x:xs) ys = x `elem` ys && esSubconjunto xs ys 

-- 2ª definición (por comprensión):
esSubconjunto2 :: Eq a => [a] -> [a] -> Bool
esSubconjunto2 xs ys = and [x `elem` ys | x <- xs]

-- 3ª definición (por plegado):
esSubconjunto3 :: Eq a => [a] -> [a] -> Bool
esSubconjunto3 xs ys = foldr (\ x -> (&&) (x `elem` ys)) True xs

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    igualConjunto :: Eq a => [a] -> [a] -> Bool
-- tal que (igualConjunto xs ys) se verifica si xs e ys son iguales como
-- conjuntos. Por ejemplo,
--    igualConjunto [3,2,1,8,1,6,3] [1,2,3,5,6,7]     == False
--    igualConjunto [3,2,1,1,1,6,3] [1,2,3,5,6,7]     == False
--    igualConjunto [3,2,1,1,1,6,3] [1,2,3,5,6]       == False
--    igualConjunto [3,2,1,1,1,6,3,5] [1,2,3,5,6]     == True
-- ---------------------------------------------------------------------

igualConjunto :: Eq a => [a] -> [a] -> Bool
igualConjunto xs ys = esSubconjunto xs ys && esSubconjunto ys xs

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir por comprensión la función 
--    repiteC :: Int -> [a] -> [a]
-- tal que (repiteC n xs) es la lista que resulta de repetir cada
-- elemento de xs n veces. Por ejemplo,
--    repiteC 5 "Hola" == "HHHHHooooolllllaaaaa"
-- ---------------------------------------------------------------------

repiteC :: Int -> [a] -> [a]
repiteC n xs = [x | x <- xs, i <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir por recursión la función 
--    repiteR :: Int -> [a] -> [a]
-- tal que (repiteR n xs) es la lista que resulta de repetir cada
-- elemento de xs n veces. Por ejemplo,
--    repiteR 5 "Hola" == "HHHHHooooolllllaaaaa"
-- ---------------------------------------------------------------------

repiteR :: Int -> [a] -> [a]
repiteR _ []     = []
repiteR n (x:xs) = replicate n x ++ repiteR n xs 
