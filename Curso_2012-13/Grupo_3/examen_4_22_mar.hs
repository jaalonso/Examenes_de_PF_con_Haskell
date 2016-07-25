-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 4º examen de evaluación continua (22 de marzo de 2013)
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Consideremos un número n y sumemos reiteradamente sus
-- cifras hasta un número de una única cifra. Por ejemplo,
--    477 -> 18 -> 9
--    478 -> 19 -> 10 -> 1
-- El número de pasos se llama la persistencia aditiva de n y el último
-- número su raíz digital. Por ejemplo, 
--    la persistencia aditiva de 477 es 2 y su raíz digital es 9; 
--    la persistencia aditiva de 478 es 3 y su raíz digital es 1. 
-- 
-- Definir la función
--    persistenciaAditiva :: Integer -> Int
-- tal que (persistenciaAditiva n) es el número de veces que hay que
-- reiterar el proceso anterior hasta llegar a un número de una
-- cifra. Por ejemplo,
--    persistenciaAditiva 477  ==  2
--    persistenciaAditiva 478  ==  3
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

persistenciaAditiva :: Integer -> Int
persistenciaAditiva n = length (listaSumas n) -1

-- (listaSumas n) es la lista de las sumas de las cifras de los números
-- desde n hasta su raíz digital. Por ejemplo, 
--    listaSumas 477  ==  [477,18,9]
--    listaSumas 478  ==  [478,19,10,1]
listaSumas :: Integer -> [Integer]
listaSumas n | n < 10    = [n]
             | otherwise = n: listaSumas (sumaCifras n)

-- (sumaCifras) es la suma de las cifras de n. Por ejemplo,
--    sumaCifras 477  ==  18
sumaCifras :: Integer -> Integer
sumaCifras = sum . cifras

-- (cifras n) es la lista de las cifras de n. Por ejemplo,
--    cifras 477  ==  [4,7,7]
cifras:: Integer -> [Integer]
cifras n = [read [x] | x <- show n]

-- 2ª definición
-- =============

persistenciaAditiva2 :: Integer -> Int
persistenciaAditiva2 n 
    | n < 10    = 0
    | otherwise = 1 + persistenciaAditiva2 (sumaCifras n)

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función
--    raizDigital :: Integer -> Integer
-- tal que (raizDigital n) es la raíz digital de n. Por ejemplo,
--    raizDigital 477  ==  9
--    raizDigital 478  ==  1
-- ---------------------------------------------------------------------

-- 1ª definición:
raizDigital :: Integer -> Integer
raizDigital n = last (listaSumas n)

-- 2ª definición:
raizDigital2 :: Integer -> Integer
raizDigital2 n 
    | n < 10    = n
    | otherwise = raizDigital2 (sumaCifras n)

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Comprobar experimentalmente que si n/=0 es múltiplo de
-- 9, entonces la raíz digital n es 9; y en los demás casos, es el resto
-- de la división de n entre 9.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_raizDigital :: Integer -> Property
prop_raizDigital n =
    n > 0 ==>
    if n `rem` 9 == 0 then raizDigital n == 9
                      else raizDigital n == rem n 9

-- La comprobación es
--    ghci> quickCheck prop_raizDigital
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Basándose en estas propiedades, dar una nueva
-- definición de raizDigital.
-- ---------------------------------------------------------------------

raizDigital3 :: Integer -> Integer
raizDigital3 n | r /= 0    = r
               | otherwise = 9
               where r = n `rem` 9
                       
-- Puede definirse sin condicionales:
raizDigital3' :: Integer -> Integer                       
raizDigital3' n = 1 + (n-1) `rem` 9

-- ---------------------------------------------------------------------
-- Ejercicio 1.5. Comprobar con QuickCheck que las definiciones de raíz
-- digital son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_equivalencia_raizDigital :: Integer -> Property
prop_equivalencia_raizDigital n =
    n > 0 ==>
    raizDigital2  n == x && 
    raizDigital3  n == x && 
    raizDigital3' n == x
    where x = raizDigital n 

-- La comprobación es
--    ghci> quickCheck prop_equivalencia_raizDigital
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 1.6. Con las definiciones anteriores, calcular la raíz
-- digital del número 987698764521^23456 y comparar su eficiencia. 
-- ---------------------------------------------------------------------

-- ghci> :set +s
-- ghci> raizDigital (987698764521^23456)
-- 9
-- (6.55 secs, 852846660 bytes)
-- ghci> raizDigital2 (987698764521^23456)
-- 9
-- (6.42 secs, 852934412 bytes)
-- ghci> raizDigital3 (987698764521^23456)
-- 9
-- (0.10 secs, 1721860 bytes)
-- ghci> raizDigital3' (987698764521^23456)
-- 9
-- (0.10 secs, 1629752 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función 
--    interVerifican :: Eq a => (b -> Bool) -> (b -> a) -> [[b]] -> [a]        
-- tal que (interVerifican p f xss) calcula la intersección de las
-- imágenes por f de los elementos de las listas de xss que verifican p.
-- Por ejemplo,
--    interVerifican even (\x -> x+1) [[1,3,4,2], [4,8], [9,4]] == [5]
--    interVerifican even (\x -> x+1) [[1,3,4,2], [4,8], [9]]   == []
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
interVerifican :: Eq a => (b -> Bool) -> (b -> a) -> [[b]] -> [a]
interVerifican p f xss = interseccion [[f x | x <- xs, p x] | xs <- xss]

-- (interseccion xss) es la intersección de los elementos de xss. Por
-- ejemplo,  
--    interseccion [[1,3,4,2], [4,8,3], [9,3,4]]  ==  [3,4]
interseccion :: Eq a => [[a]] -> [a]
interseccion []   = []
interseccion (xs:xss) = [x | x<-xs, and [x `elem` ys| ys <-xss]]

-- 2ª definición (con map y filter):
interVerifican2 :: Eq a => (b -> Bool) -> (b -> a) -> [[b]] -> [a]
interVerifican2 p f = interseccion . map (map f . filter p)

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. La sucesión autocontadora  
--    1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, ...
-- está formada por 1 copia de1 1, 2 copias del 2, 3 copias del 3, ...
--  
-- Definir la constante
--    autocopiadora :: [Integer]
-- tal que autocopiadora es lista de los términos de la sucesión
-- anterior. Por ejemplo, 
--    take 20 autocopiadora == [1,2,2,3,3,3,4,4,4,4,5,5,5,5,5,6,6,6,6,6]        
-- ---------------------------------------------------------------------

autocopiadora :: [Integer]
autocopiadora = concat [genericReplicate n n | n <- [1..]]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función 
--    terminoAutocopiadora :: Integer -> Integer
-- tal que (terminoAutocopiadora n) es el lugar que ocupa en la sucesión
-- la primera ocurrencia de n. Por ejemplo,
--    terminoAutocopiadora 4  == 6
--    terminoAutocopiadora 5  == 10
--    terminoAutocopiadora 10 == 45        
-- ---------------------------------------------------------------------        

-- 1ª definición (por comprensión):
terminoAutocopiadora :: Integer -> Integer
terminoAutocopiadora x = 
    head [n | n <- [1..], genericIndex autocopiadora n == x]

-- 2ª definición (con takeWhile):
terminoAutocopiadora2 :: Integer -> Integer
terminoAutocopiadora2 x = genericLength (takeWhile (/=x) autocopiadora)

-- 3ª definición (por recursión)
terminoAutocopiadora3 :: Integer -> Integer
terminoAutocopiadora3 x = aux x autocopiadora 0
  where aux x (y:ys) k | x == y    = k
                       | otherwise = aux x ys (k+1)
                                     
-- 4ª definición (sumando):
terminoAutocopiadora4 :: Integer -> Integer
terminoAutocopiadora4 x = sum [1..x-1]

-- 5ª definición (explícitamente):
terminoAutocopiadora5 :: Integer -> Integer
terminoAutocopiadora5 x = (x-1)*x `div` 2

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Calcular el lugar que ocupa en la sucesión la
-- primera ocurrencia de 2013. Y también el de 20132013.
-- ---------------------------------------------------------------------

-- El cálculo es
--    terminoAutocopiadora5 2013      ==  2025078
--    terminoAutocopiadora5 20132013  ==  202648963650078

-- ---------------------------------------------------------------------
-- Ejercicio 4. Se consideran los árboles binarios definidos por 
--    data Arbol = H Int 
--               | N Arbol Int Arbol
--               deriving (Show, Eq)
-- Por ejemplo, los árboles siguientes 
--         5              8             5           5
--        / \            / \           / \         / \
--       /   \          /   \         /   \       /   \
--      9     7        9     3       9     2     4     7
--     / \   / \      / \   / \     / \               / \
--    1   4 6   8    1   4 6   2   1   4             6   2
-- se representan por
--    arbol1, arbol2, arbol3, arbol4 :: Arbol
--    arbol1 = N (N (H 1) 9 (H 4)) 5 (N (H 6) 7 (H 8))
--    arbol2 = N (N (H 1) 9 (H 4)) 8 (N (H 6) 3 (H 2))
--    arbol3 = N (N (H 1) 9 (H 4)) 5 (H 2)
--    arbol4 = N (H 4) 5 (N (H 6) 7 (H 2))
--
-- Observad que los árboles arbol1 y arbol2 tiene la misma estructura,
-- pero los árboles arbol1 y arbol3 o arbol1 y arbol4 no la tienen
--
-- Definir la función
--    igualEstructura :: Arbol -> Arbol -> Bool
-- tal que (igualEstructura a1 a1) se verifica si los árboles a1 y a2 
-- tienen la misma estructura. Por ejemplo,
--    igualEstructura arbol1 arbol2 == True
--    igualEstructura arbol1 arbol3 == False
--    igualEstructura arbol1 arbol4 == False
-- ---------------------------------------------------------------------

data Arbol = H Int 
           | N Arbol Int Arbol
           deriving (Show, Eq)

arbol1, arbol2, arbol3, arbol4 :: Arbol
arbol1 = N (N (H 1) 9 (H 4)) 5 (N (H 6) 7 (H 8))
arbol2 = N (N (H 1) 9 (H 4)) 8 (N (H 6) 3 (H 2))
arbol3 = N (N (H 1) 9 (H 4)) 5 (H 2)
arbol4 = N (H 4) 5 (N (H 6) 7 (H 2))

igualEstructura :: Arbol -> Arbol -> Bool
igualEstructura (H _) (H _)               = True
igualEstructura (N i1 r1 d1) (N i2 r2 d2) = 
    igualEstructura i1 i2 && igualEstructura d1 d2
igualEstructura _ _                       = False  

