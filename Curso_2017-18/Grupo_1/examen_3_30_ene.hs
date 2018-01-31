-- Informática (1º del Grado en Matemáticas, Grupos 1, 2 y 3)
-- 3º examen de evaluación continua (30 de enero de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Una secuencia de números es estrictamente oscilante
-- si el orden relativo entre términos consecutivos se va alternando. Se
-- pueden dar dos casos de secuencias estrictamente oscilantes: 
-- + El primer término es estrictamente menor que el segundo, el segundo
--   es estrictamente mayor que el tercero, el tercero es estrictamente
--   menor que el cuarto, el cuarto es estrictamente mayor que el
--   quinto, y así sucesivamente. Por ejemplo las secuencias
--      0, 1, -1, 2, -2, 3, -3, 4, -4
--      1, 10, 5, 28, 3, 12, 4
-- + El primer término es estrictamente mayor que el segundo, el segundo
--   es estrictamente menor que el tercero, el tercero es estrictamente
--   mayor que el cuarto, el cuarto es estrictamente menor que el
--   quinto, y así sucesivamente. Por ejemplo las secuencias
--      0, -1, 1, -2, 2, -3, 3, -4, 4
--      10, 5, 28, 3, 12, 4, 24
--   
-- Definir la función   
--   estrictamenteOscilante :: [Int] -> Bool
-- tal que (estrictamenteOscilante xs) se cumple si y sólo si la
-- secuencia de números enteros xs es estrictamente oscilante. Por
-- ejemplo, 
--   estrictamenteOscilante [0,1,-1,2,-2,3,-3,4,-4]  ==  True
--   estrictamenteOscilante [1,10,5,28,3,12,4]       ==  True
--   estrictamenteOscilante [0,-1,1,-2,2,-3,3,-4,4]  ==  True
--   estrictamenteOscilante [10,5,28,3,12,4,24]      ==  True
--   estrictamenteOscilante [1,1,1,1,1]              ==  False
--   estrictamenteOscilante [1,2,3,4,5,6]            ==  False
-- ----------------------------------------------------------------------------

-- 1ª solución
-- ============

estrictamenteOscilante :: [Int] -> Bool
estrictamenteOscilante xs =
  signosAlternados [x-y | (x,y) <- zip xs (tail xs)]

signosAlternados xs =
  and [x*y < 0 | (x,y) <- zip xs (tail xs)]

-- 2ª solución
-- ===========

estrictamenteOscilante2 :: [Int] -> Bool
estrictamenteOscilante2 (x:y:z:xs) =
  signum (x-y) /= signum (y-z) && estrictamenteOscilante2 (y:z:xs)
estrictamenteOscilante2 _ = True  

-- 3ª solución
-- ===========

estrictamenteOscilante3 :: [Int] -> Bool
estrictamenteOscilante3 xs = 
  all (== (-1)) (signos (*) (signos (-) xs))

signos :: (Int -> Int -> Int) -> [Int] -> [Int]
signos f xs =
  [signum (f y x) | (x,y) <- zip xs (tail xs)]

-- 4ª solución
-- ===========

estrictamenteOscilante4 :: [Int] -> Bool
estrictamenteOscilante4 = all (-1 ==) . signos (*) . signos (-)

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Cualquier número natural admite una representación en
-- base 3; es decir, se puede escribir como combinación lineal de
-- potencias de 3, con coeficientes 0, 1 ó 2. Por ejemplo,
--    46 = 1*3^0 + 0*3^1 + 2*3^2 + 1*3^3
--   111 = 0*3^0 + 1*3^1 + 0*3^2 + 1*3^3 +1*3^4 
-- Esta representación se suele expresar como la lista de los 
-- coeficientes de las potencias sucesivas de 3. Así, 46 es [1,0,2,1] y 
-- 111 es [0,1,0,1,1].
-- 
-- De esta forma, los números que no tienen el 2 en su representación en 
-- base 3 son: 
--    0 [0], 1 [1], 
--    3 [0,1], 4 [1,1], 
--    9 [0,0,1], 10 [1,0,1], 12 [0,1,1], 13 [1,1,1], ...
--
-- Definir la lista infinita
--    sin2enBase3 :: [Integer]
-- cuyo valor es la lista formada por los números naturales que no tienen
-- el 2 en su representación en base 3. Por ejemplo,
--    λ> take 22 sin2enBase3
--    [0,1,3,4,9,10,12,13,27,28,30,31,36,37,39,40,81,82,84,85,90,91]
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

sin2enBase3 :: [Integer]
sin2enBase3 = map base3Adecimal (filter (all (/=2)) enBase3)

-- enBase3 es la lista de los números enteros en base 3. Por ejemplo,
--    λ> take 10 enBase3
--    [[0],[1],[2],[0,1],[1,1],[2,1],[0,2],[1,2],[2,2],[0,0,1]]
enBase3 :: [[Integer]]
enBase3 = map decimalAbase3 [0..]

-- (decimalAbase3 n) es la representación de n en base 3. Por
-- ejemplo,
--    decimalAbase3 34  ==  [1,2,0,1]
-- ya que 1*3^0 + 2*3^1 + 0*3^2 + 1*3^3 = 34.
decimalAbase3 :: Integer -> [Integer]
decimalAbase3 n
  | n < 3     = [n]
  | otherwise = n `mod` 3 : decimalAbase3 (n `div` 3)

-- (base3Adecimal xs) es el número decimal cuya representación
-- en base 3b es xs. Por ejemplo,  
--    base3Adecimal [1,2,0,1]  ==  34
-- ya que 1*3^0 + 2*3^1 + 0*3^2 + 1*3^3 = 34.
base3Adecimal :: [Integer] -> Integer
base3Adecimal xs = sum (zipWith (\x k -> x*3^k) xs [0..])

-- Otra definición de la función anterior es
base3Adecimal2 :: [Integer] -> Integer
base3Adecimal2 = foldr (\x a -> x+3*a) 0

-- 2ª solución
-- ===========

sin2enBase3b :: [Integer]
sin2enBase3b = map base3Adecimal (filter (notElem 2) enBase3)

-- 3ª solución
-- ===========

sin2enBase3c :: [Integer]
sin2enBase3c =
  0 : aux 0 [0]
  where aux n ns = map ((3^n)+) ns ++ aux (n+1) (ns ++ map ((3^n)+) ns)

-- 4ª solución
-- ===========

sin2enBase3d :: [Integer]
sin2enBase3d =
  0 : 1 : concatMap (\n -> [3*n,3*n+1]) (tail sin2enBase3d)

-- 5ª solución
-- ===========

sin2enBase3e :: [Integer]
sin2enBase3e = 0 : aux
  where aux = 1 : concat [[3*n,3*n+1] | n <- aux]

-- 6ª solución
-- ===========

sin2enBase3f :: [Integer]
sin2enBase3f = map base3Adecimal enBase3sin2

-- enBase3sin2 es la lista de los números en base 3 que no tienen el
-- dígito 2. Por ejemplo,
--    λ> take 9 enBase3sin2
--    [[0],[1],[0,1],[1,1],[0,0,1],[1,0,1],[0,1,1],[1,1,1],[0,0,0,1]]
enBase3sin2 :: [[Integer]]
enBase3sin2 = [0] : aux
  where aux = [1] : concat [[0:xs,1:xs] | xs <- aux]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. ¿Cuál será el próximo año que no tenga 2 en su
-- representación en base 3?
-- ---------------------------------------------------------------------

-- El cálculo es
--    λ> head (dropWhile (<2018) sin2enBase3)
--    2187

-- ---------------------------------------------------------------------
-- Ejercicio 3.1 Definir la función
--    listasParciales :: [a] -> [[a]]
-- tal que (listasParciales xs) es la lista obtenida agrupando los
-- elementos de la lista infinita xs de forma que la primera tiene 0
-- elementos; la segunda, el primer elemento de xs; la tercera, los dos
-- siguientes; y así sucesivamente. Por ejemplo,
--    λ> take 6 (listasParciales [1..])
--    [[],[1],[2,3],[4,5,6],[7,8,9,10],[11,12,13,14,15]]
--    λ> take 6 (listasParciales [1,3..])
--    [[],[1],[3,5],[7,9,11],[13,15,17,19],[21,23,25,27,29]]
-- ---------------------------------------------------------------------

-- 1ª solución
listasParciales :: [a] -> [[a]]
listasParciales = aux 0
  where aux n xs = ys : aux (n+1) zs  
          where (ys,zs) = splitAt n xs

-- 2ª solución
listasParciales2 :: [a] -> [[a]]
listasParciales2 = aux 0
  where aux n xs = ys : aux (n+1) zs  
          where (ys,zs) = (take n xs, drop n xs)

-- 3ª solución
listasParciales3 :: [a] -> [[a]]
listasParciales3 xs = aux xs 0
  where aux xs n = take n xs : aux (drop n xs) (n+1)

-- ---------------------------------------------------------------------
-- Ejercicio 3.2 Definir la función
--    sumasParciales  :: [Int] -> [Int]
-- tal que (sumasParciales xs) es la lista de las sumas de las listas
-- parciales de la lista infinita xs. Por ejemplo,
--    λ> take 15 (sumasParciales [1..])
--    [0,1,5,15,34,65,111,175,260,369,505,671,870,1105,1379]
--    λ> take 15 (sumasParciales [1,3..])
--    [0,1,8,27,64,125,216,343,512,729,1000,1331,1728,2197,2744]
-- ---------------------------------------------------------------------

-- 1ª solución
sumasParciales :: [Int] -> [Int]
sumasParciales xs = map sum (listasParciales xs)

-- 2ª solución
sumasParciales2 :: [Int] -> [Int]
sumasParciales2 = map sum . listasParciales

-- ---------------------------------------------------------------------
-- Ejercicio 3.3 Comprobar con QuickChek que, para todo número natural
-- n, el término n-ésimo de (sumasParciales [1,3..]) es n^3.
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

-- La propiedad es
prop_sumasParciales :: Int -> Property
prop_sumasParciales n =
  n >= 0 ==> (sumasParciales [1,3..] !! n == n^3)

-- La comprobación es
--    λ> quickCheck prop_sumasParciales
--    +++ OK, passed 100 tests.

-- 2ª solución
-- ===========

-- La propiedad es
prop_sumasParciales2 :: Positive Int -> Bool
prop_sumasParciales2 (Positive n) =
  sumasParciales [1,3..] !! n == n^3

-- La comprobación es
--    λ> quickCheck prop_sumasParciales2
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4. Los árboles se pueden representar mediante el siguiente
-- tipo de datos  
--    data Arbol a = N a [Arbol a]
--                   deriving Show
-- Por ejemplo, los árboles
--      1               3      
--     / \             /|\     
--    2   3           / | \    
--        |          5  4  7   
--        4          |    /|\  
--                   6   2 8 1 
-- se representan por
--    ej1, ej2 :: Arbol Int
--    ej1 = N 1 [N 2 [], N 3 [N 4 []]]
--    ej2 = N 3 [N 5 [N 6 []], 
--               N 4 [], 
--               N 7 [N 2 [], N 8 [], N 1 []]]
--
-- Definir la función
--    nodosConNHijosMaximales :: Arbol a -> [a]
-- tal que (nodosConNHijosMaximales x) es la lista de los nodos del
-- árbol x que tienen una cantidad máxima de hijos. Por ejemplo,
--    nodosConNHijosMaximales ej1  ==  [1]
--    nodosConNHijosMaximales ej2  ==  [3,7]
-- ---------------------------------------------------------------------

data Arbol a = N a [Arbol a]
  deriving Show

ej1, ej2, ej3 :: Arbol Int
ej1 = N 1 [N 2 [], N 3 [N 4 []]]
ej2 = N 3 [N 5 [N 6 []], 
           N 4 [], 
           N 7 [N 2 [], N 8 [], N 1 []]]
ej3 = N 3 [N 5 [N 6 []], 
           N 4 [N 1 [N 2 [], N 8 [N 9 []]]], 
           N 12 [N 20 [], N 0 [], N 10 []]]

-- 1ª solución
-- ===========

nodosConNHijosMaximales :: Arbol a -> [a]
nodosConNHijosMaximales x =
  let nh = nHijos x
      m  = maximum (map fst nh)
  in map snd (filter (\ (l,v) -> m == l) (nHijos x))

-- (nHijos x) es la lista de los pares (k,n) donde n es un nodo del
-- árbol x y k es su número de hijos. Por ejemplo,  
--    λ> nHijos ej1
--    [(2,1),(0,2),(1,3),(0,4)]
--    λ> nHijos ej2
--    [(3,3),(1,5),(0,6),(0,4),(3,7),(0,2),(0,8),(0,1)]
nHijos :: Arbol a -> [(Int,a)]
nHijos (N r []) = [(0,r)]
nHijos (N r xs) = (length xs, r) : concatMap nHijos xs

-- 2ª solución
-- ===========

nodosConNHijosMaximales2 :: Arbol a -> [a]
nodosConNHijosMaximales2 x =
  [y | (n,y) <- nh, n == m]
  where nh = nHijos x
        m  = maximum (map fst nh)

-- 3ª solución
-- ===========

nodosConNHijosMaximales3 :: Ord a => Arbol a -> [a]
nodosConNHijosMaximales3 =
  map snd . head . groupBy eq . sortBy (flip compare) . nHijos
  where eq (a,b) (c,d) = a == c
