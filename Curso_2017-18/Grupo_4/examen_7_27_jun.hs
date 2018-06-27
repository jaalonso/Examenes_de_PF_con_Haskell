-- Informática (1º del Grado en Matemáticas)
-- Examen de la 1ª convocatoria (27 de junio de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Array
import Data.List
import Data.Matrix 
import Data.Numbers.Primes
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Un camino es una sucesión de pasos en una de las cuatros
-- direcciones Norte, Sur, Este, Oeste. Ir en una dirección y a
-- continuación en la opuesta es un esfuerzo que se puede reducir, Por
-- ejemplo, el camino [Norte,Sur,Este,Sur] se puede reducir a
-- [Este,Sur]. 
--
-- Un camino se dice que es reducido si no tiene dos pasos consecutivos
-- en direcciones opuesta. 
--
-- En Haskell, las direcciones y los caminos se pueden definir por
--    data Direccion = N | S | E | O deriving (Show, Eq)
--    type Camino = [Direccion]
--
-- Definir la función 
--    reducido :: Camino -> Camino
-- tal que (reducido ds) es el camino reducido equivalente al camino
-- ds. Por ejemplo,
--    reducido []                              ==  []
--    reducido [N]                             ==  [N]
--    reducido [N,O]                           ==  [N,O]
--    reducido [N,O,E]                         ==  [N]
--    reducido [N,O,E,S]                       ==  [] 
--    reducido [N,O,S,E]                       ==  [N,O,S,E]
--    reducido [S,S,S,N,N,N]                   ==  []
--    reducido [N,S,S,E,O,N]                   ==  []
--    reducido [N,S,S,E,O,N,O]                 ==  [O]
--    reducido (take (10^7) (cycle [N,E,O,S])) ==  []
--
-- Nótese que en el penúltimo ejemplo las reducciones son
--        [N,S,S,E,O,N,O]  
--    --> [S,E,O,N,O]  
--    --> [S,N,O]  
--    --> [O]  
--
-- ---------------------------------------------------------------------

data Direccion = N | S | E | O deriving (Show, Eq)

type Camino = [Direccion]

-- 1ª solución (por recursión)
-- ===========================

reducido :: Camino -> Camino
reducido [] = []
reducido (d:ds) | null ds'                = [d]
                | d == opuesta (head ds') = tail ds'
                | otherwise               = d:ds'
    where ds' = reducido ds

opuesta :: Direccion -> Direccion
opuesta N = S
opuesta S = N
opuesta E = O
opuesta O = E

-- 2ª solución (por plegado)
-- =========================

reducido2 :: Camino -> Camino
reducido2 = foldr aux []
  where aux N (S:xs) = xs
        aux S (N:xs) = xs
        aux E (O:xs) = xs
        aux O (E:xs) = xs
        aux x xs     = x:xs

-- 3ª solución
-- ===========

reducido3 :: Camino -> Camino
reducido3 []       = []
reducido3 (N:S:ds) = reducido3 ds
reducido3 (S:N:ds) = reducido3 ds
reducido3 (E:O:ds) = reducido3 ds
reducido3 (O:E:ds) = reducido3 ds
reducido3 (d:ds) | null ds'                = [d]
                 | d == opuesta (head ds') = tail ds'
                 | otherwise               = d:ds'
    where ds' = reducido3 ds

-- 4ª solución
-- ===========

reducido4 :: Camino -> Camino
reducido4 ds = reverse (aux ([],ds)) where 
  aux (N:xs, S:ys) = aux (xs,ys)
  aux (S:xs, N:ys) = aux (xs,ys)
  aux (E:xs, O:ys) = aux (xs,ys)
  aux (O:xs, E:ys) = aux (xs,ys)
  aux (  xs, y:ys) = aux (y:xs,ys)
  aux (  xs,   []) = xs

-- Comparación de eficiencia
-- =========================

--    λ> reducido (take (10^6) (cycle [N,E,O,S]))
--    []
--    (3.87 secs, 460160736 bytes)
--    λ> reducido2 (take (10^6) (cycle [N,E,O,S]))
--    []
--    (1.16 secs, 216582880 bytes)
--    λ> reducido3 (take (10^6) (cycle [N,E,O,S]))
--    []
--    (0.58 secs, 98561872 bytes)
--    λ> reducido4 (take (10^6) (cycle [N,E,O,S]))
--    []
--    (0.64 secs, 176154640 bytes)
--    
--    λ> reducido3 (take (10^7) (cycle [N,E,O,S]))
--    []
--    (5.43 secs, 962694784 bytes)
--    λ> reducido4 (take (10^7) (cycle [N,E,O,S]))
--    []
--    (9.29 secs, 1722601528 bytes)
-- 
--    λ> length $ reducido3 (take 2000000 $ cycle [N,O,N,S,E,N,S,O,S,S])
--    400002
--    (4.52 secs, 547004960 bytes)
--    λ> length $ reducido4 (take 2000000 $ cycle [N,O,N,S,E,N,S,O,S,S])
--    400002
--    
--    λ> let n=10^6 in reducido (replicate n N ++ replicate n S)
--    []
--    (7.35 secs, 537797096 bytes)
--    λ> let n=10^6 in reducido2 (replicate n N ++ replicate n S)
--    []
--    (2.30 secs, 244553404 bytes)
--    λ> let n=10^6 in reducido3 (replicate n N ++ replicate n S)
--    []
--    (8.08 secs, 545043608 bytes)
--    λ> let n=10^6 in reducido4 (replicate n N ++ replicate n S)
--    []
--    (1.96 secs, 205552240 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Un capicúa es un número que es igual leído de izquierda
-- a derecha que de derecha a izquierda. 
-- 
-- Definir la función
--    mayorCapicuaP :: Integer -> Integer
-- tal que (mayorCapicuaP n) es el mayor capicúa que es el producto de
-- dos números de n cifras. Por ejemplo,  
--    mayorCapicuaP 2  ==  9009
--    mayorCapicuaP 3  ==  906609
--    mayorCapicuaP 4  ==  99000099
--    mayorCapicuaP 5  ==  9966006699
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

mayorCapicuaP :: Integer -> Integer
mayorCapicuaP n = head (capicuasP n)

-- (capicuasP n) es la lista de las capicúas de 2*n cifras que
-- pueden escribirse como productos de dos números de n cifras. Por
-- ejemplo, Por ejemplo,
--    λ> capicuasP 2
--    [9009,8448,8118,8008,7227,7007,6776,6336,6006,5775,5445,5335,
--     5225,5115,5005,4884,4774,4664,4554,4224,4004,3773,3663,3003,
--     2992,2772,2552,2442,2332,2112,2002,1881,1771,1551,1221,1001]
capicuasP n = [x | x <- capicuas n,
                   not (null (productosDosNumerosCifras n x))]

-- (capicuas n) es la lista de las capicúas de 2*n cifras de mayor a
-- menor. Por ejemplo, 
--    capicuas 1           ==  [99,88,77,66,55,44,33,22,11]
--    take 7 (capicuas 2)  ==  [9999,9889,9779,9669,9559,9449,9339]
capicuas :: Integer -> [Integer]
capicuas n = [capicua x | x <- numerosCifras n]

-- (numerosCifras n) es la lista de los números de n cifras de mayor a
-- menor. Por ejemplo,
--    numerosCifras 1           ==  [9,8,7,6,5,4,3,2,1]
--    take 7 (numerosCifras 2)  ==  [99,98,97,96,95,94,93]
--    take 7 (numerosCifras 3)  ==  [999,998,997,996,995,994,993]
numerosCifras :: Integer -> [Integer]
numerosCifras n = [a,a-1..b]
  where a = 10^n-1
        b = 10^(n-1) 

-- (capicua n) es la capicúa formada añadiendo el inverso de n a
--  continuación de n. Por ejemplo,
--    capicua 93  ==  9339
capicua :: Integer -> Integer
capicua n = read (xs ++ reverse xs)
  where xs = show n

-- (productosDosNumerosCifras n x) es la lista de los números y de n
-- cifras tales que existe un z de n cifras y x es el producto de y por
-- z. Por ejemplo, 
--    productosDosNumerosCifras 2 9009  ==  [99,91]
productosDosNumerosCifras n x = [y | y <- numeros,
                                     mod x y == 0,
                                     div x y `elem` numeros]
  where numeros = numerosCifras n

-- 2ª solución
-- ===========

mayorCapicuaP2 :: Integer -> Integer
mayorCapicuaP2 n = maximum [x*y | x <- [a,a-1..b],
                                  y <- [a,a-1..b],
                                  esCapicua (x*y)] 
  where a = 10^n-1
        b = 10^(n-1)

-- (esCapicua x) se verifica si x es capicúa. Por ejemplo,
--    esCapicua 353  ==  True
--    esCapicua 357  ==  False
esCapicua :: Integer -> Bool
esCapicua n = xs == reverse xs
  where xs = show n

-- 3ª solución
-- ===========

mayorCapicuaP3 :: Integer -> Integer
mayorCapicuaP3 n = maximum [x*y | (x,y) <- pares a b, 
                                  esCapicua (x*y)] 
  where a = 10^n-1
        b = 10^(n-1)

-- (pares a b) es la lista de los pares de números entre a y b de forma
-- que su suma es decreciente. Por ejemplo,
--    pares 9 7  ==  [(9,9),(8,9),(8,8),(7,9),(7,8),(7,7)]
pares a b = [(x,z-x) | z <- [a1,a1-1..b1],
                       x <- [a,a-1..b],
                       x <= z-x, z-x <= a]
  where a1 = 2*a
        b1 = 2*b

-- 4ª solución
-- ===========

mayorCapicuaP4 :: Integer -> Integer
mayorCapicuaP4 n = maximum [x | y <- [a..b],
                                z <- [y..b],
                                let x = y * z,
                                let s = show x,
                                s == reverse s]
  where a = 10^(n-1)
        b = 10^n-1
     
-- 5ª solución
-- ===========

mayorCapicuaP5 :: Integer -> Integer
mayorCapicuaP5 n = maximum [x*y | (x,y) <- pares2 b a, esCapicua (x*y)]
  where a = 10^(n-1)
        b = 10^n-1
                       
-- (pares2 a b) es la lista de los pares de números entre a y b de forma
-- que su suma es decreciente. Por ejemplo,
--    pares2 9 7  ==  [(9,9),(8,9),(8,8),(7,9),(7,8),(7,7)]
pares2 a b = [(x,y) | x <- [a,a-1..b], y <- [a,a-1..x]]

-- 6ª solución
-- ===========

mayorCapicuaP6 :: Integer -> Integer
mayorCapicuaP6 n = maximum [x*y | x <- [a..b], 
                                  y <- [x..b] , 
                                  esCapicua (x*y)]
  where a = 10^(n-1)
        b = 10^n-1
 
-- (cifras n) es la lista de las cifras de n en orden inverso. Por
-- ejemplo,  
--    cifras 325  == [5,2,3]
cifras :: Integer -> [Integer]
cifras n 
  | n < 10    = [n]
  | otherwise = ultima n : cifras (quitarUltima n)

-- (ultima n) es la última cifra de n. Por ejemplo,
--    ultima 325  ==  5
ultima  :: Integer -> Integer
ultima n =  n - (n `div` 10)*10

-- (quitarUltima n) es el número obtenido al quitarle a n su última
-- cifra. Por ejemplo,
--    quitarUltima 325  =>  32 
quitarUltima :: Integer -> Integer
quitarUltima n = (n - ultima n) `div` 10

-- 7ª solución
-- ===========

mayorCapicuaP7 :: Integer -> Integer
mayorCapicuaP7 n = head [x | x <- capicuas n, esFactorizable x n]

-- (esFactorizable x n) se verifica si x se puede escribir como producto
-- de dos números de n dígitos. Por ejemplo,
--    esFactorizable 1219 2  ==  True
--    esFactorizable 1217 2  ==  False
esFactorizable x n = aux i x
  where b = 10^n-1
        i = floor (sqrt (fromIntegral x))
        aux i x | i > b          = False
                | x `mod` i == 0 = x `div` i < b 
                | otherwise      = aux (i+1) x

-- Comparación de soluciones                                          --
-- =========================

-- El tiempo de cálculo de (mayorCapicuaP n) para las 6 definiciones es
--    +------+------+------+------+
--    | Def. | 2    | 3    | 4    |
--    |------+------+------+------|
--    |    1 | 0.01 | 0.13 | 1.39 |
--    |    2 | 0.03 | 2.07 |      |
--    |    3 | 0.05 | 3.86 |      |
--    |    4 | 0.01 | 0.89 |      |
--    |    5 | 0.03 | 1.23 |      |
--    |    6 | 0.02 | 1.03 |      |
--    |    7 | 0.01 | 0.02 | 0.02 |
--    +------+------+------+------+

-- ---------------------------------------------------------------------
-- Ejercicio 3. Los árboles binarios con valores en las hojas y en los
-- nodos se definen por 
--    data Arbol a = H a
--                 | Nd a (Arbol a) (Arbol a) 
--      deriving (Eq, Show)
-- 
-- Por ejemplo, el árbol
--          1 
--        /   \
--       2     3
--      / \   / \
--     4   5 6   7
--        / \
--       8   9 
-- se pueden representar por
--    ejArbol :: Arbol Int
--    ejArbol = Nd 1 (Nd 2 (H 4)
--                         (Nd 5 (H 8) (H 9)))
--                   (Nd 3 (H 6) (H 7))
--
-- Definir la función
--    recorrido :: Arbol t -> [t]
-- tal que (recorrido a) es el recorrido del árbol a por niveles desde
-- la raíz a las hojas y de izquierda a derecha. Por ejemplo,
--    λ> recorrido (Nd 1 (Nd 2 (H 4) (Nd 5 (H 8) (H 9))) (Nd 3 (H 6) (H 7)))
--    [1,2,3,4,5,6,7,8,9]
--    λ> recorrido (Nd 1 (Nd 3 (H 6) (H 7)) (Nd 2 (H 4) (Nd 5 (H 8) (H 9))))
--    [1,3,2,6,7,4,5,8,9]
--    λ> recorrido (Nd 1 (Nd 3 (H 6) (H 7)) (Nd 2 (H 4) (H 5)))
--    [1,3,2,6,7,4,5]
--    λ> recorrido (Nd 1 (Nd 2 (H 4) (H 5)) (Nd 3 (H 6) (H 7)))
--    [1,2,3,4,5,6,7]
--    λ> recorrido (Nd 1 (Nd 2 (H 4) (H 5)) (H 3))
--    [1,2,3,4,5]
--    λ> recorrido (Nd 1 (H 4) (H 3))
--    [1,4,3]
--    λ> recorrido (H 3)
--    [3]
-- ---------------------------------------------------------------------

data Arbol a = H a
             | Nd a (Arbol a) (Arbol a) 
  deriving (Eq, Show)

ejArbol :: Arbol Int
ejArbol = Nd 1 (Nd 2 (H 4)
                     (Nd 5 (H 8) (H 9)))
               (Nd 3 (H 6) (H 7))

-- 1ª definición
-- =============

recorrido :: Arbol t -> [t]
recorrido = concat . niveles

-- (niveles a) es la lista de los niveles del árbol a. Por ejemplo,
--    λ> niveles (Nd 1 (Nd 2 (H 4) (Nd 5 (H 8) (H 9))) (Nd 3 (H 6) (H 7)))
--    [[1],[2,3],[4,5,6,7],[8,9]]
niveles :: Arbol t -> [[t]]
niveles (H x)      = [[x]]
niveles (Nd x i d) = [x] : mezcla2 (niveles i) (niveles d)

-- (mezcla2 xss yss) es la lista obtenida concatenando los
-- correspondientes elementos de xss e yss. Por ejemplo,
--    λ> mezcla2 [[1,3],[2,5,7]] [[4],[1,9],[0,14]]
--    [[1,3,4],[2,5,7,1,9],[0,14]]
--    λ> mezcla2 [[1,3],[2,5,7]] [[4]]
--    [[1,3,4],[2,5,7]]
mezcla2 :: [[a]] -> [[a]] -> [[a]]
mezcla2 [] yss            = yss
mezcla2 xss []            = xss
mezcla2 (xs:xss) (ys:yss) = (xs ++ ys) : mezcla2 xss yss

-- 2ª definición
-- =============

recorrido2 :: Arbol t -> [t]
recorrido2 = concat . niveles2

-- (niveles2 a) es la lista de los niveles del árbol a. Por ejemplo,
--    λ> niveles2 (Nd 1 (Nd 2 (H 4) (Nd 5 (H 8) (H 9))) (Nd 3 (H 6) (H 7)))
--    [[1],[2,3],[4,5,6,7],[8,9]]
niveles2 :: Arbol t -> [[t]]
niveles2 a = takeWhile (not . null) [nivel n a | n <- [0..]]

-- (nivel n a) es el nivel iésimo del árbol a. Por ejemplo,
--    λ> nivel 2 (Nd 1 (Nd 2 (H 4) (Nd 5 (H 8) (H 9))) (Nd 3 (H 6) (H 7)))
--    [4,5,6,7]
--    λ> nivel 5 (Nd 1 (Nd 2 (H 4) (Nd 5 (H 8) (H 9))) (Nd 3 (H 6) (H 7)))
--    []
nivel :: Int -> Arbol t -> [t]
nivel 0 (H x)      = [x]
nivel n (H _)      = []
nivel 0 (Nd x _ _) = [x]
nivel n (Nd x i d) = nivel (n-1) i ++ nivel (n-1) d

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la función
--    diagonalesDescendentes :: Int -> [[(Int,Int)]]
-- tal que (diagonalesDescendentes) es la lista de las posiciones de las
-- diagonales secundarias de una matriz cuadrada de orden n desde la
-- posición superior izquierda hasta la inferior derecha, recorriendo
-- las diagonales de arriba hacia abajo. Por ejemplo,
--    λ> diagonalesDescendentes 4
--    [[(1,1)],
--     [(1,2),(2,1)],
--     [(1,3),(2,2),(3,1)],
--     [(1,4),(2,3),(3,2),(4,1)],
--     [(2,4),(3,3),(4,2)],
--     [(3,4),(4,3)],
--     [(4,4)]]
-----------------------------------------------------------------------

diagonalesDescendentes :: Int -> [[(Int,Int)]]
diagonalesDescendentes n =
  [[(i,m-i) | i <- [max 1 (m-n)..min n (m-1)]] | m <- [2..2*n]]

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función
--    diagonalesZigZag :: Int -> [[(Int,Int)]]
-- tal que (diagonalesZigZag n) es la lista de las posiciones de las
-- diagonales secundarias de una matriz cuadrada de orden n desde la
-- posición superior izquierda hasta la inferior derecha, recorriendo
-- las diagonales en zig zag; es decir, altenativamente de arriba hacia
-- abajo y de abajo hacia arriba. Por ejemplo, 
--    λ> diagonalesZigZag 4
--    [[(1,1)],
--     [(2,1),(1,2)],
--     [(1,3),(2,2),(3,1)],
--     [(4,1),(3,2),(2,3),(1,4)],
--     [(2,4),(3,3),(4,2)],
--     [(4,3),(3,4)],
--     [(4,4)]]
-----------------------------------------------------------------------

diagonalesZigZag :: Int -> [[(Int,Int)]]
diagonalesZigZag n =
  [f d | (f,d) <- zip (cycle [id,reverse]) (diagonalesDescendentes n)]

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Definir la función 
--    numeracion :: (Int -> [[(Int,Int)]]) -> Int -> Matrix Int
-- tal que (numeracion r n) es la matriz cuadrada de orden n obtenida
-- numerando sus elementos con los números del 0 al n^2-1 según su
-- posición en el recorrido r. Por ejemplo,
--    λ> numeracion diagonalesDescendentes 4
--    (  0  1  3  6 )
--    (  2  4  7 10 )
--    (  5  8 11 13 )
--    (  9 12 14 15 )
--    
--    λ> numeracion diagonalesZigZag 4
--    (  0  2  3  9 )
--    (  1  4  8 10 )
--    (  5  7 11 14 )
--    (  6 12 13 15 )
-- ---------------------------------------------------------------------

numeracion :: (Int -> [[(Int,Int)]]) -> Int -> Matrix Int
numeracion r n =
  fromList n n (elems (numeracionAux r n))

numeracionAux :: (Int -> [[(Int,Int)]]) -> Int -> Array (Int,Int) Int
numeracionAux r n =
  array ((1,1),(n,n)) (zip (concat (r n)) [0..])

-- ---------------------------------------------------------------------
-- Ejercicio 4.4. Comprobar con QuickCheck que para para cualquier
-- número natural n se verifica que son iguales las diagonales
-- principales de (numeracion r n) donde r es cualquiera de los dos
-- recorridos definidos (es decir, diagonalesDescendentes y
-- diagonalesZigZag). 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_numeracion :: Int -> Property
prop_numeracion n =
  n >= 0 ==> 
   getDiag (numeracion diagonalesDescendentes n) ==
   getDiag (numeracion diagonalesZigZag n)

-- La comprobación es
--    λ> quickCheck prop_numeracion
--    +++ OK, passed 100 tests.
