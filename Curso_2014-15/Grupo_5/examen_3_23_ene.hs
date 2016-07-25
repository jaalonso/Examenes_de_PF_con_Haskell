-- Informática (1º del Grado en Matemáticas)
-- 3º examen de evaluación continua (23 de enero de 2015)
-- ---------------------------------------------------------------------

import Data.List 
import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    divisiblesPorAlguno :: [Int] -> [Int]
-- tal que (divisiblesPorAlguno xs) es la lista de los números que son
-- divisibles por algún elemento de xs. Por ejemplo, 
--    take 10 (divisiblesPorAlguno [2,3])    ==  [2,3,4,6,8,9,10,12,14,15]
--    take 10 (divisiblesPorAlguno [2,4,3])  ==  [2,3,4,6,8,9,10,12,14,15]
--    take 10 (divisiblesPorAlguno [2,5,3])  ==  [2,3,4,5,6,8,9,10,12,14]
-- ---------------------------------------------------------------------

divisiblesPorAlguno :: [Int] -> [Int]
divisiblesPorAlguno xs = [n | n <- [1..], divisiblePorAlguno xs n]

-- 1ª definición (con any)
divisiblePorAlguno :: [Int] -> Int -> Bool
divisiblePorAlguno xs n = any (\x -> n `mod` x == 0) xs

-- 2ª definición (por comprensión)
divisiblePorAlguno1 :: [Int] -> Int -> Bool
divisiblePorAlguno1 xs n = or [n `mod` x == 0 | x <- xs]

-- 3ª definición (por recursión)
divisiblePorAlguno2 :: [Int] -> Int -> Bool
divisiblePorAlguno2 [] _     = False
divisiblePorAlguno2 (x:xs) n = n `mod` x == 0 || divisiblePorAlguno2 xs n

-- ---------------------------------------------------------------------
-- Ejercicio 2. Las matrices pueden representarse mediante tablas cuyos
-- índices son pares de números naturales:    
--    type Matriz = Array (Int,Int) Int
--
-- Definir la función 
--    ampliada :: Matriz -> Matriz
-- tal que (ampliada p) es la matriz obtenida ampliando p añadiéndole
-- al final una columna con la suma de los elementos de cada fila y
-- añadiéndole al final una fila con la suma de los elementos de cada
-- columna. Por ejemplo, al ampliar las matrices
--    |1 2 3|      |1 2|
--    |4 5 6|      |3 4|
--                 |5 6|
-- se obtienen, respectivamente
--    |1 2 3  6|   |1  2  3|
--    |4 5 6 15|   |3  4  7|
--    |5 7 9 21|   |5  6 11|
--                 |9 12 21|
-- En Haskell,
--    ghci> ampliada (listArray ((1,1),(2,3)) [1,2,3, 4,5,6])
--    array ((1,1),(3,4)) [((1,1),1),((1,2),2),((1,3),3),((1,4),6),
--                         ((2,1),4),((2,2),5),((2,3),6),((2,4),15),
--                         ((3,1),5),((3,2),7),((3,3),9),((3,4),21)]
--    ghci> ampliada (listArray ((1,1),(3,2)) [1,2, 3,4, 5,6])
--    array ((1,1),(4,3)) [((1,1),1),((1,2),2),((1,3),3),
--                         ((2,1),3),((2,2),4),((2,3),7),
--                         ((3,1),5),((3,2),6),((3,3),11),
--                         ((4,1),9),((4,2),12),((4,3),21)]
-- ---------------------------------------------------------------------

type Matriz = Array (Int,Int) Int

ampliada :: Matriz -> Matriz
ampliada p = array ((1,1),(m+1,n+1)) 
                   [((i,j),f i j) | i <- [1..m+1], j <- [1..n+1]]
    where 
      (_,(m,n)) = bounds p
      f i j | i <= m   && j <= n   = p ! (i,j)
            | i <= m   && j == n+1 = sum [p!(i,j) | j <- [1..n]]
            | i == m+1 && j <= n   = sum [p!(i,j) | i <- [1..m]]
            | i == m+1 && j == n+1 = sum [p!(i,j) | i <- [1..m], j <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 3. El siguiente tipo de dato representa expresiones
-- construidas con variables, sumas y productos
--    data Expr = Var String
--              | S Expr Expr
--              | P Expr Expre
--              deriving (Eq, Show)
-- Por ejemplo, x*(y+z) se representa por (P (V "x") (S (V "y") (V "z"))) 
-- 
-- Una expresión está en forma normal si es una suma de términos. Por
-- ejemplo, x*(y*z) y x+(y*z) está en forma normal; pero x*(y+z) y
-- (x+y)*(x+z) no lo están. 
-- 
-- Definir la función 
--    normal :: Expr -> Expr
-- tal que (normal e) es la forma normal de la expresión e obtenida
-- aplicando, mientras que sea posible, las propiedades distributivas:
--    (a+b)*c = a*c+b*c
--    c*(a+b) = c*a+c*b
-- Por ejemplo,
--    ghci> normal (P (S (V "x") (V "y")) (V "z"))
--    S (P (V "x") (V "z")) (P (V "y") (V "z"))
--    ghci> normal (P (V "z") (S (V "x") (V "y")))
--    S (P (V "z") (V "x")) (P (V "z") (V "y"))
--    ghci> normal (P (S (V "x") (V "y")) (S (V "u") (V "v")))
--    S (S (P (V "x") (V "u")) (P (V "x") (V "v"))) 
--      (S (P (V "y") (V "u")) (P (V "y") (V "v")))
--    ghci> normal (S (P (V "x") (V "y")) (V "z"))
--    S (P (V "x") (V "y")) (V "z")
--    ghci> normal (V "x")
--    V "x"
-- ---------------------------------------------------------------------

data Expr = V String
          | S Expr Expr
          | P Expr Expr
          deriving (Eq, Show)

normal :: Expr -> Expr
normal (V v)   = V v
normal (S a b) = S (normal a) (normal b)
normal (P a b) = p (normal a) (normal b)
    where p (S a b) c = S (p a c) (p b c)
          p a (S b c) = S (p a b) (p a c)
          p a b       = P a b

-- ---------------------------------------------------------------------
-- Ejercicio 4. Los primeros números de Fibonacci son
--    1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, ...
-- tales que los dos primeros son iguales a 1 y los siguientes se
-- obtienen sumando los dos anteriores. 
-- 
-- El teorema de Zeckendorf establece que todo entero positivo n se
-- puede representar, de manera única, como la suma de números de
-- Fibonacci no consecutivos decrecientes. Dicha suma se llama la
-- representación de Zeckendorf de n. Por ejemplo, la representación de
-- Zeckendorf de 100 es  
--    100 = 89 + 8 + 3
-- Hay otras formas de representar 100 como sumas de números de
-- Fibonacci; por ejemplo,
--    100 = 89 +  8 + 2 + 1
--    100 = 55 + 34 + 8 + 3
-- pero no son representaciones de Zeckendorf porque 1 y 2 son números
-- de Fibonacci consecutivos, al igual que 34 y 55.
-- 
-- Definir la función
--    zeckendorf :: Integer -> [Integer]
-- tal que (zeckendorf n) es la representación de Zeckendorf de n. Por
-- ejemplo, 
--    zeckendorf 100       == [89,8,3]
--    zeckendorf 2014      == [1597,377,34,5,1]
--    zeckendorf 28656     == [17711,6765,2584,987,377,144,55,21,8,3,1]
--    zeckendorf 14930396  == [14930352,34,8,2]
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========
zeckendorf1 :: Integer -> [Integer]
zeckendorf1 n = reverse (head (aux n (tail fibs)))
    where aux 0 _ = [[]]
          aux n (x:y:zs) 
              | x <= n     = [x:xs | xs <- aux (n-x) zs] ++ aux n (y:zs)
              | otherwise  = []

-- fibs es la sucesión de los números de Fibonacci. Por ejemplo,
--    take 14 fibs  == [1,1,2,3,5,8,13,21,34,55,89,144,233,377]
fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

-- 2ª solución
-- ===========
zeckendorf2 :: Integer -> [Integer]
zeckendorf2 n = aux n (reverse (takeWhile (<= n) fibs))
    where aux 0 _ = []
          aux n (x:xs) = x : aux (n-x) (dropWhile (>n-x) xs)

-- 3ª solución
-- ===========
zeckendorf3 :: Integer -> [Integer]
zeckendorf3 0 = []
zeckendorf3 n = x : zeckendorf3 (n - x) 
    where x = last (takeWhile (<= n) fibs)

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    ghci> zeckendorf1 300000
--    [196418,75025,17711,6765,2584,987,377,89,34,8,2]
--    (0.72 secs, 58478576 bytes)
--    ghci> zeckendorf2 300000
--    [196418,75025,17711,6765,2584,987,377,89,34,8,2]
--    (0.00 secs, 517852 bytes)
--    ghci> zeckendorf3 300000
--    [196418,75025,17711,6765,2584,987,377,89,34,8,2]
--    (0.00 secs, 515360 bytes)
-- Se observa que las definiciones más eficientes son la 2ª y la 3ª.

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    maximoIntercambio :: Int -> Int
-- tal que (maximoIntercambio x) es el máximo número que se puede
-- obtener intercambiando dos dígitos de x. Por ejemplo, 
--    maximoIntercambio 983562  ==  986532
--    maximoIntercambio 31524   ==  51324
--    maximoIntercambio 897     ==  987
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============
maximoIntercambio :: Int -> Int
maximoIntercambio = maximum . intercambios

-- (intercambios x) es la lista de los números obtenidos intercambiando
-- dos dígitos de x. Por ejemplo,
--    intercambios 1234  ==  [2134,3214,4231,1324,1432,1243]
intercambios :: Int -> [Int]
intercambios x = [intercambio i j x | i <- [0..n-2], j <- [i+1..n-1]]
    where n = length (show x)

-- (intercambio i j x) es el número obtenido intercambiando las cifras
-- que ocupan las posiciones i y j (empezando a contar en cero) del
-- número x. Por ejemplo,
--    intercambio 2 5 123456789  ==  126453789
intercambio :: Int -> Int -> Int -> Int
intercambio i j x = read (concat [as,[d],cs,[b],ds])
    where xs        = show x
          (as,b:bs) = splitAt i xs 
          (cs,d:ds) = splitAt (j-i-1) bs

-- 2ª definición (con vectores)
-- ============================

maximoIntercambio2 :: Int -> Int
maximoIntercambio2 = read . elems . maximum . intercambios2

-- (intercambios2 x) es la lista de los vectores obtenidos
-- intercambiando dos elementos del vector de dígitos de x. Por ejemplo, 
--    ghci> intercambios2 1234
--    [array (0,3) [(0,'2'),(1,'1'),(2,'3'),(3,'4')],
--     array (0,3) [(0,'3'),(1,'2'),(2,'1'),(3,'4')],
--     array (0,3) [(0,'4'),(1,'2'),(2,'3'),(3,'1')],
--     array (0,3) [(0,'1'),(1,'3'),(2,'2'),(3,'4')],
--     array (0,3) [(0,'1'),(1,'4'),(2,'3'),(3,'2')],
--     array (0,3) [(0,'1'),(1,'2'),(2,'4'),(3,'3')]]
intercambios2 :: Int -> [Array Int Char]
intercambios2 x = [intercambioV i j v | i <- [0..n-2], j <- [i+1..n-1]]
    where xs = show x
          n  = length xs
          v  = listArray (0,n-1) xs

-- (intercambioV i j v) es el vector obtenido intercambiando los
-- elementos de v que ocupan las posiciones i y j. Por ejemplo,
--    ghci> intercambioV 2 4 (listArray (0,4) [3..8])
--    array (0,4) [(0,3),(1,4),(2,7),(3,6),(4,5)]
intercambioV i j v = v // [(i,v!j),(j,v!i)]
