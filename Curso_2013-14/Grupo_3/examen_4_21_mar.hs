-- Informática (1º del Grado en Matemáticas)
-- 4º examen de evaluación continua (21 de marzo de 2014)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. [2.5 puntos] Definir la función
--    interpretaciones :: [a] -> [[(a,Int)]]
-- tal que (interpretaciones xs) es la lista de las interpretaciones
-- sobre la lista de las variables proposicionales xs sobre los valores
-- de verdad 0 y 1. Por ejemplo,
--    ghci> interpretaciones "A"
--    [[('A',0)],[('A',1)]]
--    ghci> interpretaciones "AB"
--    [[('A',0),('B',0)],[('A',0),('B',1)],
--     [('A',1),('B',0)],[('A',1),('B',1)]]
--    ghci> interpretaciones "ABC"
--    [[('A',0),('B',0),('C',0)],[('A',0),('B',0),('C',1)],
--     [('A',0),('B',1),('C',0)],[('A',0),('B',1),('C',1)],
--     [('A',1),('B',0),('C',0)],[('A',1),('B',0),('C',1)],
--     [('A',1),('B',1),('C',0)],[('A',1),('B',1),('C',1)]]
-- ---------------------------------------------------------------------

interpretaciones :: [a] -> [[(a,Int)]]
interpretaciones [] = [[]]
interpretaciones (x:xs) = 
    [(x,0):i | i <-is] ++ [(x,1):i | i <- is]
    where is = interpretaciones xs

-- ---------------------------------------------------------------------
-- Ejercicio 2. [2.5 puntos] Los números de Hamming forman una sucesión
-- estrictamente creciente de números que cumplen las siguientes
-- condiciones:  
--    * El número 1 está en la sucesión.
--    * Si x está en la sucesión, entonces 2x, 3x y 5x también están.
--    * Ningún otro número está en la sucesión. 
-- Los primeros términos de la sucesión de Hamming son
--    1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, ...
--
-- Definir la función
--    siguienteHamming :: Int -> Int
-- tal que (siguienteHamming x) es el menor término de la sucesión de
-- Hamming mayor que x. Por ejemplo,
--    siguienteHamming 10  ==  12
--    siguienteHamming 12  ==  15
--    siguienteHamming 15  ==  16
-- ---------------------------------------------------------------------

siguienteHamming :: Int -> Int
siguienteHamming x = head (dropWhile (<=x) hamming)

-- hamming es la sucesión de Hamming. Por ejemplo,
--    take 12 hamming == [1,2,3,4,5,6,8,9,10,12,15,16]
hamming :: [Int]
hamming = 1 : mezcla3 [2*i | i <- hamming]  
                      [3*i | i <- hamming]  
                      [5*i | i <- hamming]  

-- (mezcla3 xs ys zs) es la lista obtenida mezclando las listas
-- ordenadas xs, ys y zs y eliminando los elementos duplicados. Por
-- ejemplo, 
--    ghci> mezcla3 [2,4,6,8,10] [3,6,9,12] [5,10]
--    [2,3,4,5,6,8,9,10,12]
mezcla3 :: [Int] -> [Int] -> [Int] -> [Int]
mezcla3 xs ys zs = mezcla2 xs (mezcla2 ys zs)  

-- (mezcla2 xs ys zs) es la lista obtenida mezclando las listas
-- ordenadas xs e ys y eliminando los elementos duplicados. Por ejemplo,
--    ghci> mezcla2 [2,4,6,8,10,12] [3,6,9,12]
--    [2,3,4,6,8,9,10,12]
mezcla2 :: [Int] -> [Int] -> [Int]
mezcla2 p@(x:xs) q@(y:ys) | x < y     = x:mezcla2 xs q
                          | x > y     = y:mezcla2 p  ys  
                          | otherwise = x:mezcla2 xs ys
mezcla2 []       ys                   = ys
mezcla2 xs       []                   = xs

-- ---------------------------------------------------------------------
-- Ejercicio 3. [2.5 puntos] Las operaciones de suma, resta y
-- multiplicación se pueden representar mediante el siguiente tipo de
-- datos 
--    data Op = S | R | M
-- La expresiones aritméticas con dichas operaciones se pueden
-- representar mediante el siguiente tipo de dato algebraico
--    data Expr = N Int | A Op Expr Expr
-- Por ejemplo, la expresión
--    (7-3)+(2*5)
-- se representa por
--    A S (A R (N 7) (N 3)) (A M (N 2) (N 5))
--
-- Definir la función
--    valor :: Expr -> Int
-- tal que (valor e) es el valor de la expresión e. Por ejemplo,
--    valor (A S (A R (N 7) (N 3)) (A M (N 2) (N 5)))  ==  14
--    valor (A M (A R (N 7) (N 3)) (A S (N 2) (N 5)))  ==  28
-- ---------------------------------------------------------------------

data Op = S | R | M

data Expr = N Int | A Op Expr Expr

valor :: Expr -> Int
valor (N x) = x
valor (A o e1 e2) = aplica o (valor e1) (valor e2)

aplica :: Op -> Int -> Int -> Int
aplica S x y = x+y
aplica R x y = x-y
aplica M x y = x*y

-- ---------------------------------------------------------------------
-- Ejercicio 4. [2.5 puntos] Los polinomios con coeficientes naturales
-- se pueden representar mediante el siguiente tipo algebraico
--    data Polinomio = O | C Int Int Polinomio
-- Por ejemplo, el polinomio 2x^5 + 4x^3 + 2x se representa por
--    C 2 5 (C 4 3 (C 2 1 O))
-- También se pueden representar mediante listas no crecientes de
-- naturales. Por ejemplo, el polinomio 2x^5 + 4x^3 + 2x se representa
-- por 
--    [5,5,3,3,3,3,1,1]
-- en la lista anterior, el número de veces que aparece cada número n es
-- igual al coeficiente de x^n en el polinomio.
-- 
-- Definir la función
--    transformaPol :: Polinomio -> [Int]
-- tal que (transformaPol p) es el polinomio obtenido transformado el
-- polinomio p de la primera representación a la segunda. Por ejemplo, 
--    transformaPol (C 2 5 (C 4 3 (C 2 1 O)))  ==  [5,5,3,3,3,3,1,1]
--    transformaPol (C 2 100 (C 3 1 O))        ==  [100,100,1,1,1]
-- ---------------------------------------------------------------------

data Polinomio = O | C Int Int Polinomio

transformaPol :: Polinomio -> [Int]
transformaPol O         = []
transformaPol (C a n p) = replicate a n ++ transformaPol p

