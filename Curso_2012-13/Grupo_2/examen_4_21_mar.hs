-- Informática (1º del Grado en Matemáticas)
-- 4º examen de evaluación continua (21 de marzo de 2013)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. [2.5 puntos] Los pares de números impares se pueden
-- ordenar según su suma y, entre los de la misma suma, su primer
-- elemento como sigue:  
--    (1,1),(1,3),(3,1),(1,5),(3,3),(5,1),(1,7),(3,5),(5,3),(7,1),...
-- Definir la función
--     paresDeImpares :: [(Int,Int)]
-- tal que paresDeImpares es la lista de pares de números impares con
-- dicha ordenación. Por ejemplo,  
--    ghci> take 10 paresDeImpares
--    [(1,1),(1,3),(3,1),(1,5),(3,3),(5,1),(1,7),(3,5),(5,3),(7,1)]
-- Basándose en paresDeImpares, definir la función
--    posicion
-- tal que (posicion p) es la posición del par p en la sucesión. Por
-- ejemplo, 
--    posicion (3,5)  ==  7
-- ---------------------------------------------------------------------
 
paresDeImpares :: [(Int,Int)]
paresDeImpares = 
  [(x,n-x) | n <- [2,4..], x <- [1,3..n]]
 
posicion :: (Int,Int) -> Int    
posicion (x,y) = 
  length (takeWhile (/=(x,y)) paresDeImpares)
    
-- ---------------------------------------------------------------------
-- Ejercicio 2. [2.5 puntos] Definir la constante 
--    cuadradosConcatenados :: [(Integer,Integer,Integer)]
-- de forma que su valor es la lista de ternas (x,y,z) de tres cuadrados
-- perfectos tales que z es la concatenación de x e y. Por ejemplo,
--   ghci> take 5 cuadradosConcatenados
--   [(4,9,49),(16,81,1681),(36,100,36100),(1,225,1225),(4,225,4225)]
-- ---------------------------------------------------------------------
 
cuadradosConcatenados :: [(Integer,Integer,Integer)]
cuadradosConcatenados =
  [(x,y,concatenacion x y) | y <- cuadrados, 
                             x <- [1..y], 
                             esCuadrado x,     
                             esCuadrado (concatenacion x y)]
    
-- cuadrados es la lista de los números que son cuadrados perfectos. Por
-- ejemplo,  
--    take 5 cuadrados  ==  [1,4,9,16,25]
cuadrados :: [Integer]
cuadrados = [x^2 | x <- [1..]]
 
-- (concatenacion x y) es el número obtenido concatenando los números x
-- e y. Por ejemplo, 
--    concatenacion 3252 476  ==  3252476
concatenacion :: Integer -> Integer -> Integer
concatenacion x y = read (show x ++ show y)
 
-- (esCuadrado x) se verifica si  x es un cuadrado perfecto; es decir,
-- si existe un y tal que y^2 es igual a x. Por ejemplo,  
--    esCuadrado 16  ==  True
--    esCuadrado 17  ==  False
esCuadrado :: Integer -> Bool
esCuadrado x = y^2 == x
  where y = round (sqrt (fromIntegral x))

-- ---------------------------------------------------------------------
-- Ejercicio 3. [2.5 puntos] La expresiones aritméticas se pueden
-- representar mediante el siguiente tipo 
--    data Expr = V Char 
--              | N Int 
--              | S Expr Expr
--              | P Expr Expr
-- por ejemplo, la expresión "z*(3+x)" se representa por 
-- (P (V 'z') (S (N 3) (V 'x'))). 
--
-- Definir la función
--    sumas :: Expr -> Int
-- tal que (sumas e) es el número de sumas en la expresión e. Por 
-- ejemplo, 
--    sumas (P (V 'z') (S (N 3) (V 'x')))  ==  1
--    sumas (S (V 'z') (S (N 3) (V 'x')))  ==  2
--    sumas (P (V 'z') (P (N 3) (V 'x')))  ==  0
-- ---------------------------------------------------------------------
                   
data Expr = V Char 
          | N Int 
          | S Expr Expr
          | P Expr Expr

sumas :: Expr -> Int
sumas (V _)   = 0
sumas (N _)   = 0
sumas (S x y) = 1 + sumas x + sumas y
sumas (P x y) = sumas x + sumas y

-- ---------------------------------------------------------------------
-- Ejercicio 4. [2.5 puntos]  Los árboles binarios se pueden representar
-- mediante el tipo Arbol definido por
--    data Arbol = H2 Int 
--               | N2 Int Arbol Arbol
-- Por ejemplo, el árbol
--         1
--        / \ 
--       /   \
--      2     5
--     / \   / \
--    3   4 6   7
-- se puede representar por 
--    N2 1 (N2 2 (H2 3) (H2 4)) (N2 5 (H2 6) (H2 7))
--
-- Definir la función
--    ramas :: Arbol -> [[Int]]              
-- tal que (ramas a) es la lista de las ramas del árbol. Por ejemplo, 
--    ghci> ramas (N2 1 (N2 2 (H2 3) (H2 4)) (N2 5 (H2 6) (H2 7)))
--    [[1,2,3],[1,2,4],[1,5,6],[1,5,7]]
-- ---------------------------------------------------------------------

data Arbol = H2 Int 
           | N2 Int Arbol Arbol
              
ramas :: Arbol -> [[Int]]              
ramas (H2 x)     = [[x]]
ramas (N2 x i d) = [x:r | r <- ramas i ++  ramas d]

