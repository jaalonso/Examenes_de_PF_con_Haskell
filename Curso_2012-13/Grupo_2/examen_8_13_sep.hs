-- Informática (1º del Grado en Matemáticas)
-- Examen de la 2ª convocatoria (13 de septiembre de 2013)
-- ---------------------------------------------------------------------

import Data.List
import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. [1 punto] Las notas se pueden agrupar de distinta
-- formas. Una es por la puntuación; por ejemplo,
--    [(4,["juan","ana"]),(9,["rosa","luis","mar"])]
-- Otra es por nombre; por ejemplo,
--    [("ana",4),("juan",4),("luis",9),("mar",9),("rosa",9)]
-- 
-- Definir la función
--    transformaPaN :: [(Int,[String])] -> [(String,Int)]
-- tal que (transformaPaN xs) es la agrupación de notas por nombre
-- correspondiente a la agrupación de notas por puntuación xs. Por
-- ejemplo, 
--    > transformaPaN [(4,["juan","ana"]),(9,["rosa","luis","mar"])]
--    [("ana",4),("juan",4),("luis",9),("mar",9),("rosa",9)]
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
transformaPaN :: [(Int,[String])] -> [(String,Int)]
transformaPaN xs = sort [(a,n) | (n,as) <- xs, a <- as]

-- 2ª definición (por recursión):
transformaPaN2 :: [(Int,[String])] -> [(String,Int)]
transformaPaN2 []          = []
transformaPaN2 ((n,xs):ys) = [(x,n)|x<-xs] ++ transformaPaN2 ys

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. [1 punto] Definir la función
--    transformaNaP :: [(String,Int)] -> [(Int,[String])] 
-- tal que (transformaPaN xs) es la agrupación de notas por nombre
-- correspondiente a la agrupación de notas por puntuación xs. Por
-- ejemplo, 
--    > transformaNaP [("ana",4),("juan",4),("luis",9),("mar",9),("rosa",9)]
--    [(4,["ana","juan"]),(9,["luis","mar","rosa"])]
-- ---------------------------------------------------------------------

transformaNaP :: [(String,Int)] -> [(Int,[String])] 
transformaNaP xs = [(n, [a | (a,n') <- xs, n' == n]) | n <- notas]
    where notas = sort (nub [n | (_,n) <- xs]) 

-- ---------------------------------------------------------------------
-- Ejercicio 2. [2 puntos] Definir la función
--    multiplosCon9 :: Integer -> [Integer]
-- tal que (multiplosCon9 n) es la lista de los múltiplos de n cuya
-- única cifra es 9. Por ejemplo,
--    take 3 (multiplosCon9 3)  ==  [9,99,999]
--    take 3 (multiplosCon9 7)  ==  [999999,999999999999,999999999999999999]
-- Calcular el menor múltiplo de 2013 formado sólo por nueves.
-- ---------------------------------------------------------------------
          
multiplosCon9 :: Integer -> [Integer]
multiplosCon9 n = [x | x <- numerosCon9, rem x n == 0]

-- numerosCon9 es la lista de los número cuyas cifras son todas iguales
-- a 9. Por ejemplo,
--    take 5 numerosCon9  ==  [9,99,999,9999,99999]
numerosCon9 :: [Integer]
numerosCon9 = [10^n-1 | n <- [1..]]

-- 2ª definición (por recursión):
numerosCon9R :: [Integer]
numerosCon9R = 9 : sig 9
    where sig x = (10*x+9) : sig (10*x+9)

-- El cálculo es
--    ghci> head (multiplosCon9 2013)
--    999999999999999999999999999999999999999999999999999999999999

-- ---------------------------------------------------------------------
-- Ejercicio 3. [2 puntos] Una sucesión es suave si valor absoluto de la
-- diferencia de sus términos consecutivos es 1. Definir la función 
--    suaves :: Int -> [[Int]]
-- tal que (suaves n) es la lista de las sucesiones suaves de longitud n
-- cuyo último término es 0. Por ejemplo,
--    suaves 2  ==  [[1,0],[-1,0]]
--    suaves 3  ==  [[2,1,0],[0,1,0],[0,-1,0],[-2,-1,0]]
-- ---------------------------------------------------------------------

suaves :: Int -> [[Int]]
suaves 0 = []
suaves 1 = [[0]]
suaves n = concat [[x+1:x:xs,x-1:x:xs] | (x:xs) <- suaves (n-1)] 

-- ---------------------------------------------------------------------
-- Ejercicio 4. [2 puntos] Los árboles binarios se pueden representar
-- mediante el tipo Arbol definido por 
--    data Arbol a = H a 
--                 | N a (Arbol a) (Arbol a)
--                 deriving Show
-- Por ejemplo, el árbol
--          1
--         / \ 
--        /   \
--       4     6
--      / \   / \
--     0   7 4   3
-- se puede definir por 
--    ej1 :: Arbol Int
--    ej1 = N 1 (N 4 (H 0) (H 7)) (N 6 (H 4) (H 3))
--
-- Definir la función
--    algunoArbol :: Arbol t -> (t -> Bool) -> Bool
-- tal que (algunoArbol a p) se verifica si algún elemento del árbol a
-- cumple la propiedad p. Por ejemplo,
--    algunoArbol ej1 (>9)  ==  False
--    algunoArbol ej1 (>5)  ==  True
-- ---------------------------------------------------------------------

data Arbol a = H a 
             | N a (Arbol a) (Arbol a)
             deriving Show

ej1 :: Arbol Int
ej1 = N 1 (N 4 (H 0) (H 7)) (N 6 (H 4) (H 3))

algunoArbol :: Arbol a -> (a -> Bool) -> Bool
algunoArbol (H x) p     = p x
algunoArbol (N x i d) p = p x || algunoArbol i p || algunoArbol d p

-- ----------------------------------------------------------------------
-- Ejercicio 5. [2 puntos] Las matrices enteras se pueden representar
-- mediante tablas con índices enteros:
--    type Matriz = Array (Int,Int) Int
-- 
-- Definir la función
--    matrizPorBloques :: Matriz -> Matriz -> Matriz -> Matriz -> Matriz
-- tal que (matrizPorBloques p1 p2 p3 p4) es la matriz cuadrada de orden
-- 2nx2n construida con las matrices cuadradas de orden nxn p1, p2 p3 y
-- p4 de forma que p1 es su bloque superior izquierda, p2 es su bloque
-- superior derecha, p3 es su bloque inferior izquierda y p4 es su bloque
-- inferior derecha. Por ejemplo, si p1, p2, p3 y p4 son las matrices
-- definidas por
--    p1, p2, p3, p4 :: Matriz
--    p1 = listArray ((1,1),(2,2)) [1,2,3,4]
--    p2 = listArray ((1,1),(2,2)) [6,5,7,8]
--    p3 = listArray ((1,1),(2,2)) [0,6,7,1]
--    p4 = listArray ((1,1),(2,2)) [5,2,8,3]
-- entonces
--    ghci> matrizPorBloques p1 p2 p3 p4
--    array ((1,1),(4,4)) [((1,1),1),((1,2),2),((1,3),6),((1,4),5),
--                         ((2,1),3),((2,2),4),((2,3),7),((2,4),8),
--                         ((3,1),0),((3,2),6),((3,3),5),((3,4),2),
--                         ((4,1),7),((4,2),1),((4,3),8),((4,4),3)]
-- --------------------------------------------------------------------- 

type Matriz = Array (Int,Int) Int

p1, p2, p3, p4 :: Matriz
p1 = listArray ((1,1),(2,2)) [1,2,3,4]
p2 = listArray ((1,1),(2,2)) [6,5,7,8]
p3 = listArray ((1,1),(2,2)) [0,6,7,1]
p4 = listArray ((1,1),(2,2)) [5,2,8,3]

matrizPorBloques :: Matriz -> Matriz -> Matriz -> Matriz -> Matriz
matrizPorBloques p1 p2 p3 p4 =
  array ((1,1),(m,m)) [((i,j), f i j) | i <- [1..m], j <- [1..m]]
  where ((_,_),(n,_)) = bounds p1
        m = 2*n
        f i j | i <= n && j <= n = p1!(i,j)
              | i <= n && j >  n = p2!(i,j-n)
              | i >  n && j <= n = p3!(i-n,j)
              | i >  n && j >  n = p4!(i-n,j-n)                             
