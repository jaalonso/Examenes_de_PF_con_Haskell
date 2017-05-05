-- Informática (1º del Grado en Matemáticas)
-- 4º examen de evaluación continua (22 de marzo de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Matrix

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    reducida :: Eq a => [a] -> [a]
-- tal que (reducida xs) es la lista obtenida a partir de xs de forma
-- que si hay dos o más elementos idénticos consecutivos, borra las
-- repeticiones y deja sólo el primer elemento. Por ejemplo, 
--    ghci> reducida "eesssooo essss   toodddooo"
--    "eso es todo"
-- ---------------------------------------------------------------------

-- 1ª solución (por recursión):
reducida1 :: Eq a => [a] -> [a]
reducida1 []     = []
reducida1 (x:xs) = x : reducida1 (dropWhile (==x) xs)

-- 2ª solución (por comprensión):
reducida2 :: Eq a => [a] -> [a]
reducida2 xs = [x | (x:_) <- group xs]

-- 3ª solución (sin argumentos):
reducida3 :: Eq a => [a] -> [a]
reducida3 = map head . group

-- ---------------------------------------------------------------------
-- Ejercicio 2. La codificación de Luka consiste en añadir detrás de
-- cada vocal la letra 'p' seguida de la vocal. Por ejemplo, la palabra
-- "elena" se codifica como "epelepenapa" y "luisa" por "lupuipisapa".
--
-- Definir la función
--    codifica   :: String -> String
-- tal que (codifica cs) es la codificación de Luka de la cadena cs. Por
-- ejemplo,  
--    ghci> codifica "elena admira a luisa"
--    "epelepenapa apadmipirapa apa lupuipisapa"
--    ghci> codifica "todo para nada"
--    "topodopo paparapa napadapa"
-- ---------------------------------------------------------------------

codifica :: String -> String
codifica "" = ""
codifica (c:cs) | esVocal c = c : 'p' : c : codifica cs
                | otherwise = c : codifica cs

esVocal :: Char -> Bool
esVocal c = c `elem` "aeiou"

-- ---------------------------------------------------------------------
-- Ejercicio 3. Un elemento de una matriz es un máximo local si es mayor
-- que todos sus vecinos. Por ejemplo, sea ejM la matriz definida por  
--    ejM :: Matrix Int
--    ejM = fromLists [[1,0,0,8],
--                     [0,2,0,3],
--                     [0,0,0,5],
--                     [3,5,7,6],
--                     [1,2,3,4]]
-- Los máximos locales de ejM son 8 (en la posición (1,4)), 2 (en la
-- posición (2,2)) y 7 (en la posición (4,3)). 
-- 
-- Definir la función 
--    maximosLocales :: Matrix Int -> [((Int,Int),Int)]
-- tal que (maximosLocales p) es la lista de las posiciones en las que
-- hay un máximo local, con el valor correspondiente. Por ejemplo,
--    maximosLocales ejM == [((1,4),8),((2,2),2),((4,3),7)]
-- ---------------------------------------------------------------------

ejM :: Matrix Int
ejM = fromLists [[1,0,0,8],
                 [0,2,0,3],
                 [0,0,0,5],
                 [3,5,7,6],
                 [1,2,3,4]]

maximosLocales :: Matrix Int -> [((Int,Int),Int)]
maximosLocales p = 
    [((i,j),p!(i,j)) | i <- [1..m]
                     , j <- [1..n]
                     , and [p!(a,b) < p!(i,j) | (a,b) <- vecinos (i,j)]] 
    where m = nrows p
          n = ncols p
          vecinos (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)]
                                 , b <- [max 1 (j-1)..min n (j+1)]
                                 , (a,b) /= (i,j)]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Los árboles binarios se pueden representar con el de
-- dato algebraico 
--    data Arbol a = H
--                 | N a (Arbol a) (Arbol a)
--                 deriving Show
-- Por ejemplo, los árboles 
--         9                9                
--        / \              / 
--       /   \            /  
--      8     6          8  
--     / \   / \        / \ 
--    3   2 4   5      3   2 
-- se pueden representar por
--    ej1, ej2:: Arbol Int
--    ej1 = N 9 (N 8 (N 3 H H) (N 2 H H)) (N 6 (N 4 H H) (N 5 H H))
--    ej2 = N 9 (N 8 (N 3 H H) (N 2 H H)) H
--
-- Para indicar las posiciones del árbol se define el tipo
--   type Posicion = [Direccion]
-- donde
--   data Direccion = D | I
--     deriving Eq
-- representa un movimiento hacia la derecha (D) o a la izquierda (I). Por
-- ejemplo, las posiciones de los elementos del ej1 son 
--   9 [] 
--   8 [I]
--   3 [I,I]
--   2 [I,D]
--   6 [D]
--   4 [D,I]
--   5 [D,D]
--
-- Definir la función
--    sustitucion :: Posicion -> a -> Arbol a -> Arbol a
-- tal que (sustitucion ds z x) es el árbol obtenido sustituyendo el
-- elemento de x en la posición ds por z. Por ejemplo,
--   ghci> sustitucion [I,D] 7 ej1
--   N 9 (N 8 (N 3 H H) (N 7 H H)) (N 6 (N 4 H H) (N 5 H H))
--   ghci> sustitucion [D,D] 7 ej1
--   N 9 (N 8 (N 3 H H) (N 2 H H)) (N 6 (N 4 H H) (N 7 H H))
--   ghci> sustitucion [I] 7 ej1
--   N 9 (N 7 (N 3 H H) (N 2 H H)) (N 6 (N 4 H H) (N 5 H H))
--   ghci> sustitucion [] 7 ej1
--   N 7 (N 8 (N 3 H H) (N 2 H H)) (N 6 (N 4 H H) (N 5 H H))
-- ---------------------------------------------------------------------

data Arbol a = H | N a (Arbol a) (Arbol a)
  deriving (Eq, Show)

ej1, ej2:: Arbol Int
ej1 = N 9 (N 8 (N 3 H H) (N 2 H H)) (N 6 (N 4 H H) (N 5 H H))
ej2 = N 9 (N 8 (N 3 H H) (N 2 H H)) H

data Direccion = D | I
  deriving Eq

type Posicion = [Direccion]

sustitucion :: Posicion  -> a -> Arbol a -> Arbol a
sustitucion (I:ds) z (N x i d) = N x (sustitucion ds z i) d
sustitucion (D:ds) z (N x i d) = N x i (sustitucion ds z d)
sustitucion []     z (N _ i d) = N z i d
sustitucion _      _ H         = H


