-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 6º examen de evaluación continua (25 de mayo de 2016)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Matrix
import Data.Numbers.Primes
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    seccion :: Eq a => [a] -> [a]
-- tal que (seccion xs) es la mayor sección inicial de xs que no
-- contiene ningún elemento repetido. Por ejemplo: 
--    seccion [1,2,3,2,4,5]                      == [1,2,3] 
--    seccion "caceres"                          == "ca"
--    length (seccion ([1..7531] ++ [1..10^9]))  ==  7531
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

seccion1 :: Eq a => [a] -> [a]
seccion1 = last . filter (\ys -> nub ys == ys) . inits

-- 2ª solución
-- ===========

seccion2 :: Eq a => [a] -> [a]
seccion2 xs = aux xs []
    where aux [] ys = reverse ys
          aux (x:xs) ys | x `elem` ys = reverse ys
                        | otherwise   = aux xs (x:ys)

-- Comparación de eficiencia
-- =========================

--    ghci> last (seccion1 [1..10^3])
--    1000
--    (6.19 secs, 59,174,640 bytes)
--    ghci> last (seccion2 [1..10^3])
--    1000
--    (0.04 secs, 0 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Un número n es especial si al unir las cifras de sus
-- factores primos, se obtienen exactamente las cifras de n, aunque
-- puede ser en otro  orden. Por ejemplo, 1255 es especial, pues los
-- factores primos de 1255 son 5 y 251. 
--
-- Definir la función 
--    esEspecial :: Integer -> Bool
-- tal que (esEspecial n) se verifica si un número n es especial. Por
-- ejemplo, 
--    esEspecial 1255 == True
--    esEspecial 125  == False
--    esEspecial 132  == False
-- ---------------------------------------------------------------------

esEspecial :: Integer -> Bool
esEspecial n = 
    sort (show n) == sort (concatMap show (primeFactors n))

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Comprobar con QuickCheck que todo número primo es
-- especial.  
-- ---------------------------------------------------------------------

-- La propiedad es
prop_primos:: Integer -> Property
prop_primos n = 
    isPrime (abs n) ==> esEspecial (abs n)

-- La comprobación es
--    ghci> quickCheck prop_primos
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Calcular los 5 primeros números especiales que no son
-- primos.
-- ---------------------------------------------------------------------

-- El cálculo es
--    ghci> take 5 [n | n <- [2..], esEspecial n, not (isPrime n)]
--    [1255,12955,17482,25105,100255]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Los árboles binarios se pueden representar mediante el
-- tipo Arbol definido por 
--    data Arbol a = H a 
--                 | N a (Arbol a) (Arbol a)
--                 deriving Show
-- Por ejemplo, el árbol
--         "B"
--         / \ 
--        /   \
--       /     \
--     "B"     "A"
--     / \     / \
--   "A" "B" "C" "C" 
-- se puede definir por 
--    ej1 :: Arbol String
--    ej1 = N "B" (N "B" (H "A") (H "B")) (N "A" (H "C") (H "C"))
--
-- Definir la función
--    enumeraArbol :: Arbol t -> Arbol Int
-- tal que (enumeraArbol a) es el árbol obtenido numerando las hojas y
-- los nodos de a desde la hoja izquierda hasta la raíz. Por ejemplo,
--    ghci> enumeraArbol ej1
--    N 6 (N 2 (H 0) (H 1)) (N 5 (H 3) (H 4))
-- Gráficamente, 
--          6 
--         / \ 
--        /   \
--       /     \
--      2       5 
--     / \     / \
--    0   1   3   4  
-- ---------------------------------------------------------------------

data Arbol a = H a 
             | N a (Arbol a) (Arbol a)
             deriving (Show, Eq)

ej1 :: Arbol String
ej1 = N "B" (N "B" (H "A") (H "B")) (N "A" (H "C") (H "C"))

enumeraArbol1 :: Arbol t -> Arbol Int
enumeraArbol1 a = fst (aux a 0)
    where aux :: Arbol a -> Int -> (Arbol Int,Int)
          aux (H _) n     = (H n, n+1)
          aux (N x i d) n = (N n2 i' d', 1+n2)
              where (i', n1) = aux i n
                    (d', n2) = aux d n1
                
-- ---------------------------------------------------------------------
-- Ejercicio 4. El buscaminas es un juego cuyo objetivo es despejar un
-- campo de minas sin detonar ninguna. 
-- 
-- El campo de minas se representa mediante un cuadrado con NxN
-- casillas. Algunas casillas tienen un número, este número indica las
-- minas que hay en todas las casillas vecinas. Cada casilla tiene como
-- máximo 8 vecinas. Por ejemplo, el campo 4x4 de la izquierda
-- contiene dos minas, cada una representada por el número 9, y a la 
-- derecha se muestra el campo obtenido anotando las minas vecinas de
-- cada casilla
--    9 0 0 0       9 1 0 0
--    0 0 0 0       2 2 1 0
--    0 9 0 0       1 9 1 0
--    0 0 0 0       1 1 1 0
-- de la misma forma, la anotación del siguiente a la izquierda es el de
-- la derecha 
--    9 9 0 0 0     9 9 1 0 0
--    0 0 0 0 0     3 3 2 0 0
--    0 9 0 0 0     1 9 1 0 0
--
-- Utilizando la librería Data.Matrix, los campos de minas se
-- representan mediante matrices: 
--    type Campo = Matrix Int
-- Por ejemplo, los anteriores campos de la izquierda se definen por
--    ejCampo1, ejCampo2 :: Campo
--    ejCampo1 = fromLists [[9,0,0,0],
--                          [0,0,0,0], 
--                          [0,9,0,0], 
--                          [0,0,0,0]]
--    ejCampo2 = fromLists [[9,9,0,0,0],
--                          [0,0,0,0,0],
--                          [0,9,0,0,0]]
-- 
-- Definir la función
--    buscaminas :: Campo -> Campo
-- tal que (buscaminas c) es el campo obtenido anotando las minas
-- vecinas de cada casilla. Por ejemplo,
--    ghci> buscaminas ejCampo1
--    ( 9 1 0 0 )
--    ( 2 2 1 0 )
--    ( 1 9 1 0 )
--    ( 1 1 1 0 )
--    
--    ghci> buscaminas ejCampo2
--    ( 9 9 1 0 0 )
--    ( 3 3 2 0 0 )
--    ( 1 9 1 0 0 )
-- ---------------------------------------------------------------------

type Campo   = Matrix Int
type Casilla = (Int,Int)

ejCampo1, ejCampo2 :: Campo
ejCampo1 = fromLists [[9,0,0,0],
                      [0,0,0,0], 
                      [0,9,0,0], 
                      [0,0,0,0]]
ejCampo2 = fromLists [[9,9,0,0,0],
                      [0,0,0,0,0],
                      [0,9,0,0,0]]

-- 1ª solución
-- ===========

buscaminas1 :: Campo -> Campo
buscaminas1 c = matrix m n (\(i,j) -> minas c (i,j))
    where m = nrows c
          n = ncols c

-- (minas c (i,j)) es el número de minas en las casillas vecinas de la
-- (i,j) en el campo de mina c y es 9 si en (i,j) hay una mina. Por
-- ejemplo,
--    minas ejCampo (1,1)  ==  9
--    minas ejCampo (1,2)  ==  1
--    minas ejCampo (1,3)  ==  0
--    minas ejCampo (2,1)  ==  2
minas :: Campo -> Casilla -> Int
minas c (i,j) 
    | c!(i,j) == 9 = 9
    | otherwise    = length (filter (==9) 
                            [c!(x,y) | (x,y) <- vecinas m n (i,j)])
    where m = nrows c
          n = ncols c

-- (vecinas m n (i,j)) es la lista de las casillas vecinas de la (i,j) en
-- un campo de dimensiones mxn. Por ejemplo,
--    vecinas 4 (1,1)  ==  [(1,2),(2,1),(2,2)]
--    vecinas 4 (1,2)  ==  [(1,1),(1,3),(2,1),(2,2),(2,3)]
--    vecinas 4 (2,3)  ==  [(1,2),(1,3),(1,4),(2,2),(2,4),(3,2),(3,3),(3,4)]
vecinas :: Int -> Int -> Casilla -> [Casilla]
vecinas m n (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)],
                             b <- [max 1 (j-1)..min n (j+1)],
                             (a,b) /= (i,j)]

-- 2ª solución
-- ===========

buscaminas2 :: Campo -> Campo
buscaminas2 c = matrix m n (\(i,j) -> minas (i,j))
    where m = nrows c
          n = ncols c
          minas :: Casilla -> Int
          minas (i,j) 
              | c!(i,j) == 9 = 9
              | otherwise    = length (filter (==9) 
                                      [c!(x,y) | (x,y) <- vecinas (i,j)])
          vecinas :: Casilla -> [Casilla]
          vecinas (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)],
                                   b <- [max 1 (j-1)..min n (j+1)],
                                   (a,b) /= (i,j)]
