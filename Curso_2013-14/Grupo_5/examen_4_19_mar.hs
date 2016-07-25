-- Informática (1º Grado Matemáticas y doble Grado Matemáticas y Física)
-- 4º examen de evaluación continua (19 de marzo de 2014)
-- =====================================================================

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List

-- ----------------------------------------------------------------------
-- Ejercicio 1.1. Un número se dirá ordenado si sus cifras están en orden 
-- creciente. Por ejemplo, 11257 es ordenado pero 2423 no lo es.
--
-- Definir la lista 
--    ordenados :: [Integer]
-- formada  por todos los enteros ordenados. Por ejemplo,
--    ghci> take 20 (dropWhile (<30) ordenados)
--    [33,34,35,36,37,38,39,44,45,46,47,48,49,55,56,57,58,59,66,67]
-- ---------------------------------------------------------------------

ordenados :: [Integer]
ordenados = [n | n <- [1..], esOrdenado n]

-- (esOrdenado x) se verifica si el número x es ordenado. Por ejemplo,
--    esOrdenado 359  ==  True
--    esOrdenado 395  ==  False
esOrdenado :: Integer -> Bool
esOrdenado = esOrdenada . show

-- (esOrdenada xs) se verifica si la lista xs está ordenada. Por
-- ejemplo, 
--    esOrdenada [3,5,9]  ==  True
--    esOrdenada [3,9,5]  ==  False
--    esOrdenada "359"  ==  True
--    esOrdenada "395"  ==  False
esOrdenada :: Ord a => [a] -> Bool
esOrdenada (x:y:xs) = x <= y && esOrdenada (y:xs)
esOrdenada _        = True

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Calcular en qué posición de la lista aparece el número
-- 13333. 
-- ---------------------------------------------------------------------

-- El cálculo es
--    ghci> length (takeWhile (<=13333) ordenados)
--    1000

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Una lista se dirá comprimida si sus elementos
-- consecutivos han sido agrupados. Por ejemplo, la comprimida de
-- "aaabcccc" es [(3,'a'),(1,'b'),(4,'c')].
-- 
-- Definir la función    
--    comprimida :: Eq a => [a] -> [(a,Int)] 
-- tal que (comprimida xs) es la comprimida de la lista xs. Por ejemplo,
--    comprimida "aaabcccc"  ==  [(3,'a'),(1,'b'),(4,'c')]
-- ---------------------------------------------------------------------

-- 2ª definición (por recursión usando takeWhile):
comprimida :: Eq a => [a] -> [(Int,a)]
comprimida [] = []
comprimida (x:xs) = 
    (1 + length (takeWhile (==x) xs),x) : comprimida (dropWhile (==x) xs)

-- 2ª definición (por recursión sin takeWhile)
comprimida2 :: Eq a => [a] -> [(Int,a)]
comprimida2 xs = aux xs 1
    where aux (x:y:zs) n | x == y    = aux (y:zs) (n+1)
                         | otherwise = (n,x) : aux (y:zs) 1
          aux [x]      n             = [(n,x)]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función
--    expandida :: [(Int,a)] -> [a]
-- tal que (expandida ps) es la lista expandida correspondiente a ps (es
-- decir, es la lista xs tal que la comprimida de xs es ps). Por
-- ejemplo, 
--    expandida [(2,1),(3,7),(2,5),(4,7)]  ==  [1,1,7,7,7,5,5,7,7,7,7]
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión)
expandida :: [(Int,a)] -> [a]
expandida ps = concat [replicate k x | (k,x) <- ps]

-- 2ª definición (por recursión)
expandida2 :: [(Int,a)] -> [a]
expandida2 [] = []
expandida2 ((n,x):ps) = replicate n x ++ expandida2 ps

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Los árboles binarios pueden representarse mediante el
-- tipo de dato 
--    data Arbol a = Hoja a | Nodo a (Arbol a) (Arbol a)
-- Un ejemplo de árbol es
--    ejArbol = Nodo 7 (Nodo 2 (Hoja 5) (Hoja 4)) (Hoja 9) 
-- 
-- Un elemento de un árbol se dirá de nivel k si aparece en el árbol a
-- distancia k  de la raíz. 
-- 
-- Definir la función
--    nivel :: Int -> Arbol a -> [a]
-- tal que (nivel k a) es la lista de los elementos de nivel k del árbol
-- a. Por ejemplo,
--    nivel 0 ejArbol  ==  [7]
--    nivel 1 ejArbol  ==  [2,9]
--    nivel 2 ejArbol  ==  [5,4]
--    nivel 3 ejArbol  ==  []
-- ---------------------------------------------------------------------

data Arbol a = Hoja a | Nodo a (Arbol a) (Arbol a)

ejArbol = Nodo 7 (Nodo 2 (Hoja 5) (Hoja 4)) (Hoja 9) 

nivel :: Int -> Arbol a -> [a]
nivel 0 (Hoja x)     = [x]
nivel 0 (Nodo x _ _) = [x]
nivel k (Hoja _ )    = []
nivel k (Nodo _ i d) = nivel (k-1) i ++ nivel (k-1) d

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    todosDistintos :: Eq a => Arbol a -> Bool
-- tal que (todosDistintos a) se verifica si todos los elementos del
-- árbol a son distintos entre sí. Por ejemplo, 
--    todosDistintos ejArbol                                       == True
--    todosDistintos (Nodo 7 (Hoja 3) (Nodo 4 (Hoja 7) (Hoja 2)))  ==  False
-- ---------------------------------------------------------------------

todosDistintos :: Eq a => Arbol a -> Bool
todosDistintos a = xs == nub xs
    where xs = preorden a

preorden :: Arbol a -> [a]
preorden (Hoja x)     = [x]
preorden (Nodo x i d) = x : (preorden i ++ preorden d)

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. En un colisionador de partículas se disponen m placas,
-- con n celdillas cada una. Cada celdilla detecta si alguna partícula 
-- ha pasado por ella (1 si ha detectado una partícula, 0 en caso
-- contrario). El siguiente ejemplo muestra 5 placas con 9 celdillas
-- cada una: 
--    experimento:: [[Int]]
--    experimento = [[0, 0, 1, 1, 0, 1, 0, 0, 1],
--                   [0, 1, 0, 1, 0, 1, 0, 1, 0],
--                   [1, 0, 1, 0, 0, 0, 1, 0, 0],
--                   [0, 1, 0, 0, 0, 1, 0, 1, 0],
--                   [1, 0, 0, 0, 1, 0, 0, 0, 1]]
-- Se quiere reconstruir las trayectorias que han realizado las
-- partículas que  atraviesan dichas placas.
-- 
-- Una trayectoria de una partícula vendrá dada por un par, donde la
-- primera componente indica la celdilla por la que pasó en la primera
-- placa y la segunda componente será una lista que indica el camino
-- seguido en las sucesivas placas. Es decir, cada elemento de la lista
-- indicará si de una placa a la siguiente, la partícula se desvió una
-- celdilla hacia la derecha (+1), hacia la izquierda (-1) o pasó por la
-- misma celdilla (0). Por ejemplo, una trayectoria en el ejemplo
-- anterior sería: 
--        [(2,[-1,1,-1,-1])]
-- Se puede observar que es posible crear más de una trayectoria para la 
-- misma  partícula. 
-- 
-- Definir la función
--    calculaTrayectorias :: [[Int]] -> [(Int,[Int])] 
-- que devuelva una lista con todas las trayectorias posibles para un
-- experimento. Por ejemplo,
--    ghci> calculaTrayectorias experimento  
--    [(2,[-1,-1,1,-1]),  (2,[-1,1,-1,-1]), (2,[1,-1,-1,-1]), 
--     (3,[0,-1,-1,-1]), 
--     (5,[0,1,-1,-1]),   (5,[0,1,1,1]),
--     (8,[-1,-1,-1,-1]), (8,[-1,-1,1,1])]
-- ---------------------------------------------------------------------

experimento:: [[Int]]
experimento = [[0, 0, 1, 1, 0, 1, 0, 0, 1],
               [0, 1, 0, 1, 0, 1, 0, 1, 0],
               [1, 0, 1, 0, 0, 0, 1, 0, 0],
               [0, 1, 0, 0, 0, 1, 0, 1, 0],
               [1, 0, 0, 0, 1, 0, 0, 0, 1]]

calculaTrayectorias :: [[Int]] -> [(Int,[Int])]
calculaTrayectorias [] = []
calculaTrayectorias (xs:xss) = 
    [(i,ys) | (i,e) <- zip [0..] xs, e==1, ys <- posiblesTrayectorias i xss]

-- Solución 1, con recursión
posiblesTrayectorias :: Int -> [[Int]] -> [[Int]]
posiblesTrayectorias i [] = [[]]
posiblesTrayectorias i (xs:xss) = 
    [desp:ys | desp <- [-1,0,1], 
               i+desp >= 0 && i+desp < length xs, 
               xs!!(i+desp) == 1,
               ys <- posiblesTrayectorias (i+desp) xss]

-- Solución 2, con recursión con acumulador
posiblesTrayectorias' i xss = posiblesTrayectoriasRecAcum i [[]] xss

posiblesTrayectoriasRecAcum i yss [] = yss
posiblesTrayectoriasRecAcum i yss (xs:xss) = 
  concat[posiblesTrayectoriasRecAcum idx [ys++[idx-i] | ys <- yss] xss
        | idx <- [i-1..i+1], idx >= 0 && idx < length xs, xs!!idx == 1]

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Consideraremos una trayectoria válida si no cambia de
-- dirección. Esto es, si una partícula tiene una trayectoria hacia la
-- izquierda, no podrá desviarse a la derecha posteriormenete y vice versa. 
-- 
-- Definir la función
--    trayectoriasValidas :: [(Int,[Int])] -> [(Int,[Int])]
-- tal que (trayectoriasValidas xs) es la lista de las trayectorias de
-- xs que son válidas. Por ejemplo,
--    ghci> trayectoriasValidas (calculaTrayectorias experimento) 
--    [(3,[0,-1,-1,-1]),(5,[0,1,1,1]),(8,[-1,-1,-1,-1])]
-- ---------------------------------------------------------------------

trayectoriasValidas :: [(Int,[Int])] -> [(Int,[Int])]
trayectoriasValidas xss = [(i,xs) | (i,xs) <- xss, trayectoriaValida3 xs]

-- 1ª definición (con recursión)
trayectoriaValida :: [Int] -> Bool
trayectoriaValida [] = True
trayectoriaValida (x:xs) | x == 0  = trayectoriaValida xs
                         | x == -1 = notElem 1 xs
                         | x == 1  = notElem (-1) xs

-- 2ª definición (con operaciones lógicas)
trayectoriaValida2 :: [Int] -> Bool
trayectoriaValida2 xs = (dirDer `xor` dirIzq) || dirRec
    where xor x y = x/=y
          dirDer  = elem 1 xs
          dirIzq  = elem (-1) xs
          dirRec  = elem 0 xs && not dirDer && not dirIzq

-- 3ª definición 
trayectoriaValida3 :: [Int] -> Bool
trayectoriaValida3 xs = not (1 `elem` xs && (-1) `elem` xs) 
