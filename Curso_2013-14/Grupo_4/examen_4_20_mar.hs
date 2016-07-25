-- Informática (1º del Grado en Matemáticas y en Matemáticas y Estadística)
-- 4º examen de evaluación continua (20 de marzo de 2014)
-- =====================================================================

-- ---------------------------------------------------------------------
-- Ejercicio 1. Se consideran los árboles binarios representados
-- mediante el tipo Arbol definido por 
--    data Arbol = Hoja Int
--               | Nodo Int Arbol Arbol
--               deriving Show
-- Por ejemplo, el árbol
--         1
--        / \
--       /   \
--      3     2
--     / \   / \
--    5   4 6   7
-- se puede representar por
--    Nodo 1 (Nodo 3 (Hoja 5) (Hoja 4)) (Nodo 2 (Hoja 6) (Hoja 7))
-- En los ejemplos se usarán los árboles definidos por
--    ej1 = Nodo 1 (Nodo 3 (Hoja 5) (Hoja 4)) (Nodo 2 (Hoja 6) (Hoja 7))
--    ej2 = Nodo 3 (Hoja 1) (Hoja 4)
--    ej3 = Nodo 2 (Hoja 3) (Hoja 5)
--    ej4 = Nodo 1 (Hoja 2) (Nodo 2 (Hoja 3) (Hoja 3))
--    ej5 = Nodo 1 (Nodo 2 (Hoja 3) (Hoja 5)) (Hoja 2)
-- 
-- Las capas de un árbol binario son las listas de elementos que están a
-- la misma profundidad. Por ejemplo, las capas del árbol 
--         1
--        / \
--       /   \
--      3     2
--     / \   / \
--    5   4 6   7
-- son: [1], [3,2] y [5,4,6,7]
--
-- Definir la función 
--    capas :: Arbol -> [[Int]]
-- tal que (capas a) es la lista de las capas de dicho árbol ordenadas
-- según la profunidad. Por ejemplo, 
--    capas ej1  ==  [[1],[3,2],[5,4,6,7]]
--    capas ej2  ==  [[3],[1,4]]
--    capas ej3  ==  [[2],[3,5]]
--    capas ej4  ==  [[1],[2,2],[3,3]]
--    capas ej5  ==  [[1],[2,2],[3,5]]
-- ---------------------------------------------------------------------

data Arbol = Hoja Int
           | Nodo Int Arbol Arbol
           deriving Show

ej1 = Nodo 1 (Nodo 3 (Hoja 5) (Hoja 4)) (Nodo 2 (Hoja 6) (Hoja 7))
ej2 = Nodo 3 (Hoja 1) (Hoja 4)
ej3 = Nodo 2 (Hoja 3) (Hoja 5)
ej4 = Nodo 1 (Hoja 2) (Nodo 2 (Hoja 3) (Hoja 3))
ej5 = Nodo 1 (Nodo 2 (Hoja 3) (Hoja 5)) (Hoja 2)

capas :: Arbol -> [[Int]]
capas (Hoja n) = [[n]]
capas (Nodo n i d) = [n] : union (capas i) (capas d)

-- (union xss yss) es la lista obtenida concatenando los
-- correspondientes elementos de xss e yss. Por ejemplo,
--    union [[3,4],[2]] [[5],[7,6,8]]  ==  [[3,4,5],[2,7,6,8]]
--    union [[3,4]]     [[5],[7,6,8]]  ==  [[3,4,5],[7,6,8]]
--    union [[3,4],[2]] [[5]]          ==  [[3,4,5],[2]]
union :: [[a]] -> [[a]] -> [[a]]
union [] yss = yss
union xss [] = xss
union (xs:xss) (ys:yss) = (xs ++ ys) : union xss yss

-- ---------------------------------------------------------------------
-- Ejercicio 2. Un árbol es subárbol de otro si se puede establecer una
-- correspondencia de los nodos del primero con otros mayores o iguales
-- en el segundo, de forma que se respeten las relaciones de
-- descendencia. Este concepto se resume en varias situaciones posibles: 
-- * El primer árbol es subárbol del hijo izquierdo del segundo
--   árbol. De esta forma ej2 es subárbol de ej1.
-- * El primer árbol es subárbol del hijo derecho del segundo árbol. De
--   esta forma ej3 es subárbol de ej1.
-- * La raíz del primer árbol es menor o igual que la del segundo, el
--   hijo izquierdo del primer árbol es subárbol del hijo izquierdo del
--   segundo y el hijo derecho del primer árbol es subárbol del hijo
--   derecho del segundo. De esta forma ej4 es subárbol de ej1.
--
-- Definir la función 
--    subarbol :: Arbol -> Arbol -> Bool
-- tal que (subarbol a1 a2) se verifica si a1 es subárbol de a2. Por
-- ejemplo, 
--    subarbol ej2 ej1  ==  True
--    subarbol ej3 ej1  ==  True
--    subarbol ej4 ej1  ==  True
--    subarbol ej5 ej1  ==  False
-- ---------------------------------------------------------------------

subarbol :: Arbol -> Arbol -> Bool
subarbol (Hoja n) (Hoja m) =
    n <= m
subarbol (Hoja n) (Nodo m i d) =
    n <= m || subarbol (Hoja n) i || subarbol (Hoja n) d
subarbol (Nodo _ _ _) (Hoja _) =
    False
subarbol (Nodo n i1 d1) (Nodo m i2 d2) =
    subarbol (Nodo n i1 d1) i2 ||
    subarbol (Nodo n i1 d1) d2 ||
    n <= m && (subarbol i1 i2) && (subarbol d1 d2)

-- ---------------------------------------------------------------------
-- Ejercicio 3.1 (1.2 puntos): Definir la función 
--    intercalaRep :: Eq a => a -> [a] -> [[a]]
-- tal que (intercalaRep x ys), es la lista de las listas obtenidas
-- intercalando x entre los elementos de ys, hasta la primera ocurrencia
-- del elemento x en ys. Por ejemplo,
--    intercalaRep 1 []       ==  [[1]]
--    intercalaRep 1 [1]      ==  [[1,1]]
--    intercalaRep 1 [2]      ==  [[1,2],[2,1]]
--    intercalaRep 1 [1,1]    ==  [[1,1,1]]
--    intercalaRep 1 [1,2]    ==  [[1,1,2]]
--    intercalaRep 1 [2,1]    ==  [[1,2,1],[2,1,1]]
--    intercalaRep 1 [1,2,1]  ==  [[1,1,2,1]]
--    intercalaRep 1 [2,1,1]  ==  [[1,2,1,1],[2,1,1,1]]
--    intercalaRep 1 [1,1,2]  ==  [[1,1,1,2]]
--    intercalaRep 1 [1,2,2]  ==  [[1,1,2,2]]
--    intercalaRep 1 [2,1,2]  ==  [[1,2,1,2],[2,1,1,2]]
--    intercalaRep 1 [2,2,1]  ==  [[1,2,2,1],[2,1,2,1],[2,2,1,1]]
-- ---------------------------------------------------------------------

-- 1ª definición (con map):
intercalaRep :: Eq a => a -> [a] -> [[a]]
intercalaRep x [] = [[x]]
intercalaRep x (y:ys)
    | x == y    = [x:y:ys]
    | otherwise = (x:y:ys) : (map (y:) (intercalaRep x ys))

-- 2ª definición (sin map):
intercalaRep2 :: Eq a => a -> [a] -> [[a]]
intercalaRep2 x [] = [[x]]
intercalaRep2 x (y:ys)
    | x == y    = [x:y:ys]
    | otherwise = (x:y:ys) : [y:zs | zs <- intercalaRep2 x ys]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2.Definir la función
--    permutacionesRep :: Eq a => [a] -> [[a]]
-- tal que (permutacionesRep xs) es la lista (sin elementos repetidos)
-- de todas las permutaciones con repetición de la lista xs. Por
-- ejemplo, 
--    permutacionesRep []         ==  [[]]
--    permutacionesRep [1]        ==  [[1]]
--    permutacionesRep [1,1]      ==  [[1,1]]
--    permutacionesRep [1,2]      ==  [[1,2],[2,1]]
--    permutacionesRep [1,2,1]    ==  [[1,2,1],[2,1,1],[1,1,2]]
--    permutacionesRep [1,1,2]    ==  [[1,1,2],[1,2,1],[2,1,1]]
--    permutacionesRep [2,1,1]    ==  [[2,1,1],[1,2,1],[1,1,2]]
--    permutacionesRep [1,1,1]    ==  [[1,1,1]]
--    permutacionesRep [1,1,2,2]  ==  [[1,1,2,2],[1,2,1,2],[2,1,1,2],
--                                     [1,2,2,1],[2,1,2,1],[2,2,1,1]]
-- ---------------------------------------------------------------------

permutacionesRep :: Eq a => [a] -> [[a]]
permutacionesRep []     = [[]]
permutacionesRep [x]    = [[x]]
permutacionesRep (x:xs) =
    concat (map (intercalaRep x) (permutacionesRep xs))

-- ---------------------------------------------------------------------
-- Ejercicio 4. Un montón de barriles se construye apilando unos encima
-- de otros por capas, de forma que en cada capa todos los barriles
-- están apoyados sobre dos de la capa inferior y todos los barriles de
-- una misma capa están pegados unos a otros. Por ejemplo, los
-- siguientes montones son válidos:
--             _          _   _                   _
--            / \        / \ / \                 / \
--           _\_/_      _\_/_\_/_   _       _   _\_/_   _
--          / \ / \    / \ / \ / \ / \     / \ / \ / \ / \
--          \_/ \_/    \_/ \_/ \_/ \_/     \_/ \_/ \_/ \_/
--
-- y los siguientes no son válidos:
--         _   _          _       _               _   _
--        / \ / \        / \     / \             / \ / \
--        \_/_\_/_      _\_/_   _\_/_       _   _\_/_\_/
--          / \ / \    / \ / \ / \ / \     / \ / \ / \
--          \_/ \_/    \_/ \_/ \_/ \_/     \_/ \_/ \_/
--
-- Se puede comprobar que el número de formas distintas de construir
-- montones con n barriles en la base M_n viene dado por la siguiente
-- fórmula: 
--
--              (n-1)
--             -------
--              \
--               \
--   M_n = 1 +    )    (n-j) * M_j
--               /
--              /
--             -------
--              j = 1
--
-- Definir la función 
--    montones :: Integer -> Integer
-- tal que (montones n) es el número de formas distintas de construir
-- montones con n barriles en la base. Por ejemplo,
--    montones 1   ==  1
--    montones 10  ==  4181
--    montones 20  ==  63245986
--    montones 30  ==  956722026041
--
-- Calcular el número de formas distintas de construir montones con 50
-- barriles en la base.
-- ---------------------------------------------------------------------

montones :: Integer -> Integer
montones 1 = 1
montones n = 1 + sum [(n-j)*(montones j) | j <- [1..n-1]]

-- 2ª definición, a partir de la siguiente observación
--    M(1) = 1                          = 1
--    M(2) = 1 + M(1)                   = M(1) + M(1)   
--    M(3) = 1 + 2*M(1) + M(2)          = M(2) + (M(1) + M(2))
--    M(4) = 1 + 3*M(1) + 2*M(2) + M(3) = M(3) + (M(1) + M(2) + M(3))
montones2 :: Int -> Integer
montones2 n = montonesSuc !! (n-1)

montonesSuc :: [Integer]
montonesSuc = 1 : zipWith (+) montonesSuc (scanl1 (+) montonesSuc)

-- 3ª definición
montones3 :: Integer -> Integer
montones3 0 = 0
montones3 n = head (montonesAcc [] n)

montonesAcc :: [Integer] -> Integer -> [Integer]
montonesAcc ms 0 = ms
montonesAcc ms n =
    montonesAcc ((1 + sum (zipWith (*) ms [1..])):ms) (n-1)

-- El cálculo es
--    ghci> montones2 50
--    218922995834555169026
