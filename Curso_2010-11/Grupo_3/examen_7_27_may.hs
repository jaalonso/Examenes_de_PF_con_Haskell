-- Informática (1º del Grado en Matemáticas, Grupo 3)
-- 7º examen de evaluación continua (27 de mayo de 2011)
-- ---------------------------------------------------------------------

import Data.List
import Data.Array
import Test.QuickCheck
-- import GrafoConMatrizDeAdyacencia 
import GrafoConVectorDeAdyacencia

-- ---------------------------------------------------------------------
-- Ejercicio 1. En los distintos apartados de este ejercicio
-- consideraremos relaciones binarias, representadas mediante una lista
-- de pares. Para ello, definimos el tipo de las relaciones binarias
-- sobre el tipo a. 
--    type RB a = [(a,a)]
-- Usaremos los siguientes ejemplos de relaciones
--    r1, r2, r3 :: RB Int
--    r1 = [(1,3),(3,1), (1,1), (3,3)]
--    r2 = [(1,3),(3,1)]
--    r3 = [(1,2),(1,4),(3,3),(2,1),(4,2)]
-- 
--  Definir la función 
--    universo :: Eq a => RB a -> [a] 
-- tal que (universo r) es la lista de elementos de la relación r. Por
-- ejemplo, 
--     universo r1 == [1,3]
--     universo r3 == [1,2,3,4]
-- ---------------------------------------------------------------------

type RB a = [(a,a)]

r1, r2, r3 :: RB Int
r1 = [(1,3),(3,1), (1,1), (3,3)]
r2 = [(1,3),(3,1)]
r3 = [(1,2),(1,4),(3,3),(2,1),(4,2)]

-- 1ª definición:
universo :: Eq a => RB a -> [a]
universo r = nub (l1 ++ l2) 
    where l1 = map fst r
          l2 = map snd r

-- 2ª definición:
universo2 :: Eq a => RB a -> [a]
universo2 r = nub (concat [[x,y] | (x,y) <- r])

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función 
--    reflexiva:: RB Int -> Bool
-- tal que (reflexiva r) se verifica si r es una relación reflexiva en
-- su universo. Por ejemplo, 
--    reflexiva r1 == True
--    reflexiva r2 == False
-- ---------------------------------------------------------------------

reflexiva:: RB Int -> Bool
reflexiva r = and [(x,x) `elem` r | x <- universo r]

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Dadas dos relaciones binarias R y S, la composición es
-- la relación R o S = {(a,c) | existe b tal que aRb y bRc}.
-- 
-- Definir la función 
--    compRB:: RB Int -> RB Int -> RB Int
-- tal que (compRB r1 r2) es la composición de las relaciones r1 y r2.
-- Por ejemplo, 
--   compRB r1 r3 == [(1,3),(3,2),(3,4),(1,2),(1,4),(3,3)]
--   compRB r3 r1 == [(3,1),(3,3),(2,3),(2,1)]
-- ---------------------------------------------------------------------

-- 1ª definición:
compRB:: RB Int -> RB Int -> RB Int
compRB r s = [(x,z) | (x,y) <- r, (y',z) <- s, y == y']

-- 2ª definición:
compRB2:: RB Int -> RB Int -> RB Int
compRB2 [] _        = []
compRB2 ((x,y):r) s = compPar (x,y) s ++ compRB2 r s

-- (compPar p r) es la relación obtenida componiendo el par p con la
-- relación binaria r. Por ejemplo,
--    compPar (5,1) r1  ==  [(5,3),(5,1)]
compPar:: (Int,Int) -> RB Int -> RB Int
compPar _ []            = []
compPar (x,y) ((z,t):r) | y == z    = (x,t) : compPar (x,y) r
                        | otherwise = compPar (x,y) r

-- 3ª definición:
compRB3:: RB Int -> RB Int -> RB Int
compRB3 r1 r2 = [(x,z) | x <- universo r1, z <- universo r2,
                         interRelacionados x z r1 r2]

-- (interRelacionados x z r s) se verifica si existe un y tal que (x,y)
-- está en r e (y,z) está en s. Por ejemplo.
--    interRelacionados 3 4 r1 r3  ==  True
interRelacionados :: Int -> Int -> RB Int -> RB Int -> Bool
interRelacionados x z r s =
    not (null [y | y<-universo r, (x,y) p `elem` r, (y,z) `elem` s])

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Definir la función 
--    transitiva:: RB Int -> Bool
-- tal que (transitiva r) se verifica si r es una relación
-- transitiva. Por ejemplo, 
--    transitiva r1 == True
--    transitiva r2 == False
-- ---------------------------------------------------------------------

-- 1ª solución:
transitiva :: RB Int -> Bool
transitiva r = and [(x,z) `elem` r | (x,y) <- r, (y',z) <- r, y == y'] 

-- 2ª solución:
transitiva2 :: RB Int -> Bool
transitiva2 [] = True
transitiva2 r  = and [trans par r | par <- r] 
    where trans (x,y) r = and [(x,v) `elem` r | (u,v) <- r, u == y ]

-- 3ª solución (usando la composición de relaciones):
transitiva3 :: RB Int -> Bool
transitiva3 r = contenida r (compRB r r)
    where contenida [] _      = True
          contenida (x:xs) ys = elem x ys && contenida  xs ys

-- 4ª solución:
transitiva4 :: RB Int -> Bool
transitiva4 = not . noTransitiva

-- (noTransitiva r) se verifica si r no es transitiva; es decir, si
-- existe un (x,y), (y,z) en r tales que (x,z) no está en r. 
noTransitiva :: RB Int -> Bool
noTransitiva r = 
    not (null [(x,y,z) | (x,y,z) <- ls, 
                         (x,y) `elem` r , (y,z) `elem` r, 
                         (x,z) `notElem` r])
    where l = universo r
          ls = [(x,y,z) | x <- l, y <- l, z <- l, x/=y, y /= z]

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Consideremos un grafo G = (V,E), donde V es un
-- conjunto finito de nodos ordenados y E es un conjunto de arcos. En un
-- grafo, la anchura de un nodo es el máximo de los valores absolutos de
-- la diferencia entre el valor del nodo y los de sus adyacentes; y la
-- anchura del grafo es la máxima anchura de sus nodos. Por ejemplo, en
-- el grafo  
--    g :: Grafo Int Int
--    g = creaGrafo ND (1,5) [(1,2,1),(1,3,1),(1,5,1),
--                            (2,4,1),(2,5,1),
--                            (3,4,1),(3,5,1),
--                            (4,5,1)]
-- su anchura es 4 y el nodo de máxima anchura es el 5.
-- 
-- Definir la función 
--    anchura :: Grafo Int Int -> Int
-- tal que (anchuraG g) es la anchura del grafo g. Por ejemplo,
--    anchura g  ==  4
-- ---------------------------------------------------------------------

g :: Grafo Int Int
g = creaGrafo ND (1,5) [(1,2,1),(1,3,1),(1,5,1),
                        (2,4,1),(2,5,1),
                        (3,4,1),(3,5,1),
                        (4,5,1)]

anchura :: Grafo Int Int -> Int
anchura g = maximum [anchuraN g x | x <- nodos g]

-- (anchuraN g x) es la anchura del nodo x en el grafo g. Por ejemplo, 
--    anchuraN g 1  ==  4
--    anchuraN g 2  ==  3
--    anchuraN g 4  ==  2
--    anchuraN g 5  ==  4
anchuraN :: Grafo Int Int -> Int -> Int
anchuraN g x = maximum (0 : [abs (x-v) | v <- adyacentes g x])

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Comprobar experimentalmente que la anchura del grafo
-- grafo cíclico de orden n es n-1.
-- ---------------------------------------------------------------------

-- La conjetura
conjetura :: Int -> Bool
conjetura n = anchura (grafoCiclo n) == n-1

-- (grafoCiclo n) es el grafo cíclico de orden n. Por ejemplo,
--    ghci> grafoCiclo 4
--    G ND (array (1,4) [(1,[(4,0),(2,0)]),(2,[(1,0),(3,0)]),
--                       (3,[(2,0),(4,0)]),(4,[(3,0),(1,0)])])
grafoCiclo :: Int -> Grafo Int Int
grafoCiclo n = creaGrafo ND (1,n) xs
    where xs = [(x,x+1,0) | x <- [1..n-1]] ++ [(n,1,0)]

-- La comprobación es
--    ghci> and [conjetura n | n <- [2..10]]
--    True

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Se dice que una matriz es booleana si sus elementos
-- son los valores booleanos: True, False.
-- 
-- Definir la función 
--    sumaB :: Bool -> Bool -> Bool 
-- tal que (sumaB x y) es falso si y sólo si ambos argumentos son
-- falsos. 
-- ---------------------------------------------------------------------

sumaB :: Bool -> Bool -> Bool 
sumaB = (||)

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función 
--    prodB :: Bool -> Bool -> Bool 
-- tal que (prodB x y) es verdadero si y sólo si ambos argumentos son
-- verdaderos. 
-- ---------------------------------------------------------------------

prodB :: Bool -> Bool -> Bool 
prodB = (&&)

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. En los siguientes apartados usaremos los tipos
-- definidos a continuación: 
-- * Los vectores son tablas cuyos índices son números naturales. 
--      type Vector a = Array Int a
-- * Las matrices son tablas cuyos índices son pares de números
--   naturales. 
--      type Matriz a = Array (Int,Int) a
-- En los ejemplos se usarán las siguientes matrices:
--    m1, m2 :: Matriz Bool
--    m1 = array ((1,1),(3,3)) [((1,1),True), ((1,2),False),((1,3),True),
--                              ((2,1),False),((2,2),False),((2,3),False),
--                              ((3,1),True), ((3,2),False),((3,3),True)] 
--    m2 = array ((1,1),(3,3)) [((1,1),False),((1,2),False),((1,3),True), 
--                              ((2,1),False),((2,2),False),((2,3),False),
--                              ((3,1),True), ((3,2),False),((3,3),False)]
--    
-- También se usan las siguientes funciones definidas en las relaciones
-- de ejercicios.
--    numFilas :: Matriz a -> Int
--    numFilas = fst . snd . bounds
--    
--    numColumnas:: Matriz a -> Int
--    numColumnas = snd . snd . bounds
--    
--    filaMat :: Int -> Matriz a -> Vector a
--    filaMat i p = array (1,n) [(j,p!(i,j)) | j <- [1..n]]
--        where n = numColumnas p
--    
--    columnaMat :: Int -> Matriz a -> Vector a
--    columnaMat j p = array (1,m) [(i,p!(i,j)) | i <- [1..m]]
--        where m = numFilas p
-- 
-- Definir la función 
--    prodMatricesB :: Matriz Bool -> Matriz Bool -> Matriz Bool 
-- tal que (prodMatricesB p q) es el producto de las matrices booleanas
-- p y q, usando la suma y el producto de booleanos, definidos
-- previamente. Por ejemplo, 
--    ghci> prodMatricesB m1 m2
--    array ((1,1),(3,3)) [((1,1),True), ((1,2),False),((1,3),True),
--                         ((2,1),False),((2,2),False),((2,3),False),
--                         ((3,1),True), ((3,2),False),((3,3),True)]
-- ---------------------------------------------------------------------

type Vector a = Array Int a

type Matriz a = Array (Int,Int) a

m1, m2 :: Matriz Bool
m1 = array ((1,1),(3,3)) [((1,1),True), ((1,2),False),((1,3),True),
                          ((2,1),False),((2,2),False),((2,3),False),
                          ((3,1),True), ((3,2),False),((3,3),True)] 
m2 = array ((1,1),(3,3)) [((1,1),False),((1,2),False),((1,3),True), 
                          ((2,1),False),((2,2),False),((2,3),False),
                          ((3,1),True), ((3,2),False),((3,3),False)]

numFilas :: Matriz a -> Int
numFilas = fst . snd . bounds

numColumnas:: Matriz a -> Int
numColumnas = snd . snd . bounds

filaMat :: Int -> Matriz a -> Vector a
filaMat i p = array (1,n) [(j,p!(i,j)) | j <- [1..n]]
    where n = numColumnas p

columnaMat :: Int -> Matriz a -> Vector a
columnaMat j p = array (1,m) [(i,p!(i,j)) | i <- [1..m]]
    where m = numFilas p

prodMatricesB:: Matriz Bool -> Matriz Bool -> Matriz Bool
prodMatricesB p q = 
    array ((1,1),(m,n))
          [((i,j), prodEscalarB (filaMat i p) (columnaMat j q)) |
           i <- [1..m], j <- [1..n]]
    where m = numFilas p
          n = numColumnas q

-- (prodEscalarB v1 v2) es el producto escalar booleano de los vectores
-- v1 y v2.
prodEscalarB :: Vector Bool -> Vector Bool -> Bool
prodEscalarB v1 v2 = 
    sumB [prodB i j | (i,j) <- zip (elems v1) (elems v2)]
    where sumB = foldr sumaB False

-- ---------------------------------------------------------------------
-- Ejercicio 3.4. Se considera la siguiente relación de orden entre
-- matrices: p es menor o igual que q si para toda posición (i,j), el
-- elemento de p en (i,j) es menor o igual que el elemento de q en la
-- posición (i,j). Definir la función 
--    menorMatricesB :: Ord a => Matriz a -> Matriz a -> Bool
-- tal que (menorMatricesB p q) se verifica si p es menor o igual que
-- q.
--    menorMatricesB m1 m2  ==  False
--    menorMatricesB m2 m1  ==  True
-- ---------------------------------------------------------------------

menorMatricesB :: Ord a => Matriz a -> Matriz a -> Bool
menorMatricesB p q = 
    and [p!(i,j) <= q!(i,j) | i <- [1..m], j <-[1..n]]
        where m = numFilas p
              n = numColumnas p

-- ---------------------------------------------------------------------
-- Ejercicio 3.5. Dada una relación r sobre un conjunto de números
-- naturales mayores que 0, la matriz asociada a r es una matriz
-- booleana p, tal que p_ij = True si y sólo si i está relacionado con j
-- mediante la relación r. Definir la función  
--    matrizRB:: RB Int -> Matriz Bool
-- tal que (matrizRB r) es la matriz booleana asociada a r. Por ejemplo, 
--    ghci> matrizRB r1
--    array ((1,1),(3,3)) [((1,1),True),((1,2),False),((1,3),True),
--                         ((2,1),False),((2,2),False),((2,3),False),
--                         ((3,1),True),((3,2),False),((3,3),True)]
--    ghci> matrizRB r2
--    array ((1,1),(3,3)) [((1,1),False),((1,2),False),((1,3),True),
--                         ((2,1),False),((2,2),False),((2,3),False),
--                         ((3,1),True),((3,2),False),((3,3),False)]
-- 
-- Nota: Construir una matriz booleana cuadrada, de dimensión nxn,
-- siendo n el máximo de los elementos del universo de r. 
-- ---------------------------------------------------------------------

matrizRB:: RB Int -> Matriz Bool
matrizRB r = array ((1,1),(n,n)) [((i,j),f (i,j)) | i <- [1..n],j<-[1..n]]
    where n       = maximum (universo r)
          f (i,j) = (i,j) `elem` r 

-- ---------------------------------------------------------------------
-- Ejercicio 3.5. Se verifica la siguiente propiedad: r es una relación
-- transitiva si y sólo si M^2 <= M, siendo M la matriz booleana
-- asoociada a r, y M^2 el resultado de multiplicar M por M mediante
-- el producto booleano. Definir la función 
--    transitivaB :: RB Int -> Bool
-- tal que (transitivaB r) se verifica si r es una relación
-- transitiva. Por ejemplo, 
--    transitivaB r1 == True
--    transitivaB r2 == False 
-- ---------------------------------------------------------------------

transitivaB :: RB Int -> Bool
transitivaB r =  menorMatricesB q p
    where p = matrizRB r
          q = prodMatricesB p p
