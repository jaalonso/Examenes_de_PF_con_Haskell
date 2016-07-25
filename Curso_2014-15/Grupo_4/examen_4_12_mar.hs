-- Informática (1º del Grado en Matemáticas)
-- 4º examen de evaluación continua (12 de marzo de 2015)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List 
import Data.Array
import I1M.PolOperaciones

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. En este ejercicio, representemos las fracciones
-- mediante pares de números de enteros.
-- 
-- Definir la función 
--    fracciones :: Integer -> [(Integer,Integer)]
-- tal que (fracciones n) es la lista con las fracciones propias
-- positivas, con denominador menor o igual que n. Por ejemplo, 
--    fracciones 4 == [(1,2),(1,3),(2,3),(1,4),(3,4)]
--    fracciones 5 == [(1,2),(1,3),(2,3),(1,4),(3,4),(1,5),(2,5),(3,5),(4,5)]
-- ---------------------------------------------------------------------

fracciones :: Integer -> [(Integer,Integer)]
fracciones n = [(x,y) | y <- [2..n], x <- [1..y-1], gcd x y == 1]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función 
--    fraccionesOrd :: Integer -> [(Integer,Integer)]
-- tal que (fraccionesOrd n) es la lista con las fracciones propias
-- positivas ordenadas, con denominador menor o igual que n. Por
-- ejemplo,
--    fraccionesOrd 4 == [(1,4),(1,3),(1,2),(2,3),(3,4)]
--    fraccionesOrd 5 == [(1,5),(1,4),(1,3),(2,5),(1,2),(3,5),(2,3),(3,4),(4,5)]
-- ---------------------------------------------------------------------

fraccionesOrd :: Integer -> [(Integer,Integer)]
fraccionesOrd n = sortBy comp (fracciones n)
    where comp (a,b) (c,d) = compare (a*d) (b*c)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Todo número par se puede escribir como suma de números
-- pares de varias formas. Por ejemplo: 
--    8 = 8 
--      = 6 + 2
--      = 4 + 4
--      = 4 + 2 + 2
--      = 2 + 2 + 2 + 2
--
-- Definir la función
--    descomposicionesDecrecientes:: Integer -> [[Integer]]
-- tal que (descomposicionesDecrecientes n) es la lista con las
-- descomposiciones de n como suma de pares, en forma decreciente. Por
-- ejemplo,
--    ghci> descomposicionesDecrecientes 8
--    [[8],[6,2],[4,4],[4,2,2],[2,2,2,2]]
--    ghci> descomposicionesDecrecientes 10
--    [[10],[8,2],[6,4],[6,2,2],[4,4,2],[4,2,2,2],[2,2,2,2,2]]
--
-- Calcular el número de descomposiciones de 40.
-- ---------------------------------------------------------------------

descomposicionesDecrecientes:: Integer -> [[Integer]]
descomposicionesDecrecientes 0 = [[0]]
descomposicionesDecrecientes n = aux n [n,n-2..2]
    where aux _ [] = []
          aux n (x:xs) | x > n     = aux n xs
                       | x == n    = [n] : aux n xs
                       | otherwise = map (x:) (aux (n-x) (x:xs)) ++ aux n xs

-- El cálculo es
--    ghci> length (descomposicionesDecrecientes 40)
--    627

-- ---------------------------------------------------------------------
-- Ejercicio 3. Consideremos los árboles binarios con etiquetas en las
-- hojas y en los nodos. Por ejemplo,
--          5       
--         / \      
--        2   4      
--           / \    
--          7   1
--             / \
--            2   3   
-- 
-- Un camino es una sucesión de nodos desde la raiz hasta una hoja. Por
-- ejemplo, [5,2] y [5,4,1,2] son caminos que llevan a 2, mientras que
-- [5,4,1] no es un camino, pues no lleva a una hoja.
-- 
-- Definimos el tipo de dato Arbol y el ejemplo por
--    data Arbol = H Int | N Arbol Int Arbol 
--                 deriving Show
--    
--    arb1:: Arbol 
--    arb1 = N (H 2) 5 (N (H 7) 4 (N (H 2) 1 (H 3)))
--    
-- Definir la función 
--    maxLong :: Int -> Arbol -> Int
-- tal que (maxLong x a) es la longitud máxima de los caminos que
-- terminan en x. Por ejemplo, 
--    maxLong 3 arb1 == 4
--    maxLong 2 arb1 == 4
--    maxLong 7 arb1 == 3
-- ---------------------------------------------------------------------

data Arbol = H Int | N Arbol Int Arbol 
             deriving Show

arb1:: Arbol 
arb1 = N (H 2) 5 (N (H 7) 4 (N (H 2) 1 (H 3)))

-- 1ª solución (calculando los caminos)
-- ------------------------------------

-- (caminos x a) es la lista de los caminos en el árbol a desde la raíz
-- hasta las hojas x. Por ejemplo,
--    caminos 2 arb1 == [[5,2],[5,4,1,2]]
--    caminos 3 arb1 == [[5,4,1,3]]
--    caminos 1 arb1 == []
caminos :: Int -> Arbol -> [[Int]]
caminos x (H y) | x == y    = [[x]]
                | otherwise = []
caminos x (N i r d) = map (r:) (caminos x i ++ caminos x d)

maxLong1 :: Int -> Arbol -> Int
maxLong1 x a = maximum (0: map length (caminos x a))

-- 2ª solución
-- -----------

maxLong2 :: Int -> Arbol -> Int
maxLong2 x a = maximum (0 : aux x a)
    where aux x (H y) | x == y    = [1]
                      | otherwise = []
          aux x (N i r d) = map (+1) (aux x i ++ aux x d)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Un elemento de una matriz es un máximo local si es un
-- elemento interior, que es mayor que todos sus vecinos. Por ejemplo,
-- en la matriz 
--     [[1,0,0,1],
--      [0,2,0,3],
--      [0,0,0,5],
--      [3,5,7,6],
--      [1,2,3,4]]
-- los máximos locales son 2 (en la posición (2,2)) y 7 (en la posición
-- (4,3)). 
-- 
-- Definimos el tipo de las matrices, mediante
--    type Matriz a = Array (Int,Int) a
-- y el ejemplo anterior por
--    ej1 :: Matriz Int
--    ej1 = listArray ((1,1),(5,4)) (concat [[1,0,0,1],
--                                           [0,2,0,3],
--                                           [0,0,0,5],
--                                           [3,5,7,6],
--                                           [1,2,3,4]])
--    
-- Definir la función 
--    maximosLocales :: Matriz Int -> [((Int,Int),Int)]
-- tal que (maximosLocales p) es la lista de las posiciones en las que
-- hay un máximo local, con el valor correspondiente. Por ejemplo,
--    maximosLocales ej1 == [((2,2),2),((4,3),7)]
-- ---------------------------------------------------------------------

type Matriz a = Array (Int,Int) a

ej1 :: Matriz Int
ej1 = listArray ((1,1),(5,4)) (concat [[1,0,0,1],
                                       [0,2,0,3],
                                       [0,0,0,5],
                                       [3,5,7,6],
                                       [1,2,3,4]])

maximosLocales :: Matriz Int -> [((Int,Int),Int)]
maximosLocales p = 
    [((i,j),p!(i,j)) | i <- [1..m], j <- [1..n], posicionMaxLocal (i,j) p]
        where (_,(m,n)) = bounds p


-- (posicionMaxLocal (i,j) p) se verifica si (i,j) es la posición de un
-- máximo local de la matriz p. Por ejemplo,
--    posicionMaxLocal (2,2) ej1  ==  True
--    posicionMaxLocal (2,3) ej1  ==  False
posicionMaxLocal :: (Int,Int) -> Matriz Int -> Bool
posicionMaxLocal (i,j) p = 
    esInterior (i,j) p && all (< p!(i,j)) (vecinosInterior (i,j) p)

-- (esInterior (i,j) p) se verifica si (i,j) es una posición interior de
-- la matriz p. 
esInterior:: (Int,Int) -> Matriz a -> Bool
esInterior (i,j) p = i /= 1 && i /= m && j /= 1 && j /= n
    where (_,(m,n)) = bounds p

-- (indicesVecinos (i,j)) es la lista de las posiciones de los
-- vecinos de la posición (i,j). Por ejemplo,
--    ghci> indicesVecinos (2,2)
--    [(1,1),(1,2),(1,3),(2,1),(2,3),(3,1),(3,2),(3,3)]
indicesVecinos :: (Int,Int) -> [(Int,Int)]
indicesVecinos (i,j) = 
    [(i+a,j+b) | a <- [-1,0,1], b <- [-1,0,1], (a,b) /= (0,0)] 
              
-- (vecinosInterior (i,j) p) es la lista de los valores de los vecinos
-- de la posición (i,j) en la matriz p. Por ejemplo,
--    vecinosInterior (4,3) ej1  ==  [0,0,5,5,6,2,3,4]
vecinosInterior (i,j) p = 
    [p!(k,l) | (k,l) <- indicesVecinos (i,j)]

-- ---------------------------------------------------------------------
-- Ejercicio 5. Los polinomios de Bell forman una sucesión de
-- polinomios, definida como sigue:
--    B_0(x) = 1 (polinomio unidad)
--    B_n(x) = x*[B_n(x) + B_n'(x)]
-- Por ejemplo,  
--    B_0(x) = 1     
--    B_1(x) = x*(1+0)                    = x     
--    B_2(x) = x*(x+1)                    = x^2+x         
--    B_3(x) = x*(x^2+x + 2x+1)           = x^3+3x^2+x    
--    B_4(x) = x*(x^3+3x^2+x + 3x^2+6x+1) = x^4+6x^3+7x^2+x       
--
-- Definir la función 
--    polBell :: Int -> Polinomio Int
-- tal que (polBell n) es el polinomio de Bell de grado n. Por ejemplo, 
--    polBell1 4  ==  x^4 + 6*x^3 + 7*x^2 + 1*x
--
-- Calcular el coeficiente de x^2 en el polinomio B_30.
-- ---------------------------------------------------------------------

-- 1ª solución (por recursión)
polBell1 :: Integer -> Polinomio Integer
polBell1 0 = polUnidad
polBell1 n = multPol (consPol 1 1 polCero) (sumaPol p (derivada p))
    where p = polBell1 (n-1)

-- 2ª solución (evaluación perezosa)
polBell2 :: Integer -> Polinomio Integer
polBell2 n = sucPolinomiosBell `genericIndex` n

sucPolinomiosBell :: [Polinomio Integer]
sucPolinomiosBell = iterate f polUnidad
    where f p = multPol (consPol 1 1 polCero) (sumaPol p (derivada p))

-- El cálculo es
--    ghci> coeficiente 2 (polBellP1 30)
--    536870911
