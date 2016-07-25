-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 5º examen de evaluación continua (22 de mayo de 2013)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Data.List
import Data.Array
import PolOperaciones

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    conFinales :: Int -> [Int] -> [Int]
-- tal que (conFinales x xs) es la lista de los elementos de xs que
-- terminan en x. Por ejemplo,
--    conFinales 2 [31,12,7,142,214]  ==  [12,142]
-- Dar cuatro definiciones distintas: recursiva, por comprensión, con 
-- filtrado y por plegado.
-- ---------------------------------------------------------------------

-- 1ª definición (recursiva):
conFinales1 :: Int -> [Int] -> [Int]
conFinales1 x [] = []
conFinales1 x (y:ys) | mod y 10 == x = y : conFinales1 x ys
                     | otherwise     = conFinales1 x ys

-- 2ª definición (por comprensión):
conFinales2 :: Int -> [Int] -> [Int]
conFinales2 x xs = [y | y <- xs, mod y 10 == x]

-- 3ª definición (por filtrado):
conFinales3 :: Int -> [Int] -> [Int]
conFinales3 x xs = filter (\z -> mod z 10 == x) xs

-- 4ª definición (por plegado):
conFinales4 :: Int -> [Int] -> [Int]
conFinales4 x = foldr f [] 
    where f y ys | mod y 10 == x = y:ys
	  	 | otherwise     = ys

-- ---------------------------------------------------------------------
-- Ejercicio 2. (OME 2010) Una sucesión pucelana es una sucesión
-- creciente de dieciseis números impares positivos consecutivos, cuya
-- suma es un cubo perfecto.
-- 
-- Definir la función  
--    pucelanasDeTres :: [[Int]]
-- tal que pucelanasDeTres es la lista de la sucesiones pucelanas
-- formadas por números de tres cifras. Por ejemplo,
--    ghci> take 2 pucelanasDeTres
--    [[241,243,245,247,249,251,253,255,257,259,261,263,265,267,269,271],
---    [485,487,489,491,493,495,497,499,501,503,505,507,509,511,513,515]]
-- ¿Cuántas sucesiones pucelanas tienen solamente números de tres
-- cifras?  
-- ---------------------------------------------------------------------

pucelanasDeTres :: [[Int]]
pucelanasDeTres = [[x,x+2 .. x+30] | x <- [101, 103 .. 999-30],
		  	     	     esCubo (sum [x,x+2 .. x+30])]

esCubo x = or [y^3 == x | y <- [1..x]]

-- El número se calcula con
--    ghci> length pucelanasDeTres
--    3


-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función:
--    extraePares :: Polinomio Integer -> Polinomio Integer
-- tal que (extraePares p) es el polinomio que resulta de extraer los 
-- monomios de grado par de p. Por ejemplo, si p es el polinomio 
-- x^4 + 5*x^3 + 7*x^2 + 6*x, entonces (extraePares p) es 
-- x^4 + 7*x^2.
--    > let p1 = consPol 4 1 (consPol 3 5 (consPol 2 7 (consPol 1 6 polCero)))
--    > p1
--    x^4 + 5*x^3 + 7*x^2 + 6*x
--    > extraePares p1
--    x^4 + 7*x^2
-- ---------------------------------------------------------------------

extraePares :: Polinomio Integer -> Polinomio Integer
extraePares p 
    | esPolCero p = polCero
    | even n      = consPol n (coefLider p) (extraePares rp)
    | otherwise   = extraePares rp
    where n  = grado p
          rp = restoPol p

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    rellenaPol :: Polinomio Integer -> Polinomio Integer
-- tal que (rellenaPol p) es el polinomio obtenido completando con
-- monomios del tipo 1*x^n aquellos monomios de grado n que falten en
-- p. Por ejemplo, 
--    ghci> let p1 = consPol 4 2 (consPol 2 1 (consPol 0 5 polCero))
--    ghci> p1
--    2*x^4 + x^2 + 5
--    ghci> rellenaPol p1
--    2*x^4 + x^3 + x^2 + 1*x + 5
-- ---------------------------------------------------------------------

rellenaPol :: Polinomio Integer -> Polinomio Integer
rellenaPol p 
    | n == 0 = p
    | n == grado r + 1 = consPol n c (rellenaPol r)
    | otherwise = consPol n c (consPol (n-1) 1 (rellenaPol r))
    where n = grado p
          c = coefLider p
          r = restoPol p   
					     
-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Consideremos el tipo de las matrices
--    type Matriz a = Array (Int,Int) a
-- y, para los ejemplos, la matriz
--    m1 :: Matriz Int
--    m1 = array ((1,1),(3,3))
--               [((1,1),1),((1,2),0),((1,3),1),
--                ((2,1),0),((2,2),1),((2,3),1),
--                ((3,1),1),((3,2),1),((3,3),1)])

-- Definir la función 
--    cambiaM :: (Int, Int) -> Matriz Int -> Matriz Int
-- tal que (cambiaM i p) es la matriz obtenida cambiando en p los
-- elementos de la fila y la columna en i transformando los 0 en 1 y
-- viceversa. El valor en i cambia solo una vez. Por ejemplo,
--    ghci> cambiaM (2,3) m1
--    array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),0),
--                         ((2,1),1),((2,2),7),((2,3),0),
--                         ((3,1),1),((3,2),1),((3,3),0)]
-- ---------------------------------------------------------------------

type Matriz a = Array (Int,Int) a

m1 :: Matriz Int
m1 = array ((1,1),(3,3))
           [((1,1),1),((1,2),0),((1,3),1),
            ((2,1),0),((2,2),7),((2,3),1),
            ((3,1),1),((3,2),1),((3,3),1)]

cambiaM :: (Int, Int) -> Matriz Int -> Matriz Int
cambiaM (a,b) p = array (bounds p) [((i,j),f i j) | (i,j) <- indices p]
       where f i j  | i == a || j == b = cambia (p!(i,j))
                    | otherwise = p!(i,j)
             cambia x | x == 0    = 1
                      | x == 1    = 0
                      | otherwise = x

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función 
--    quitaRepetidosFila :: Int -> Matriz Int -> Matriz Int
-- tal que (quitaRepetidosFila i p) es la matriz obtenida a partir de p
-- eliminando los elementos repetidos de la fila i y rellenando con
-- ceros al final hasta completar la fila. Por ejemplo,
--    ghci> m1
--    array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),1),
--                         ((2,1),0),((2,2),7),((2,3),1),
--                         ((3,1),1),((3,2),1),((3,3),1)]
--    ghci> quitaRepetidosFila 1 m1
--    array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),0),
--                         ((2,1),0),((2,2),7),((2,3),1),
--                         ((3,1),1),((3,2),1),((3,3),1)]
--    ghci> quitaRepetidosFila 2 m1
--    array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),1),
--                         ((2,1),0),((2,2),7),((2,3),1),
--                         ((3,1),1),((3,2),1),((3,3),1)]
--    ghci> quitaRepetidosFila 3 m1
--    array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),1),
--                         ((2,1),0),((2,2),7),((2,3),1),
--                         ((3,1),1),((3,2),0),((3,3),0)]
-- ---------------------------------------------------------------------

quitaRepetidosFila :: Int -> Matriz Int -> Matriz Int
quitaRepetidosFila x p = 
    array (bounds p) [((i,j),f i j) | (i,j) <- indices p]
    where f i j | i == x    = (cambia (fila i p)) !! (j-1)
                | otherwise = p!(i,j)

-- (fila i p) es la fila i-ésima de la matriz p. Por ejemplo,
--    ghci> m1
--    array ((1,1),(3,3)) [((1,1),1),((1,2),0),((1,3),1),
--                         ((2,1),0),((2,2),7),((2,3),1),
--                         ((3,1),1),((3,2),1),((3,3),1)]
--    ghci> fila 2 m1
--    [0,7,1]
fila :: Int -> Matriz Int -> [Int]
fila i p = [p!(i,j) | j <- [1..n]]
    where (_,(_,n)) = bounds p

-- (cambia xs) es la lista obtenida eliminando los elementos repetidos
-- de xs y completando con ceros al final para que tenga la misma
-- longitud que xs. Por ejemplo,
--   cambia [2,3,2,5,3,2]  ==  [2,3,5,0,0,0]
cambia :: [Int] -> [Int]
cambia xs = ys ++ replicate (n-m) 0
    where ys = nub xs
          n  = length xs
          m  = length ys


