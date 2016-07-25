-- Informática (1º del Grado en Matemáticas, Grupo 2)
-- 7º examen de evaluación continua (24 de junio de 2012)
-- ---------------------------------------------------------------------

import Data.List
import Data.Array
import Data.Ratio
import Test.QuickCheck
import PolOperaciones
import GrafoConVectorDeAdyacencia

-- ------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    duplicaElemento :: Eq a => a -> [a] -> [a]
-- tal que (duplicaElemento x ys) es la lista obtenida duplicando las
-- apariciones del elemento x en la lista ys. Por ejemplo,
--    duplicaElemento 7 [2,7,3,7,7,5]  ==  [2,7,7,3,7,7,7,7,5]
-- ---------------------------------------------------------------------

duplicaElemento :: Eq a => a -> [a] -> [a]
duplicaElemento _ [] = []
duplicaElemento x (y:ys) | y == x    = y : y : duplicaElemento x ys
                         | otherwise = y : duplicaElemento x ys

-- ------------------------------------------------------------------
-- Ejercicio 2.1. Definir la función
--    listaAcumulada :: Num t => [t] -> [t]
-- tal que (listaAcumulada xs) es la lista obtenida sumando de forma
-- acumulada los elementos de xs. Por ejemplo,
--    listaAcumulada [1..4] == [1,3,6,10]
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
listaAcumulada :: Num t => [t] -> [t]
listaAcumulada xs = [sum (take n xs) | n <- [1..length xs]]

-- 2ª definición (por recursión):
listaAcumuladaR [] = []
listaAcumuladaR xs = listaAcumuladaR (init xs) ++ [sum xs]

-- 3ª definición (por recursión final)
listaAcumuladaRF [] = []
listaAcumuladaRF (x:xs) = reverse (aux xs [x])
    where aux [] ys = ys
          aux (x:xs) (y:ys) = aux xs (x+y:y:ys)

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que el último elemento de 
-- (listaAcumulada xs) coincide con la suma de los elemntos de xs.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_listaAcumulada :: [Int] -> Property
prop_listaAcumulada xs = 
    not (null xs) ==> last (listaAcumulada xs) == sum xs

-- La comprobación es  
--    ghci> quickCheck prop_listaAcumulada
--    +++ OK, passed 100 tests.
  
-- ------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función
--    menorP :: (Int -> Bool) -> Int
-- tal que (menorP p) es el menor número natural que verifica el
-- predicado p. Por ejemplo,
--    menorP (>7)   ==  8
-- ---------------------------------------------------------------------

menorP :: (Int -> Bool) -> Int
menorP p = head [n | n <- [0..], p n]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    menorMayorP :: Int -> (Int -> Bool) -> Int
-- tal que (menorMayorP m p) es el menor número natural mayor que m que
-- verifica el predicado p. Por ejemplo,
--    menorMayorP 7 (\x -> rem x 5 == 0)   ==  10
-- ---------------------------------------------------------------------

menorMayorP :: Int -> (Int -> Bool) -> Int
menorMayorP m p = head [n | n <- [m+1..], p n]

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir la función
--    mayorMenorP :: Int -> (Int -> Bool) -> Int
-- tal que (mayorMenorP p) es el mayor entero menor que m que verifica
-- el predicado p. Por ejemplo,
--    mayorMenorP 17 (\x -> rem x 5 == 0)   ==  15
-- ---------------------------------------------------------------------

mayorMenorP :: Int -> (Int -> Bool) -> Int
mayorMenorP m p = head [n | n <- [m-1,m-2..], p n]

-- ------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    polNumero :: Int -> Polinomio Int
-- tal que (polNumero n) es el polinomio cuyos coeficientes son las
-- cifras de n. Por ejemplo,
--   polNumero 5703 == 5x^3 + 7x^2 + 3
-- ---------------------------------------------------------------------
          
polNumero :: Int -> Polinomio Int
polNumero = creaPolDispersa . cifras

-- (cifras n) es la lista de las cifras de n. Por ejemplo,        
--    cifras 142857  ==  [1,4,2,8,5,7]
cifras :: Int -> [Int]
cifras n = [read [x]| x <- show n]

-- (creaPolDispersa xs) es el polinomio cuya representación dispersa es
-- xs. Por ejemplo, 
--    creaPolDispersa [7,0,0,4,0,3]  ==  7*x^5 + 4*x^2 + 3
creaPolDispersa :: (Num a, Eq a) => [a] -> Polinomio a
creaPolDispersa []     = polCero
creaPolDispersa (x:xs) = consPol (length xs) x (creaPolDispersa xs)

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Los vectores se definen por
--    type Vector = Array Int Float
-- 
-- Un vector se denomina estocástico si todos sus elementos son mayores  
-- o iguales que 0 y suman 1. 
-- 
-- Definir la función 
--    vectorEstocastico :: Vector -> Bool
-- tal que (vectorEstocastico v) se verifica si v es estocástico. Por
-- ejemplo,  
--    vectorEstocastico (listArray (1,5) [0.1, 0.2, 0, 0, 0.7]) == True
--    vectorEstocastico (listArray (1,5) [0.1, 0.2, 0, 0, 0.9]) == False
-- ---------------------------------------------------------------------

type Vector = Array Int Float

vectorEstocastico :: Vector -> Bool
vectorEstocastico v = all (>=0) xs && sum xs == 1
    where xs = elems v

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Las matrices se definen por        
--    type Matriz = Array (Int,Int) Float
-- 
-- Una matriz se demonina estocástica si sus columnas son vectores
-- estocásticos. 
-- 
-- Definir la función 
--    matrizEstocastica :: Matriz -> Bool
-- tal que (matrizEstocastico p) se verifica si p es estocástica. Por
-- ejemplo,  
--    matrizEstocastica (listArray ((1,1),(2,2)) [0.1,0.2,0.9,0.8]) == True
--    matrizEstocastica (listArray ((1,1),(2,2)) [0.1,0.2,0.3,0.8]) == False
-- ---------------------------------------------------------------------

type Matriz = Array (Int,Int) Float
          
matrizEstocastica :: Matriz -> Bool        
matrizEstocastica p = all vectorEstocastico (columnas p)

-- (columnas p) es la lista de las columnas de la matriz p. Por ejemplo, 
--    ghci> columnas (listArray ((1,1),(2,3)) [1..6])
--    [array (1,2) [(1,1.0),(2,4.0)],
--     array (1,2) [(1,2.0),(2,5.0)],
--     array (1,2) [(1,3.0),(2,6.0)]]
--    ghci> columnas (listArray ((1,1),(3,2)) [1..6])
--    [array (1,3) [(1,1.0),(2,3.0),(3,5.0)],
--     array (1,3) [(1,2.0),(2,4.0),(3,6.0)]]
columnas :: Matriz -> [Vector]
columnas p = 
    [array (1,m) [(i,p!(i,j)) | i <- [1..m]] | j <- [1..n]]
    where (_,(m,n)) = bounds p

-- ---------------------------------------------------------------------
-- Ejercicio 6. Consideremos un grafo G = (V,E), donde V es un conjunto
-- finito de nodos ordenados y E es un conjunto de arcos. En un grafo,
-- la anchura de un nodo es el número de nodos adyacentes; y la anchura
-- del grafo es la máxima anchura de sus nodos. Por ejemplo, en el grafo 
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
--    anchuraN g 1  ==  3
--    anchuraN g 2  ==  3
--    anchuraN g 3  ==  3
--    anchuraN g 4  ==  3
--    anchuraN g 5  ==  4
anchuraN :: Grafo Int Int -> Int -> Int
anchuraN g x = length (adyacentes g x)

-- ---------------------------------------------------------------------
-- Ejercicio 7. Un número natural par es admisible si es una potencia de
-- 2 o sus distintos factores primos son primos consecutivos. Los
-- primeros números admisibles son 2, 4, 6, 8, 12, 16, 18, 24, 30, 32,
-- 36, 48,...
-- 
-- Definir la constante          
--    admisibles :: [Integer]
-- que sea la lista de los números admisibles. Por ejemplo,
--    take 12 admisibles  ==  [2,4,6,8,12,16,18,24,30,32,36,48]
-- ---------------------------------------------------------------------

admisibles:: [Integer]
admisibles = [n | n <- [2,4..], esAdmisible n]

-- (esAdmisible n) se verifica si n es admisible. Por ejemplo,
--    esAdmisible 32  ==  True
--    esAdmisible 48  ==  True
--    esAdmisible 15  ==  False
--    esAdmisible 10  ==  False
esAdmisible :: Integer -> Bool
esAdmisible n = 
    even n &&
    (esPotenciaDeDos n || primosConsecutivos (nub (factorizacion n)))

-- (esPotenciaDeDos n) se verifica si n es una potencia de 2. Por ejemplo,
--    esPotenciaDeDos 4  ==  True
--    esPotenciaDeDos 5  ==  False
esPotenciaDeDos :: Integer -> Bool
esPotenciaDeDos 1 = True
esPotenciaDeDos n = even n && esPotenciaDeDos (n `div` 2)

-- (factorizacion n) es la lista de todos los factores primos de n; es
-- decir, es una lista de números primos cuyo producto es n. Por ejemplo,
--    factorizacion 300  ==  [2,2,3,5,5]
factorizacion :: Integer -> [Integer]
factorizacion n | n == 1    = []
                | otherwise = x : factorizacion (div n x)
                where x = menorFactor n

-- (menorFactor n) es el menor factor primo de n. Por ejemplo,
--    menorFactor 15  ==  3
--    menorFactor 16  ==  2
--    menorFactor 17  == 17
menorFactor :: Integer -> Integer
menorFactor n = head [x | x <- [2..], rem n x == 0]

-- (primosConsecutivos xs) se verifica si xs es una lista de primos
-- consecutivos. Por ejemplo,
--    primosConsecutivos [17,19,23]  ==  True
--    primosConsecutivos [17,19,29]  ==  False
--    primosConsecutivos [17,19,20]  ==  False
primosConsecutivos :: [Integer] -> Bool
primosConsecutivos [] = True
primosConsecutivos (x:xs) = 
    take (1 + length xs) (dropWhile (<x) primos) == x:xs

-- primos es la lista de los números primos. Por ejemplo,
--    take 10 primos  ==  [2,3,5,7,11,13,17,19,23,29]
primos :: [Integer]
primos = 2 : [n | n <- [3,5..], esPrimo n]

-- (esPrimo n) se verifica si n es primo. 
esPrimo :: Integer-> Bool
esPrimo n = [x | x <- [1..n], rem n x == 0] == [1,n]

