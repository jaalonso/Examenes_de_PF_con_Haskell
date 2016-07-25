-- Informática (1º del Grado en Matemáticas, Grupo 2)
-- 6º examen de evaluación continua (3 de mayo de 2012)
-- ---------------------------------------------------------------------

import Data.List
import Data.Array
import Test.QuickCheck
import PolOperaciones

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función
--    verificaP :: (a -> Bool) -> [[a]] -> Bool
-- tal que (verificaP p xss) se cumple si cada lista de xss contiene
-- algún elemento que verifica el predicado p. Por ejemplo,
--    verificaP odd [[1,3,4,2], [4,5], [9]] == True
--    verificaP odd [[1,3,4,2], [4,8], [9]] == False
-- ---------------------------------------------------------------------

verificaP :: (a -> Bool) -> [[a]] -> Bool
verificaP p xss = and [any p xs | xs <- xss]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función
--    verificaTT :: (a -> Bool) -> [[a]] -> Bool
-- tal que (verificaTT p xss) se cumple si todos los elementos de todas
-- las listas de xss verifican el predicado p. Por ejemplo,
--    verificaTT odd [[1,3], [7,5], [9]]     == True
--    verificaTT odd [[1,3,4,2], [4,8], [9]] == False
-- ---------------------------------------------------------------------

verificaTT :: (a -> Bool) -> [[a]] -> Bool
verificaTT p xss = and [all p xs | xs <- xss]

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la función
--    verificaEE :: (a -> Bool) -> [[a]] -> Bool
-- tal que (verificaEE p xss) se cumple si algún elemento de alguna
-- lista de xss verifica el predicado p. Por ejemplo,
--    verificaEE odd [[1,3,4,2], [4,8], [9]] == True
--    verificaEE odd [[4,2], [4,8], [10]]    == False
-- ---------------------------------------------------------------------

verificaEE :: (a -> Bool) -> [[a]] -> Bool
verificaEE p xss = or [any p xs | xs <- xss]

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Definir la función
--    verificaET :: (a -> Bool) -> [[a]] -> Bool
-- tal que (verificaET p xss) se cumple si todos los elementos de alguna
-- lista de xss verifican el predicado p. Por ejemplo, 
--    verificaET odd [[1,3], [4,8], [10]] == True
--    verificaET odd [[4,2], [4,8], [10]] == False
-- ---------------------------------------------------------------------

verificaET :: (a -> Bool) -> [[a]] -> Bool
verificaET p xss = or [all p xs | xs <- xss]

-- ---------------------------------------------------------------------
-- Ejercicio 2. (Problema 303 del proyecto Euler). Dado un número
-- natural n, se define  f(n) como el menor natural, múltiplo de n,
-- cuyos dígitos son todos menores o iguales que 2. Por ejemplo, f(2)=2,
-- f(3)=12, f(7)=21, f(42)=210, f(89)=1121222. 
--  
-- Definir la función 
--    menorMultiplo1y2 :: Int -> Int
-- tal que (menorMultiplo1y2 n) es el menor múltiplo de n cuyos dígitos
-- son todos menores o iguales que 2. Por ejemplo,
--    menorMultiplo1y2 42  ==  210
-- ---------------------------------------------------------------------

menorMultiplo1y2 :: Int -> Int
menorMultiplo1y2 n = 
    head [x | x <- [n,2*n..], all (<=2) (cifras x)]

-- (cifras n) es la lista de las cifras de n. Por ejemplo,
--    cifras 325  ==  [3,2,5]
cifras :: Int -> [Int]
cifras n = [read [x]| x <- show n]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    raicesApol :: Fractional t => [(t, Int)] -> Polinomio t
-- tal que (raicesApol rs) es el polinomio correspondiente si rs es la
-- lista de raices, con sus respectivas multiplicidades, rs; es decir, 
-- raicesApol [(r1,n1),...,(rk,nk)] es (x-r1)^n1...(x-rk)^nk. Por
-- ejemplo,
--   raicesApol [(2,1),(-1,3)] == x^4 + x^3 + -3.0*x^2 + -5.0*x + -2.0 
-- ---------------------------------------------------------------------

raicesApol :: Fractional t => [(t, Int)] -> Polinomio t
raicesApol rs = multListaPol factores
    where factores = [potencia (creaFactor x) n | (x,n) <- rs]

-- (creaFactor a) es el polinomio x-a. Por ejemplo,
--    ghci> creaFactor 5
--    1.0*x + -5.0
creaFactor :: Fractional t => t -> Polinomio t
creaFactor a = creaPolDensa [(1,1),(0,-a)]

-- (creaPolDensa ps) es el polinomio cuya representación densa (mediante
-- pares con grados y coeficientes) es ps. Por ejemplo,
--    ghci> creaPolDensa [(3,5),(2,4),(0,7)]
--    5*x^3 + 4*x^2 + 7
creaPolDensa :: Num a => [(Int,a)] -> Polinomio a
creaPolDensa []         = polCero
creaPolDensa ((n,a):ps) = consPol n a (creaPolDensa ps)

-- (potencia p n) es la n-ésima potencia de P. Por ejemplo,
--    ghci> potencia (creaFactor 5) 2
--    x^2 + -10.0*x + 25.0
potencia :: Num a => Polinomio a -> Int -> Polinomio a
potencia p 0 = polUnidad
potencia p n = multPol p (potencia p (n-1))

-- (multListaPol ps) es el producto de los polinomios de la lista
-- ps. Por ejemplo,
--    ghci> multListaPol [creaFactor 2, creaFactor 3, creaFactor 4]
--    x^3 + -9.0*x^2 + 26.0*x + -24.0
multListaPol :: Num t => [Polinomio t] -> Polinomio t
multListaPol []     = polUnidad
multListaPol (p:ps) = multPol p (multListaPol ps)

-- multListaPol se puede definir por plegado: 
multListaPol' :: Num t => [Polinomio t] -> Polinomio t
multListaPol' = foldr multPol polUnidad

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Consideremos el tipo de los vectores y las matrices
-- definidos por
--    type Vector a = Array Int a
--    type Matriz a = Array (Int,Int) a
-- 
-- Definir la función
--    esEscalar:: Num a => Matriz a -> Bool
-- tal que (esEscalar p) se verifica si p es una matriz es escalar; es
-- decir, diagonal con todos los elementos de la diagonal principal
-- iguales. Por ejemplo,
--    esEscalar (listArray ((1,1),(3,3)) [5,0,0,0,5,0,0,0,5])  ==  True
--    esEscalar (listArray ((1,1),(3,3)) [5,0,0,1,5,0,0,0,5])  ==  False
--    esEscalar (listArray ((1,1),(3,3)) [5,0,0,0,6,0,0,0,5])  ==  False
-- ---------------------------------------------------------------------

type Vector a = Array Int a
type Matriz a = Array (Int,Int) a

esEscalar:: Num a => Matriz a -> Bool
esEscalar p = esDiagonal p && todosIguales (elems (diagonalPral p))

-- (esDiagonal p) se verifica si la matriz p es diagonal. Por ejemplo.
--    esDiagonal (listArray ((1,1),(3,3)) [5,0,0,0,6,0,0,0,5])  ==  True
--    esDiagonal (listArray ((1,1),(3,3)) [5,0,0,1,5,0,0,0,5])  ==  False
esDiagonal:: Num a => Matriz a -> Bool
esDiagonal p = all (==0) [p!(i,j) | i<-[1..m],j<-[1..n], i/=j]
    where (m,n) = dimension p

-- (todosIguales xs) se verifica si todos los elementos de xs son
-- iguales. Por ejemplo,
--    todosIguales [5,5,5]  ==  True
--    todosIguales [5,6,5]  ==  False
todosIguales :: Eq a => [a] -> Bool
todosIguales (x:y:ys) = x == y && todosIguales (y:ys) 
todosIguales _ = True 

-- (diagonalPral p) es la diagonal principal de la matriz p. Por
-- ejemplo, 
--    ghci> diagonalPral (listArray ((1,1),(3,3)) [5,0,0,1,6,0,0,2,4])
--    array (1,3) [(1,5),(2,6),(3,4)]
diagonalPral :: Num a => Matriz a -> Vector a
diagonalPral p = array (1,n) [(i,p!(i,i)) | i <- [1..n]]
    where n = min (numFilas p) (numColumnas p)

-- (numFilas p) es el número de filas de la matriz p. Por ejemplo,
--    numFilas (listArray ((1,1),(2,3)) [5,0,0,1,6,0])  ==  2
numFilas :: Num a => Matriz a -> Int
numFilas = fst . snd . bounds

-- (numColumnas p) es el número de columnas de la matriz p. Por ejemplo, 
--    numColumnas (listArray ((1,1),(2,3)) [5,0,0,1,6,0])  ==  3
numColumnas:: Num a => Matriz a -> Int
numColumnas = snd . snd . bounds

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función
--    determinante:: Matriz Double -> Double
-- tal que (determinante p) es el determinante de la matriz p. Por
-- ejemplo, 
--    ghci> determinante (listArray ((1,1),(3,3)) [2,0,0,0,3,0,0,0,1])
--    6.0
--    ghci> determinante (listArray ((1,1),(3,3)) [1..9])
--    0.0
--    ghci> determinante (listArray ((1,1),(3,3)) [2,1,5,1,2,3,5,4,2])
--    -33.0
-- ---------------------------------------------------------------------

determinante:: Matriz Double -> Double
determinante p 
    | dimension p == (1,1) = p!(1,1)
    | otherwise = 
        sum [((-1)^(i+1))*(p!(i,1))*determinante (submatriz i 1 p)
             | i <- [1..numFilas p]]

-- (dimension p) es la dimensión de la matriz p. Por ejemplo,
--    dimension (listArray ((1,1),(2,3)) [5,0,0,1,6,0])  ==  (2,3)
dimension :: Num a => Matriz a -> (Int,Int)
dimension p = (numFilas p, numColumnas p)

-- (submatriz i j p) es la submatriz de p obtenida eliminado la fila i y
-- la columna j. Por ejemplo,
--    ghci> submatriz 2 3 (listArray ((1,1),(3,3)) [2,1,5,1,2,3,5,4,2])
--    array ((1,1),(2,2)) [((1,1),2),((1,2),1),((2,1),5),((2,2),4)]
--    ghci> submatriz 2 3 (listArray ((1,1),(3,3)) [1..9])
--    array ((1,1),(2,2)) [((1,1),1),((1,2),2),((2,1),7),((2,2),8)]
submatriz :: Num a => Int -> Int -> Matriz a -> Matriz a
submatriz i j p = 
    array ((1,1), (m-1,n -1))
          [((k,l), p ! f k l) | k <- [1..m-1], l <- [1..n-1]]
    where (m,n) = dimension p
          f k l | k < i  && l < j  = (k,l)
                | k >= i && l < j  = (k+1,l)
                | k < i  && l >= j = (k,l+1)
                | otherwise        = (k+1,l+1)






