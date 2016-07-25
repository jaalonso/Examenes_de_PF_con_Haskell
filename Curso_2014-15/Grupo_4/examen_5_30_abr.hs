-- Informática (1º del Grado en Matemáticas)
-- 4º examen de evaluación continua (30 de abril de 2015)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Numbers.Primes
import Test.QuickCheck
import Data.List
import Data.Array
import I1M.Grafo
import I1M.Monticulo

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Un número n es especial si al unir las cifras de sus
-- factores primos, se obtienen exactamente las cifras de n, aunque
-- puede ser en otro orden. Por ejemplo, 1255 es especial, pues los
-- factores primos de 1255 son 5 y 251.
--
-- Definir la función 
--    esEspecial :: Integer -> Bool
-- tal que (esEspecial n) se verifica si un número n es especial. Por
-- ejemplo, 
--    esEspecial 1255 == True
--    esEspecial 125  == False
-- ---------------------------------------------------------------------

esEspecial :: Integer -> Bool
esEspecial n = 
    sort (show n) == sort (concatMap show (nub (primeFactors n)))

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que todo número primo es
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
-- Ejercicio 1.3. Calcular los 5 primeros números especiales que no son
-- primos. 
-- ---------------------------------------------------------------------

-- El cálculo es
--    ghci> take 5 [n | n <- [2..], esEspecial n, not (isPrime n)]
--    [735,1255,3792,7236,11913]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Consideremos las relaciones binarias homogéneas,
-- representadas por el siguiente tipo
--    type Rel a = ([a],[(a,a)])
-- y las matrices, representadas por
--    type Matriz a = Array (Int,Int) a
-- 
-- Dada una relación r sobre un conjunto de números enteros, la matriz
-- asociada a r es una matriz booleana p (cuyos elementos son True o
-- False), tal que p(i,j) = True si y sólo si i está relacionado con j
-- mediante la relación r. 
-- 
-- Definir la función
--    matrizRB:: Rel Int -> Matriz Bool
-- tal que (matrizRB r) es la matriz booleana asociada a r. Por ejemplo, 
--    ghci> matrizRB ([1..3],[(1,1), (1,3), (3,1), (3,3)])
--    array ((1,1),(3,3)) [((1,1),True) ,((1,2),False),((1,3),True),
--                         ((2,1),False),((2,2),False),((2,3),False),
--                         ((3,1),True) ,((3,2),False),((3,3),True)]
--    ghci> matrizRB ([1..3],[(1,3), (3,1)])
--    array ((1,1),(3,3)) [((1,1),False),((1,2),False),((1,3),True),
--                         ((2,1),False),((2,2),False),((2,3),False),
--                         ((3,1),True) ,((3,2),False),((3,3),False)]
-- 
-- Nota: Construir una matriz booleana cuadrada, de dimensión nxn,
-- siendo n el máximo de los elementos del universo de r. 
-- ---------------------------------------------------------------------

type Rel a    = ([a],[(a,a)])
type Matriz a = Array (Int,Int) a

-- 1ª definición (con array, universo y grafo):
matrizRB:: Rel Int -> Matriz Bool
matrizRB r = 
    array ((1,1),(n,n)) 
          [((a,b), (a,b) `elem` grafo r) | a <- [1..n], b <- [1..n]]
    where n = maximum (universo r)

universo :: Eq a => Rel a -> [a]
universo (us,_) = us

grafo :: Eq a => Rel a -> [(a,a)]
grafo (_,ps) = ps

-- 2ª definición (con listArray y sin universo ni grafo):
matrizRB2:: Rel Int -> Matriz Bool
matrizRB2 r = 
    listArray ((1,1),(n,n)) 
              [(a,b) `elem` snd r | a <- [1..n], b <- [1..n]]
    where n = maximum (fst r)

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Dado un grafo G = (V, E), 
--    + la distancia entre dos nodos de G es el valor absoluto de su
--      diferencia,
--    + la anchura de un nodo x es la máxima distancia entre x y todos
--      los nodos adyacentes y 
--    + la anchura del grafo es la máxima anchura de sus nodos.
-- 
-- Definir la función 
--    anchuraG :: Grafo Int Int -> Int
-- tal que (anchuraG g) es la anchura del grafo g. Por ejemplo, si g es 
-- el grafo definido a continuación,
--    g :: Grafo Int Int
--    g = creaGrafo ND (1,5) [(1,2,12),(1,3,34),(1,5,78),
--                            (2,4,55),(2,5,32),
--                            (3,4,61),(3,5,44),
--                            (4,5,93)]
-- entonces
--    anchuraG g == 4
-- ---------------------------------------------------------------------

g :: Grafo Int Int
g = creaGrafo ND (1,5) [(1,2,12),(1,3,34),(1,5,78),
                        (2,4,55),(2,5,32),
                        (3,4,61),(3,5,44),
                        (4,5,93)]
 
anchuraG :: Grafo Int Int -> Int
anchuraG g = maximum [abs(a-b) | (a,b,_) <- aristas g]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar experimentalmente que la anchura del grafo
-- cíclico de orden n (para n entre 1 y 20) es n-1.
-- ---------------------------------------------------------------------

-- La propiedad es
propG :: Int -> Bool
propG n = anchuraG (grafoCiclo n) == n-1

grafoCiclo :: Int -> Grafo Int Int
grafoCiclo n = creaGrafo ND (1,n) ([(x,x+1,0) | x <- [1..n-1]] ++ [(n,1,0)])

-- La comprobación es
--    ghci> and [propG n | n <- [2..10]]
--    True

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la función
--     mayor :: Ord a => Monticulo a -> a
-- tal que (mayor m) es el mayor elemento del montículo m. Por ejemplo,
--     mayor (foldr inserta vacio [1,8,2,4,5])  ==  8
-- ---------------------------------------------------------------------

-- 1ª solución
mayor :: Ord a => Monticulo a -> a
mayor m | esVacio r = menor m
        | otherwise = mayor r
        where r = resto m

-- 2ª solución
mayor2 :: Ord a => Monticulo a -> a
mayor2 m = last (monticulo2Lista m)

monticulo2Lista :: Ord a => Monticulo a -> [a]
monticulo2Lista m | esVacio m = []
                  | otherwise = menor m : monticulo2Lista (resto m)

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función 
--    minMax :: Ord a => Monticulo a -> Maybe (a, a)
-- tal que (minMax m) es justamente el par formado por el menor y el
-- mayor elemento de m, si el montículo m es no vacío. Por ejemplo,
--    minMax (foldr inserta vacio [4,8,2,1,5])  ==  Just (1,8)
--    minMax (foldr inserta vacio [4])          ==  Just (4,4)
--    minMax vacio                              ==  Nothing
-- ---------------------------------------------------------------------

minMax :: (Ord a) => Monticulo a -> Maybe (a, a)
minMax m | esVacio m = Nothing
         | otherwise = Just (menor m,mayor m)

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Dada una lista de números naturales xs, la
-- codificación de Gödel de xs se obtiene multiplicando las potencias de
-- los primos sucesivos, siendo los exponentes los elementos de xs. Por
-- ejemplo, si xs = [6,0,4], la codificación de xs es 
--    2^6 * 3^0 * 5^4 = 64 * 1 * 625 = 40000.
-- 
-- Definir la función
--    codificaG :: [Integer] -> Integer
-- tal que (codificaG xs) es la codificación de Gödel de xs. Por
-- ejemplo, 
--    codificaG [6,0,4]           == 40000
--    codificaG [3,1,1]           == 120
--    codificaG [3,1,0,0,0,0,0,1] == 456
--    codificaG [1..6]            == 4199506113235182750
-- ---------------------------------------------------------------------

codificaG :: [Integer] -> Integer
codificaG xs = product (zipWith (^) primes xs)

-- Se puede eliminar el argumento:
codificaG2   :: [Integer] -> Integer
codificaG2 = product . zipWith (^) primes

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir la función 
--     decodificaG :: Integer -> [Integer]
-- tal que (decodificaG n) es la lista xs cuya codificación es n. Por
-- ejemplo,
--    decodificaG 40000               == [6,0,4]
--    decodificaG 120                 == [3,1,1]
--    decodificaG 456                 == [3,1,0,0,0,0,0,1]
--    decodificaG 4199506113235182750 == [1,2,3,4,5,6]
-- ---------------------------------------------------------------------

decodificaG :: Integer -> [Integer]
decodificaG n = aux primes (group $ primeFactors n)
  where aux _ [] = []
        aux (x:xs) (y:ys) | x == head y = genericLength y : aux xs ys
                          | otherwise   = 0 : aux xs (y:ys)

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Comprobar con QuickCheck que ambas funciones son
-- inversas. 
-- ---------------------------------------------------------------------

-- Las propiedades son
propCodifica1 :: [Integer] -> Bool
propCodifica1 xs = 
    decodificaG (codificaG ys) == ys
    where ys = map ((+1) . abs) xs

propCodifica2:: Integer -> Property
propCodifica2 n = 
    n > 0 ==> codificaG (decodificaG n) == n

-- Las comprobaciones son
--    ghci> quickCheck propCodifica1
--    +++ OK, passed 100 tests.
--
--    ghci> quickCheck propCodifica2
--    +++ OK, passed 100 tests.
