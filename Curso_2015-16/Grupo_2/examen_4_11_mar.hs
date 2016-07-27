-- Informática (1º del Grado en Matemáticas, Grupo 2)
-- 4º examen de evaluación continua (11 de marzo de 2016)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Librerías auxiliares                                               --
-- ---------------------------------------------------------------------

import Data.List 
import I1M.Cola
import Data.Array
import qualified Data.Matrix as M 
import Data.Set (fromList, notMember)

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. En la siguiente figura, al rotar girando 90 grados en
-- el sentido del reloj la matriz de la izquierda, obtenemos la de la
-- derecha
--    1 2 3        7 4 1
--    4 5 6        8 5 2
--    7 8 9        9 6 3
--
-- Definir la función
--    rota1 :: Array (Int,Int) Int  -> Array (Int,Int) Int
-- tal que (rota p) es la matriz obtenida girando en el sentido del
-- reloj la matriz cuadrada p. Por ejemplo,
--    ghci> elems (rota1 (listArray ((1,1),(3,3))[1..9]))
--    [7,4,1,8,5,2,9,6,3]
--    ghci> elems (rota1 (listArray ((1,1),(3,3))[7,4,1,8,5,2,9,6,3]))
--    [9,8,7,6,5,4,3,2,1]
-- ---------------------------------------------------------------------

rota1 :: Array (Int,Int) Int  -> Array (Int,Int) Int
rota1 p =
    array ((1,1),(n,m)) [((j,n+1-i),p!(i,j)) | i <- [1..m], j <- [1..n]]
    where (_,(m,n)) = bounds p

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función
--    rota2 :: M.Matrix Int  -> M.Matrix Int
-- tal que (rota p) es la matriz obtenida girando en el sentido del
-- reloj la matriz cuadrada p. Por ejemplo,
--    ghci> rota2 (M.fromList 3 3 [1..9])
--    ( 7 4 1 )
--    ( 8 5 2 )
--    ( 9 6 3 )
--    
--    ghci> rota2 (M.fromList 3 3 [7,4,1,8,5,2,9,6,3])
--    ( 9 8 7 )
--    ( 6 5 4 )
--    ( 3 2 1 )
-- ---------------------------------------------------------------------

rota2 :: M.Matrix Int  -> M.Matrix Int
rota2 p = M.matrix n m (\(i,j) -> p M.! (n+1-j,i))
    where m = M.nrows p
          n = M.ncols p
     
-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Se dice que un número tiene una bajada cuando existe
-- un par de dígitos (a,b) tales que b está a la derecha de a y b es
-- menor que a. Por ejemplo, 4312 tiene 5 bajadas ((4,3), (4,1), (4,2),
-- (3,1) y (3,2)).
--
-- Definir la función
--    bajadas :: Int -> Int
-- tal que (bajadas n) es el número de bajadas de n. Por ejemplo,
--    bajadas 4312  == 5
--    bajadas 2134  == 1
-- ---------------------------------------------------------------------

bajadas :: Int -> Int
bajadas n = sum (map aux (tails (show n)))
    where aux []     = 0
          aux (x:xs) = length (filter (<x) xs)

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Calcular cuántos números hay de 4 cifras con más de
-- dos bajadas.
-- ---------------------------------------------------------------------

-- El cálculo es
--    ghci> length [n | n <- [1000..9999], bajadas n > 2]
--    5370

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    penultimo :: Cola a -> a
-- tal que (penultimo c) es el penúltimo elemento de la cola c. Si
-- la cola esta vacía o tiene un sólo elemento, dará el error
-- correspondiente, "cola vacia" o bien "cola unitaria". Por ejemplo, 
--    ghci> penultimo vacia
--    *** Exception: cola vacia
--    ghci> penultimo (inserta 2 vacia)
--    *** Exception: cola unitaria
--    ghci> penultimo (inserta 3 (inserta 2 vacia))
--    2
--    ghci> penultimo (inserta 5 (inserta 3 (inserta 2 vacia)))
--    3
-- ---------------------------------------------------------------------

penultimo :: Cola a -> a
penultimo c 
    | esVacia c   = error "cola vacia"
    | esVacia rc  = error "cola unitaria"
    | esVacia rrc = primero c
    | otherwise   = penultimo rc
    where rc  = resto c
          rrc = resto rc     

-- ---------------------------------------------------------------------
-- Ejercicio 4. Sea xs una lista y n su longitud. Se dice que xs es casi
-- completa si sus elementos son los numeros enteros entre 0 y n excepto
-- uno. Por ejemplo, la lista [3,0,1] es casi completa. 
-- 
-- Definir la función
--    ausente :: [Integer] -> Integer
-- tal que (ausente xs) es el único entero (entre 0 y la longitud de xs)
-- que no pertenece a la lista casi completa xs. Por ejemplo,
--    ausente [3,0,1]               ==  2
--    ausente [1,2,0]               ==  3
--    ausente (1+10^7:[0..10^7-1])  ==  10000000
-- --------------------------------------------------------------------- 

-- 1ª definición
ausente1 :: [Integer] -> Integer
ausente1 xs =
    head [n | n <- [0..], n `notElem` xs]

-- 2ª definición
ausente2 :: [Integer] -> Integer
ausente2 xs =
    head [n | n <- [0..], n `notMember` ys]
    where ys = fromList xs
         
-- 3ª definición (lineal)
ausente3 :: [Integer] -> Integer
ausente3 xs =
    ((n * (n+1)) `div` 2) - sum xs
    where n = genericLength xs  

-- 4ª definición
ausente4 :: [Integer] -> Integer
ausente4 xs =
    ((n * (n+1)) `div` 2) - foldl' (+) 0 xs
    where n = genericLength xs  

-- Comparación de eficiencia
-- =========================

--    ghci> let n = 10^5 in ausente1 (n+1:[0..n-1])
--    100000
--    (68.51 secs, 25,967,840 bytes)
--    ghci> let n = 10^5 in ausente2 (n+1:[0..n-1])
--    100000
--    (0.12 secs, 123,488,144 bytes)
--    ghci> let n = 10^5 in ausente3 (n+1:[0..n-1])
--    100000
--    (0.07 secs, 30,928,384 bytes)
--    ghci> let n = 10^5 in ausente4 (n+1:[0..n-1])
--    100000
--    (0.02 secs, 23,039,904 bytes)
--    
--    ghci> let n = 10^7 in ausente2 (n+1:[0..n-1])
--    10000000
--    (14.32 secs, 15,358,509,280 bytes)
--    ghci> let n = 10^7 in ausente3 (n+1:[0..n-1])
--    10000000
--    (5.57 secs, 2,670,214,936 bytes)
--    ghci> let n = 10^7 in ausente4 (n+1:[0..n-1])
--    10000000
--    (3.36 secs, 2,074,919,184 bytes)
