-- Informática (1º del Grado en Matemáticas)
-- 5º examen de evaluación continua (3 de mayo de 2016)
-- =====================================================================

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Numbers.Primes
import I1M.Cola
import I1M.Pol
import qualified Data.Matrix as M

-- ---------------------------------------------------------------------
-- Ejercicio 3. Dados dos polinomios P y Q, su producto de grado, que
-- notaremos P (o) Q, es el polinomio que se construye de la siguiente 
-- forma: para cada par de términos de P y Q con el mismo grado se
-- considera un término con dicho grado cuyo coeficiente es el producto
-- de los coeficientes de dichos términos. Se muestran algunos ejemplos
-- de producto de grado entre polinomios:
-- + Dos polinomios con un único término con el mismo grado
--      2*x^2 (o) 3*x^2 = 6*x^2 
-- + Dos polinomios entre los que no hay términos con el mismo grado
--      2*x^2 + 1 (o) 3*x^3 - x = 0
-- + Dos polinomios entre los que hay dos pares de términos con el mismo grado
--      2*x^2 + x + 2 (o) 3*x^2 - 4*x = 6*x^2 - 4*x
-- 
-- Definir la función 
--    productoDeGrado :: Polinomio Int -> Polinomio Int -> Polinomio Int
-- tal que (productoDeGrado p q) es el producto de grado de los polinomios
-- p y q. Por ejemplo, dados los polinomios
--    pol1  =  4*x^4 + 5*x^3 + 1
--    pol2  =  6*x^5 + 5*x^4 + 4*x^3 + 3*x^2 + 2*x + 1
--    pol3  =  -3*x^7 + 3*x^6 + -2*x^4 + 2*x^3 + -1*x + 1
--    pol4  =  -1*x^6 + 3*x^4 + -3*x^2 + 1
-- entonces
--    productoDeGrado pol1 pol1  =>  16*x^4 + 25*x^3 + 1
--    productoDeGrado pol1 pol2  =>  20*x^4 + 20*x^3 + 1
--    productoDeGrado pol1 pol3  =>  -8*x^4 + 10*x^3 + 1
--    productoDeGrado pol3 pol4  =>  -3*x^6 + -6*x^4 + 1
-- ---------------------------------------------------------------------

listaApol :: [Int] -> Polinomio Int
listaApol xs = foldr (\ (n,c) p -> consPol n c p) 
                     polCero 
                     (zip [0..] xs)

pol1, pol2, pol3, pol4 :: Polinomio Int
pol1 = listaApol [1,0,0,5,4,0]
pol2 = listaApol [1,2,3,4,5,6]
pol3 = listaApol [1,-1,0,2,-2,0,3,-3]
pol4 = listaApol [1,0,-3,0,3,0,-1]

productoDeGrado :: Polinomio Int -> Polinomio Int -> Polinomio Int
productoDeGrado p q
    | esPolCero p = polCero
    | esPolCero q = polCero
    | gp < gq     = productoDeGrado p rq
    | gq < gp     = productoDeGrado rp q
    | otherwise   = consPol gp (cp*cq) (productoDeGrado rp rq)
    where gp = grado p
          gq = grado q
          cp = coefLider p
          cq = coefLider q
          rp = restoPol p
          rq = restoPol q

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Una posición (i,j) de una matriz p es un cruce si
-- todos los elementos de p que están fuera de la fila i y fuera de la
-- columna j son nulos. Una matriz cruz es una matriz que tiene algún
-- cruce. Por ejemplo, las siguientes matrices cruces
--    ( 0 0 2 0 )      ( 0 0 1 0 0 )      ( 0 0 6 0 0 )
--    ( 3 3 4 2 )      ( 0 0 2 0 0 )      ( 0 0 2 0 0 )
--    ( 0 0 0 0 )      ( 3 3 1 2 0 )      ( 3 5 1 2 5 )
--    ( 0 0 9 0 )      ( 0 0 9 0 0 )
-- ya que la 1ª tiene un cruce en (2,3), la 2ª en (3,3) y la 3ª en
-- (3,3).
--
-- Utilizaremos la librería Matrix para desarrollar este ejercicio.
--
-- Definir la función
--    esMatrizCruz :: Matrix Int -> Bool
-- tal que (esMatrizCruz m) comprueba si la matriz m es una matriz cruz.
-- Por ejemplo, dadas las matrices
--    p1 = M.matrix 3 3 (\ (i,j) -> (if any even [i,j] then 1 else 0))
--    p2 = M.matrix 3 4 (\ (i,j) -> (i+j))
-- entonces
--    esMatrizCruz p1  ==  True
--    esMatrizCruz p2  ==  False
-- ---------------------------------------------------------------------

p1, p2 :: M.Matrix Int
p1 = M.matrix 3 3 (\ (i,j) -> (if any even [i,j] then 1 else 0))
p2 = M.matrix 3 4 (uncurry (+))

-- 1ª definición
-- =============

esMatrizCruz :: M.Matrix Int -> Bool
esMatrizCruz p =
    or [esCruce p m n (i,j) | i <- [1..m], j <- [1..n]]
    where m = M.nrows p
          n = M.ncols p

-- (esCruce p m n (i,j)) se verifica si (i,j) es un cruce de p (que
-- tiene m filas y n columnas)
esCruce :: M.Matrix Int -> Int -> Int -> (Int,Int) -> Bool
esCruce p m n (i,j) =
    all (== 0) [p M.! (x,y) | x <- [1..i-1]++[i+1..m], 
                              y <- [1..j-1]++[j+1..n]]

-- 2ª definición de esCruce
esCruce2 :: M.Matrix Int -> Int -> Int -> (Int,Int) -> Bool
esCruce2 p m n (i,j) =
    M.minorMatrix i j p == M.zero (m-1) (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función 
--    matrizCruz :: Int -> Int -> Int -> Int -> M.Matrix Int
-- tal que (matrizCruz m n i j) es la matriz cruz de dimensión (m,n) con
-- respecto a la posición (i,j), en la que el valor de cada elemento no
-- nulo es la distancia en línea recta a la posición (i,j), contando
-- también esta última. Por ejemplo,
--    ghci> matrizCruz 3 3 (2,2)
--    ( 0 2 0 )
--    ( 2 1 2 )
--    ( 0 2 0 )
--    
--    ghci> matrizCruz 4 5 (2,3)
--    ( 0 0 2 0 0 )
--    ( 3 2 1 2 3 )
--    ( 0 0 2 0 0 )
-


-    ( 0 0 3 0 0 )
--    
--    ghci> matrizCruz 5 3 (2,3)
--    ( 0 0 2 )
--    ( 3 2 1 )
--    ( 0 0 2 )
--    ( 0 0 3 )
--    ( 0 0 4 )
-- ---------------------------------------------------------------------

matrizCruz :: Int -> Int -> (Int,Int) -> M.Matrix Int
matrizCruz m n (i,j) =
    M.matrix m n generador
    where generador (a,b) 
              | a == i    = 1 + abs (j - b)
              | b == j    = 1 + abs (i - a)
              | otherwise = 0


