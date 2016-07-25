-- Informática: 6º examen de evaluación continua (15 de junio de 2015)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Numbers.Primes
import Data.List
import I1M.PolOperaciones
import Test.QuickCheck
import Data.Array
import Data.Char
import Data.Matrix

-- ---------------------------------------------------------------------
-- Ejercicio 1. Sea p(n) el n-ésimo primo y sea r el resto de dividir 
-- (p(n)-1)^n + (p(n)+1)^n por p(n)^2. Por ejemplo, 
--    si n = 3, entonces p(3) =  5 y r = ( 4^3 +  6^3) mod  (5^2) =   5
--    si n = 7, entonces p(7) = 17 y r = (16^7 + 18^7) mod (17^2) = 238
-- 
-- Definir la función 
--    menorPR :: Integer -> Integer 
-- tal que (menorPR x) es el menor n tal que el resto de dividir 
-- (p(n)-1)^n + (p(n)+1)^n por p(n)^2 es mayor que x. Por ejemplo,
--    menorPR 100     == 5
--    menorPR 345     == 9
--    menorPR 1000    == 13
--    menorPR (10^9)  == 7037.
--    menorPR (10^10) == 21035
--    menorPR (10^12) == 191041
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

menorPR1 :: Integer -> Integer
menorPR1 x =  
    head [n | (n,p) <- zip [1..] primes
            , (((p-1)^n + (p+1)^n) `mod` (p^2)) > x]

-- Segunda solución (usando el binomio de Newton)
-- ==============================================

-- Desarrollando por el binomio de Newton
--    (p+1)^n = C(n,0)p^n + C(n,1)p^(n-1) +...+ C(n,n-1)p + 1
--    (p-1)^n = C(n,0)p^n - C(n,1)p^(n-1) +...+ C(n,n-1)p + (-1)^n
-- Sumando se obtiene (según n sea par o impar)
--    2*C(n,0)p^n + 2*C(n,n-2)p^(n-1) +...+ 2*C(n,2)p^2 + 2
--    2*C(n,0)p^n + 2*C(n,n-2)p^(n-1) +...+ 2*C(n,1)p^1
-- Al dividir por p^2, el resto es (según n sea par o impar) 2 ó 2*C(n,1)p

-- (restoM n p) es el resto de de dividir (p-1)^n + (p+1)^n por p^2.
restoM :: Integer -> Integer -> Integer
restoM n p | even n    = 2
           | otherwise = 2*n*p `mod`(p^2)

menorPR2 :: Integer -> Integer
menorPR2 x = head [n | (n,p) <- zip [1..] primes, restoM n p > x]

-- Comparación de eficiencia
--    ghci> menorPR1 (3*10^8)
--    3987
--    (2.44 secs, 120291676 bytes)
--    ghci> menorPR2 (3*10^8)
--    3987
--    (0.04 secs, 8073900 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    sumaPosteriores :: [Int] -> [Int]
-- tal que (sumaPosteriores xs) es la lista obtenida sustituyendo cada
-- elemento de xs por la suma de los elementos posteriores. Por ejemplo,
--    sumaPosteriores [1..8]        == [35,33,30,26,21,15,8,0]
--    sumaPosteriores [1,-3,2,5,-8] == [-4,-1,-3,-8,0]
-- 
-- Comprobar con QuickCheck que el último elemento de la lista
-- (sumaPosteriores xs) siempre es 0.
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión):
sumaPosteriores1 :: [Int] -> [Int]
sumaPosteriores1 []     = []
sumaPosteriores1 (x:xs) = sum xs : sumaPosteriores1 xs

-- 2ª definición (sin argumentos)
sumaPosteriores2 :: [Int] -> [Int]
sumaPosteriores2 = map sum . tail . tails

-- 3ª definición (con scanr)
sumaPosteriores3 :: [Int] -> [Int]
sumaPosteriores3 = tail . scanr (+) 0

-- La propiedad es
propSumaP:: [Int] -> Property
propSumaP xs = not (null xs) ==> last (sumaPosteriores1 xs) == 0

-- La comprobación es
--    ghci> quickCheck propSumaP
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la constante 
--    sucesionD :: String
-- tal que su valor es la cadena infinita "1234321234321234321..."
-- formada por la repetición de los dígitos 123432. Por ejemplo,
--    ghci> take 50 sucesionD
--    "12343212343212343212343212343212343212343212343212"
-- ---------------------------------------------------------------------

-- 1ª definición (con cycle):
sucesionD :: String
sucesionD = cycle "123432"

-- 2ª definición (con repeat):
sucesionD2 :: String
sucesionD2 = concat $ repeat "123432"

-- 3ª definición (por recursión):
sucesionD3 :: String
sucesionD3 = "123432" ++ sucesionD4

-- Comparación de eficiencia
--    ghci> sucesionD !! (2*10^7)
--    '3'
--    (0.16 secs, 1037132 bytes)
--    ghci> sucesionD2 !! (2*10^7)
--    '3'
--    (3.28 secs, 601170876 bytes)
--    ghci> sucesionD3 !! (2*10^7)
--    '3'
--    (0.17 secs, 1033344 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. La sucesión anterior se puede partir en una sucesión
-- de números, de forma que la suma de los dígitos de dichos números
-- forme la sucesión de los números naturales, como se observa a
-- continuación: 
--     1, 2, 3, 4, 32, 123, 43, 2123, 432, 1234, 32123, ...
--     1, 2, 3, 4,  5,   6,  7,    8,   9,   10,    11, ...
--
-- Definir la sucesión 
--    sucesionN :: [Integer]
-- tal que sus elementos son los números de la partición anterior. Por
-- ejemplo, 
--    ghci> take 11 sucesionN
--    [1,2,3,4,32,123,43,2123,432,1234,32123]
-- ---------------------------------------------------------------------

sucesionN :: [Int]
sucesionN = aux [1..] sucesionD
    where aux (n:ns) xs = read ys : aux ns zs
              where (ys,zs) = prefijoSuma n xs

-- (prefijoSuma n xs) es el par formado por el primer prefijo de xs cuyo
-- suma es n y el resto de xs. Por ejemplo,
--    prefijoSuma 6 "12343"  ==  ("123","43")
prefijoSuma :: Int -> String -> (String,String)
prefijoSuma n xs = 
    head [(us,vs) | (us,vs) <- zip (inits xs) (tails xs)
                  , sumaD us == n]

-- (sumaD xs) es la suma de los dígitos de xs. Por ejemplo,
--    sumaD "123"  ==  6
sumaD :: String -> Int
sumaD = sum . map digitToInt

-- ---------------------------------------------------------------------
-- Ejercicio 4. El polinomio cromático de un grafo calcula el número de
-- maneras en las cuales puede ser coloreado el grafo usando un número
-- de colores dado, de forma que dos vértices adyacentes no tengan el
-- mismo color.  
-- 
-- En el caso del grafo completo de n vértices, su polinomio cromático
-- es P(n,x) = x(x-1)(x-2) ... (x-(n-1)). Por ejemplo, 
--    P(3,x) = x(x-1)(x-2)      = x^3 - 3*x^2 + 2*x
--    P(4,x) = x(x-1)(x-2)(x-3) = x^4 - 6*x^3 + 11*x^2 - 6*x
-- Lo que significa que P(4)(x) es el número de formas de colorear el
-- grafo completo de 4 vértices con x colores. Por tanto, 
--    P(4,2) =  0 (no se puede colorear con 2 colores)
--    P(4,4) = 24 (hay 24 formas de colorearlo con 4 colores)
--
-- Definir la función 
--      polGC:: Int -> Polinomio Int
-- tal que (polGC n) es el polinomio cromático del grafo completo de n
-- vértices. Por ejemplo,
--      polGC 4  ==  x^4 + -6*x^3 + 11*x^2 + -6*x
--      polGC 5  ==  x^5 + -10*x^4 + 35*x^3 + -50*x^2 + 24*x
-- 
-- Comprobar con QuickCheck que si el número de colores (x) coincide con
-- el número de vértices del grafo (n), el número de maneras de colorear
-- el grafo es n!.  
-- 
-- Nota. Al hacer la comprobación limitar el tamaño de las pruebas como
-- se indica a continuación
--    ghci> quickCheckWith (stdArgs {maxSize=7}) prop_polGC
--    +++ OK, passed 100 tests.
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

polGC :: Int -> Polinomio Int
polGC 0 = consPol 0 1 polCero
polGC n = multPol (polGC (n-1)) (consPol 1 1 (consPol 0 (-n+1) polCero))

-- 2ª solución
-- ===========

polGC2 :: Int -> Polinomio Int
polGC2 n = multLista (map polMon [0..n-1])

-- (polMon n) es el monomio x-n. Por ejemplo,
--    polMon 3  ==  1*x + -3
polMon:: Int -> Polinomio Int
polMon n = consPol 1 1 (consPol 0 (-n) polCero)

-- (multLista ps) es el producto de la lista de polinomios ps.
multLista :: [Polinomio Int] -> Polinomio Int
multLista []     = polUnidad
multLista (p:ps) = multPol p (multLista ps)

-- La función multLista se puede definir por plegado
multLista2 :: [Polinomio Int] -> Polinomio Int
multLista2 = foldr multPol polUnidad

-- La propiedad es
prop_polGC :: Int -> Property
prop_polGC n = 
    n > 0 ==> valor (polGC n) n == product [1..n]

-- La comprobación es
--    ghci> quickCheckWith (stdArgs {maxSize=7}) prop_polGC
--    +++ OK, passed 100 tests.
--    (0.04 secs, 7785800 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 5: 

-- Consideramos un tablero de ajedrez en el que hay un único caballo y
-- lo representamos por una matriz con ceros en todas las posiciones,
-- excepto en la posición del caballo que hay un 1.

-- Definimos el tipo de las matrices:

type Matriz a = Array (Int,Int) a

-- (a) Definir una función 
--     matrizC:: (Int,Int) -> Matriz Int
-- tal que, dada la posición (i,j) donde está el caballo, obtiene la
-- matriz correspondiente. Por ejemplo, 
--   elems (matrizC (1,1))
--  [1,0,0,0,0,0,0,0,
--   0,0,0,0,0,0,0,0,
--   0,0,0,0,0,0,0,0,
--   0,0,0,0,0,0,0,0,
--   0,0,0,0,0,0,0,0,
--   0,0,0,0,0,0,0,0,
--   0,0,0,0,0,0,0,0,
--   0,0,0,0,0,0,0,0]

matrizC:: (Int,Int) -> Matrix Int
matrizC (i,j) = setElem 1 (i,j) (zero 8 8)

-- (b) Definir una función 
--     posicionesC :: (Int,Int) -> [(Int,Int)]
-- tal que dada la posición (i,j) de un caballo, obtiene la lista
-- con las posiciones posibles a las que se puede mover el caballo.
-- Por ejemplo,
-- posicionesC (1,1) == [(2,3),(3,2)]
-- posicionesC (3,4) == [(2,2),(2,6),(4,2),(4,6),(1,3),(1,5),(5,3),(5,5)]

posicionesC :: (Int,Int) -> [(Int,Int)]
posicionesC (i,j) = 
    filter p [(i-1,j-2),(i-1,j+2),(i+1,j-2),(i+1,j+2),
              (i-2,j-1),(i-2,j+1),(i+2,j-1),(i+2,j+1)]
    where p (x,y) = x >= 1 && x <= 8 && y >= 1 && y <= 8

-- (c) Definir una función 
--     saltoC:: Matriz Int -> (Int,Int) -> [Matriz Int]
-- tal que, dada una matriz m con un caballo en la posición (i,j),
-- obtiene la lista con las matrices que representan cada una de los
-- posibles movimientos del caballo.

saltoC:: Matrix Int -> (Int,Int) -> [Matrix Int]
saltoC m (i,j) = map matrizC (posicionesC (i,j))

-- o bien, sin usar matrizC

saltoC':: Matrix Int -> (Int,Int) -> [Matrix Int]
saltoC' m (i,j) = map f (posicionesC (i,j))
    where f (k,l) = setElem 0 (i,j) (setElem 1 (k,l) m)

-- También se puede definir obviando la matriz:

saltoCI:: (Int,Int) -> [Matrix Int]
saltoCI = map matrizC . posicionesC


-- saltoC m1 (1,1)
-- ( 0 0 0 0 0 0 0 0 )
-- ( 0 0 1 0 0 0 0 0 )
-- ( 0 0 0 0 0 0 0 0 )
-- ( 0 0 0 0 0 0 0 0 )
-- ( 0 0 0 0 0 0 0 0 )
-- ( 0 0 0 0 0 0 0 0 )
-- ( 0 0 0 0 0 0 0 0 )
-- ( 0 0 0 0 0 0 0 0 )
-- ,
-- ( 0 0 0 0 0 0 0 0 )
-- ( 0 0 0 0 0 0 0 0 )
-- ( 0 1 0 0 0 0 0 0 )
-- ( 0 0 0 0 0 0 0 0 )
-- ( 0 0 0 0 0 0 0 0 )
-- ( 0 0 0 0 0 0 0 0 )
-- ( 0 0 0 0 0 0 0 0 )
-- ( 0 0 0 0 0 0 0 0 )

-- (d) Definir una función 
--      juego:: IO () 
-- que realice lo siguiente: pregunte por la posición del caballo en el
-- tablero y por la casilla hacia la que queremos moverlo y nos devuelva
-- en pantalla "Correcta" o "Incorrecta". Por ejemplo,

--   juego
--   Introduce la posición actual del caballo 
--   fila: 1
--   columna: 1
--   Introduce la posición hacia la que quieres moverlo 
--   fila: 4
--   columna: 2
--   Incorrecta

--   juego
--   Introduce la posición actual del caballo 
--   fila: 3
--   columna: 4
--   Introduce la posición hacia la que quieres moverlo 
--   fila: 1
--   columna: 5
--   Correcta

juego :: IO ()
juego = do putStrLn "Introduce la posición actual del caballo "
           putStr "fila: "
           a <- getLine
           let i = read a
           putStr "columna: " 
           b <- getLine
           let j = read b
           putStrLn "Introduce la posición hacia la que quieres moverlo "
           putStr "fila: "
           a2 <- getLine
           let k = read a2
           putStr "columna: " 
           b2 <- getLine
           let l = read b
           putStrLn (if (k,l) `elem` posicionesC (i,j) 
                     then "Correcta" 
                     else "Incorrecta")
            
-- ================================================
-- Ejercicio 5: Con Array. Matrices. Entrada/salida
-- ================================================

-- Los mismos ejercicios, pero usando Array en vez de la librería de
-- matrices.

matrizC2:: (Int,Int) -> Array (Int,Int) Int
matrizC2 (i,j) = array ((1,1), (8,8)) [((k,l), f (k,l)) | k <-[1..8],
                                                          l <- [1..8]]
    where f (k,l) | (k,l) == (i,j) = 1
                  | otherwise      = 0

-- Ejemplo:
m1_2:: Array (Int,Int) Int
m1_2 = matrizC2 (1,1)

saltoC2:: Array (Int,Int) Int -> (Int,Int) -> [Array (Int,Int) Int]
saltoC2 m (i,j) = map matrizC2 (posicionesC (i,j))

saltoCI2:: (Int,Int) -> [Array (Int,Int) Int]
saltoCI2 = map matrizC2 . posicionesC
