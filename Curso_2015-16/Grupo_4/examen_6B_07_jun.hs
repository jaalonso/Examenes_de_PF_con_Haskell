-- Informática (1º del Grado en Matemáticas)
-- 6º examen de evaluación continua (7 de junio de 2016)
-- ---------------------------------------------------------------------

-- Puntuación: Cada uno de los 5 ejercicios vale 2 puntos.

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Numbers.Primes
import Data.List
import I1M.Grafo
import I1M.PolOperaciones
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función
--    esSumaP :: Integer -> Integer -> Bool
-- tal que (esSumaP k n) se verifica si n es suma de k primos (no
-- necesariamente distintos). Por ejemplo,
--    esSumaP  3 10  ==  True
--    esSumaP  3  2  ==  False
--    esSumaP 10 21  ==  True
--    esSumaP 21 10  ==  False
--    take 3 [n | n <- [1..], esSumaP 18 n]  == [36,37,38]
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

esSumaP1 :: Integer -> Integer -> Bool
esSumaP1 k n = 
    n `elem` [sum xs | xs <- combinacionesR k (takeWhile (<=n) primes)]

-- (combinacionesR k xs) es la lista de las combinaciones orden k de los
-- elementos de xs con repeticiones. Por ejemplo, 
--    ghci> combinacionesR 2 "abc"
--    ["aa","ab","ac","bb","bc","cc"]
--    ghci> combinacionesR 3 "bc"
--    ["bbb","bbc","bcc","ccc"]
--    ghci> combinacionesR 3 "abc"
--    ["aaa","aab","aac","abb","abc","acc","bbb","bbc","bcc","ccc"]
combinacionesR :: Integer -> [a] -> [[a]]
combinacionesR _ [] = []
combinacionesR 0 _  = [[]]
combinacionesR k (x:xs) =
    [x:ys | ys <- combinacionesR (k-1) (x:xs)] ++ combinacionesR k xs

-- 2ª definición
-- =============

esSumaP2 :: Integer -> Integer -> Bool
esSumaP2 k n = esSuma k n (takeWhile (<=n) primes)
            
esSuma :: Integer -> Integer -> [Integer] -> Bool
esSuma 0 x _  = x == 0
esSuma k 0 _  = k == 0
esSuma _ _ [] = False
esSuma k x (y:ys)
    | x < y     = False
    | otherwise = esSuma (k-1) (x-y) (y:ys) || esSuma k x ys  
                           
-- 3ª definición
-- =============

esSumaP3 :: Integer -> Integer -> Bool
esSumaP3 1 n = isPrime n
esSumaP3 k n = any (esSumaP3 (k-1)) (map (n-) (takeWhile (<n) primes))

-- Equivalencia
-- ============

prop_equiv_esSumaP :: Positive Integer -> Bool
prop_equiv_esSumaP (Positive x) =
    esSumaP2 3 x == v
    && esSumaP2 3 x == v
    where v = esSumaP1 3 x
                      
-- La comprobación es
--    ghci> quickCheck prop_equiv_esSumaP
--    +++ OK, passed 100 tests.

-- Eficiencia
-- ==========

--    ghci> [x | x <- [1..], esSumaP1 10 x] !! 700
--    720
--    (3.94 secs, 3,180,978,848 bytes)
--    ghci> [x | x <- [1..], esSumaP2 10 x] !! 700
--    720
--    (0.76 secs, 410,235,584 bytes)
--    ghci> [x | x <- [1..], esSumaP3 10 x] !! 700
--    720
--    (0.10 secs, 102,200,384 bytes)
--
--    ghci> take 3 [n | n <- [1..], esSumaP2 18 n]
--    [36,37,38]
--    (0.02 secs, 0 bytes)
--    ghci> take 3 [n | n <- [1..], esSumaP3 18 n]
--    [36,37,38]
--    (2.91 secs, 5,806,553,024 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que el menor número n para el
-- que se cumple la condición (esSumaP k n) es 2*k.
--
-- Nota. Al hacer la comprobación limitar el tamaño de las pruebas como
-- se indica a continuación
--    ghci> quickCheckWith (stdArgs {maxSize=7}) prop_menor_esSuma
--    +++ OK, passed 100 tests.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_menor_esSuma :: Integer -> Integer -> Property
prop_menor_esSuma k n =
    k > 0 && n > 0 ==>
      head [x | x <- [1..], esSumaP3 k x] == 2*k

-- La comprobación es
--    ghci> quickCheckWith (stdArgs {maxSize=7}) prop_menor_esSuma
--    +++ OK, passed 100 tests.
--    (0.03 secs, 0 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Se consideran los árboles binarios representados
-- mediante el tipo Arbol definido por 
--    data Arbol a = H a
--                 | N a (Arbol a) (Arbol a)
--                   deriving (Show,Eq)
-- Por ejemplo, el árbol
--         9
--        / \
--       /   \
--      3     8
--     / \   
--    2   4 
--       / \
--      1   5
-- se puede representar por
--    a1 :: Arbol Int
--    a1 = N 9 (N 3 (H 2) (N 4 (H 1) (H 5))) (H 8)
--
-- En el árbol a1 se cumple que todas sus ramas tiene un número par; 
-- pero no se cumple que todas sus ramas tenga un número primo.
--
-- Definir la función 
--    propiedadAE :: (a -> Bool) -> Arbol a -> Bool
-- tal que (propiedadAE p a) se verifica si en todas las ramas de a hay
-- algún nodo que cumple la propiedad p. Por ejemplo,
--    propiedadAE even a1  == True
--    propiedadAE (<7) a1  == False
-- ---------------------------------------------------------------------

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
             deriving (Show,Eq)
 
a1 :: Arbol Int
a1 = N 9 (N 3 (H 2) (N 4 (H 1) (H 5))) (H 8)

-- 1ª definición
propiedadAE :: (a -> Bool) -> Arbol a -> Bool
propiedadAE p (H x)     = p x
propiedadAE p (N x i d) = p x || (propiedadAE p i && propiedadAE p d)

-- 2ª definición
propiedadAE2 :: (a -> Bool) -> Arbol a -> Bool
propiedadAE2 p x = all (any p) (ramas x)

ramas :: Arbol a => [[a]]
ramas (H x)     = [[x]]
ramas (N x i d) = map (x:) (ramas i ++ ramas d)

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función 
--    propiedadEA :: (a -> Bool) -> Arbol a -> Bool
-- tal que (propiedadEA p a) se verifica si el árbol a tiene alguna rama
-- en la que todos sus nodos cumplen la propiedad p. Por ejemplo,
--    propiedadEA (>0) a1  == True
--    propiedadEA even a1  == False
--    propiedadEA (>7) a1  == True
-- ---------------------------------------------------------------------

-- 1ª definición
propiedadEA :: (a -> Bool) -> Arbol a -> Bool
propiedadEA p (H x)     = p x
propiedadEA p (N x i d) = p x && (propiedadEA p i || propiedadEA p d)

-- 2ª definición
propiedadEA2 :: (a -> Bool) -> Arbol a -> Bool
propiedadEA2 p x = any (all p) (ramas x)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Un clique de un grafo no dirigido G es un conjunto de
-- vértices V tal que el subgrafo de G inducido por V es un grafo
-- completo. Por ejemplo, en el grafo, 
--    6 
--     \
--      4 ---- 5 
--      |      | \
--      |      |  1
--      |      | /
--      3 ---- 2   
-- el conjunto de vértices {1,2,5} es un clique y el conjunto {2,3,4,5}
-- no lo es.
-- 
-- En Haskell se puede representar el grafo anterior por
--    g1 :: Grafo Int Int
--    g1 = creaGrafo ND
--                   (1,6) 
--                   [(1,2,0),(1,5,0),(2,3,0),(3,4,0),(5,2,0),(4,5,0),(4,6,0)]
-- 
-- Definir la función
--    esClique :: Grafo Int Int -> [Int] -> Bool
-- tal que (esClique g xs) se verifica si xs es un clique de g. Por
-- ejemplo, 
--    esClique g1 [1,2,5]   == True
--    esClique g1 [2,3,4,5] == False
-- ---------------------------------------------------------------------

g1 :: Grafo Int Int
g1 = creaGrafo ND
               (1,6) 
               [(1,2,0),(1,5,0),(2,3,0),(3,4,0),(5,2,0),(4,5,0),(4,6,0)]

esClique :: Grafo Int Int -> [Int] -> Bool
esClique g xs = all (aristaEn g) [(x,y) | x <- ys, y <- ys, y < x]
    where ys = sort xs

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la función
--    polNumero :: Integer -> Polinomio Integer
-- tal que (polNumero n) es el polinomio cuyos coeficientes son los
-- dígitos de n. Por ejemplo, si n = 5703, el polinomio es
-- 5x^3 + 7x^2 + 3. En Haskell,
--    ghci> polNumero 5703
--    5*x^3 + 7*x^2 + 3
-- ---------------------------------------------------------------------

polNumero :: Integer -> Polinomio Integer
polNumero n = aux (zip [0..] (reverse (digitos n)))
    where aux []         = polCero
          aux ((m,a):ps) = consPol m a (aux ps)

digitos :: Integer -> [Integer]
digitos n = [read [x]| x <- show n]

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función 
--    zeta :: Integer -> Int
-- tal que (zeta n) es el número de enteros positivos k <= n, tales que 
-- el polinomio (polNumero k) tiene alguna raíz entera. Por ejemplo,
--    zeta 100    == 33
--    zeta 100000 == 14696
-- ---------------------------------------------------------------------

zeta :: Integer -> Int
zeta n = length [k | k <- [1..n], tieneRaizEntera k]                   

tieneRaizEntera :: Integer -> Bool
tieneRaizEntera n = or [esRaiz c p | c <- ds]
    where p  = polNumero n
          t  = n `mod` 10
          ds = 0 : divisores t

divisores :: Integer -> [Integer]
divisores n = concat [[x,-x] | x <- [1..n], rem n x == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Comprobar con QuickCheck las siguientes propiedades:
-- + El valor de (polNumero n) en 0 es igual al último dígito de n
-- + El valor de (polNumero n) en 1 es igual a la suma de los dígitos de n.
-- + El valor de (polNumero n) en 10 es igual a n.
-- ---------------------------------------------------------------------
              
-- La propiedad es
prop_polNumero :: Integer -> Property
prop_polNumero n =
    n > 0 ==> valor (polNumero n) 0 == last ds &&
              valor (polNumero n) 1 == sum ds && 
              valor (polNumero n) 10 == n
    where ds = digitos n

-----------------------------------------------------------------------
-- Ejercicio 5.1. Para cada número n con k dígitos se define una sucesión
-- de tipo Fibonacci cuyos k primeros elementos son los dígitos de n y
-- los siguientes se obtienen sumando los k anteriores términos de la
-- sucesión. Por ejemplo, la sucesión definida por 197 es
--    1, 9, 7, 17, 33, 57, 107, 197, ...
--
-- Definir la función
--    fibGen :: Integer -> [Integer]
-- tal que (fibGen n) es la sucesión de tipo Fibonacci definida por
-- n. Por ejemplo,
--    ghci> take 10 (fibGen 197)
--    [1,9,7,17,33,57,107,197,361,665]
-- ---------------------------------------------------------------------

-- 1ª definición
fibGen1 :: Integer -> [Integer]
fibGen1 n = suc
    where ds     = digitos n
          k      = genericLength ds
          aux xs = sum (take k xs) : aux (tail xs)
          suc    = ds ++ aux suc

-- 2ª definición
fibGen2 :: Integer -> [Integer]
fibGen2 n = ds ++ map head (tail sucLista)
    where ds       = digitos n
          sig xs   = sum xs : init xs
          sucLista = iterate sig (reverse ds)
          
-- 3ª definición
fibGen3 :: Integer -> [Integer]
fibGen3 n = ds ++ map last (tail sucLista)
    where ds       = digitos n
          sig' xs  = tail xs ++ [sum xs]
          sucLista = iterate sig' ds

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Un número n > 9 es un número de Keith si n aparece en
-- la sucesión de tipo Fibonacci definida por n. Por ejemplo, 197 es un
-- número de Keith.
--
-- Definir la función
--    esKeith :: Integer -> Bool
-- tal que (esKeith n) se verifica si n es un número de Keith. Por
-- ejemplo, 
--    esKeith 197   == True
--    esKeith 54798 == False
-- ---------------------------------------------------------------------

esKeith :: Integer -> Bool
esKeith n = n == head (dropWhile (<n) $ fibGen1 n)

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Calcular todos los número se Keith con 5 dígitos.
-- ---------------------------------------------------------------------

-- El cálculo es
--    ghci> filter esKeith [10^4..10^5-1]
--    [31331,34285,34348,55604,62662,86935,93993]

