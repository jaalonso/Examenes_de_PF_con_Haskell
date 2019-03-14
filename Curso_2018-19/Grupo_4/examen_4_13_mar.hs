-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 4º examen de evaluación continua (13 de marzo de 2019)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Array
import Test.QuickCheck
import Text.Printf
import Data.List
import System.Timeout

-- ---------------------------------------------------------------------
-- Ejercicio 1 [2.5 puntos] Los árboles binarios se pueden representar
-- con 
--    data Arbol a = H a
--                 | Nodo a (Arbol a) (Arbol a)
--      deriving (Show, Eq)
--
-- Definir la función
--    arboles  :: Integer -> a -> [Arbol a]
-- tales que (arboles n x) es la lista de todos los árboles binarios con
-- n elementos iguales a x. Por ejemplo,
--    λ> arboles 0 7
--    []
--    λ> arboles 1 7
--    [H 7]
--    λ> arboles 2 7
--    []
--    λ> arboles 3 7
--    [Nodo 7 (H 7) (H 7)]
--    λ> arboles 4 7
--    []
--    λ> arboles 5 7
--    [Nodo 7 (H 7) (Nodo 7 (H 7) (H 7)),
--     Nodo 7 (Nodo 7 (H 7) (H 7)) (H 7)]
--    λ> arboles 6 7
--    []
--    λ> arboles 7 7
--    [Nodo 7 (H 7) (Nodo 7 (H 7) (Nodo 7 (H 7) (H 7))),
--     Nodo 7 (H 7) (Nodo 7 (Nodo 7 (H 7) (H 7)) (H 7)),
--     Nodo 7 (Nodo 7 (H 7) (H 7)) (Nodo 7 (H 7) (H 7)),
--     Nodo 7 (Nodo 7 (H 7) (Nodo 7 (H 7) (H 7))) (H 7),
--     Nodo 7 (Nodo 7 (Nodo 7 (H 7) (H 7)) (H 7)) (H 7)]
-- ---------------------------------------------------------------------

data Arbol a = H a
             | Nodo a (Arbol a) (Arbol a)
  deriving (Show, Eq)

-- 1ª definición
-- =============

arboles :: Integer -> a -> [Arbol a]
arboles 0 _ = []
arboles 1 x = [H x]
arboles n x = [Nodo x i d | k <- [0..n-1],
                            i <- arboles k x,
                            d <- arboles (n-1-k) x]

-- 2ª definición
-- =============

arboles2 :: Integer -> a -> [Arbol a]
arboles2 0 _ = []
arboles2 1 x = [H x]
arboles2 n x = [Nodo x i d | k <- [1,3..n-1],
                             i <- arboles2 k x,
                             d <- arboles2 (n-1-k) x]

-- ---------------------------------------------------------------------
-- Ejercicio 2. [2.5 puntos]. Un camino es una sucesión de pasos en una
-- de las cuatros direcciones Norte, Sur, Este, Oeste. Ir en una
-- dirección y a continuación en la opuesta es un esfuerzo que se puede
-- reducir. Por ejemplo, el camino [Norte,Sur,Este,Sur] se puede reducir
-- a [Este,Sur]. 
--
-- Un camino se dice que es reducido si no tiene dos pasos consecutivos
-- en direcciones opuesta. 
--
-- En Haskell, las direcciones y los caminos se pueden definir por
--    data Direccion = N | S | E | O deriving (Show, Eq)
--    type Camino = [Direccion]
--
-- Definir la función 
--    reducido :: Camino -> Camino
-- tal que (reducido ds) es el camino reducido equivalente al camino
-- ds. Por ejemplo,
--    reducido []                              ==  []
--    reducido [N]                             ==  [N]
--    reducido [N,O]                           ==  [N,O]
--    reducido [N,O,E]                         ==  [N]
--    reducido [N,O,E,S]                       ==  [] 
--    reducido [N,O,S,E]                       ==  [N,O,S,E]
--    reducido [S,S,S,N,N,N]                   ==  []
--    reducido [N,S,S,E,O,N]                   ==  []
--    reducido [N,S,S,E,O,N,O]                 ==  [O]
--
-- Nótese que en el último ejemplo las reducciones son
--        [N,S,S,E,O,N,O]  
--    --> [S,E,O,N,O]  
--    --> [S,N,O]  
--    --> [O]  
-- ---------------------------------------------------------------------

data Direccion = N | S | E | O deriving (Show, Eq)

type Camino = [Direccion]

-- 1ª solución
-- ===========

reducido :: Camino -> Camino
reducido [] = []
reducido (d:ds)
  | null ds'                = [d]
  | d == opuesta (head ds') = tail ds'
  | otherwise               = d:ds'
  where ds' = reducido ds

opuesta :: Direccion -> Direccion
opuesta N = S
opuesta S = N
opuesta E = O
opuesta O = E

-- 2ª solución
-- ===========

reducido2 :: Camino -> Camino
reducido2 = foldr aux []
    where aux N (S:xs) = xs
          aux S (N:xs) = xs
          aux E (O:xs) = xs
          aux O (E:xs) = xs
          aux x xs     = x:xs

-- 3ª solución
-- ===========

reducido3 :: Camino -> Camino
reducido3 []       = []
reducido3 (N:S:ds) = reducido3 ds
reducido3 (S:N:ds) = reducido3 ds
reducido3 (E:O:ds) = reducido3 ds
reducido3 (O:E:ds) = reducido3 ds
reducido3 (d:ds)
  | null ds'                = [d]
  | d == opuesta (head ds') = tail ds'
  | otherwise               = d:ds'
  where ds' = reducido3 ds

-- 4ª solución
-- ===========

reducido4 :: Camino -> Camino
reducido4 ds = reverse (aux ([],ds)) where 
    aux (N:xs, S:ys) = aux (xs,ys)
    aux (S:xs, N:ys) = aux (xs,ys)
    aux (E:xs, O:ys) = aux (xs,ys)
    aux (O:xs, E:ys) = aux (xs,ys)
    aux (  xs, y:ys) = aux (y:xs,ys)
    aux (  xs,   []) = xs

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    ghci> reducido (take (10^6) (cycle [N,E,O,S]))
--    []
--    (3.87 secs, 460160736 bytes)
--    ghci> reducido2 (take (10^6) (cycle [N,E,O,S]))
--    []
--    (1.16 secs, 216582880 bytes)
--    ghci> reducido3 (take (10^6) (cycle [N,E,O,S]))
--    []
--    (0.58 secs, 98561872 bytes)
--    ghci> reducido4 (take (10^6) (cycle [N,E,O,S]))
--    []
--    (0.64 secs, 176154640 bytes)
--    
--    ghci> reducido3 (take (10^7) (cycle [N,E,O,S]))
--    []
--    (5.43 secs, 962694784 bytes)
--    ghci> reducido4 (take (10^7) (cycle [N,E,O,S]))
--    []
--    (9.29 secs, 1722601528 bytes)
-- 
--    ghci> length $ reducido3 (take 2000000 $ cycle [N,O,N,S,E,N,S,O,S,S])
--    400002
--    (4.52 secs, 547004960 bytes)
--    ghci> length $ reducido4 (take 2000000 $ cycle [N,O,N,S,E,N,S,O,S,S])
--    400002
--    
--    ghci> let n=10^6 in reducido (replicate n N ++ replicate n S)
--    []
--    (7.35 secs, 537797096 bytes)
--    ghci> let n=10^6 in reducido2 (replicate n N ++ replicate n S)
--    []
--    (2.30 secs, 244553404 bytes)
--    ghci> let n=10^6 in reducido3 (replicate n N ++ replicate n S)
--    []
--    (8.08 secs, 545043608 bytes)
--    ghci> let n=10^6 in reducido4 (replicate n N ++ replicate n S)
--    []
--    (1.96 secs, 205552240 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. [1.5 puntos] Una serie infinita para el cálculo de pi,
-- publicada por Nilakantha en el siglo XV, es
--               4       4       4       4   
--    pi = 3 + ----- - ----- + ----- - ------ + ··· 
--             2x3x4   4x5x6   6x7x8   8x9x10 
--
-- Definir la función
--    aproximacionPi :: Int -> Double
-- tal que (aproximacionPi n) es la n-ésima aproximación de pi obtenida
-- sumando los n primeros términos de la serie de Nilakantha. Por
-- ejemplo, 
--    aproximacionPi 0        ==  3.0
--    aproximacionPi 1        ==  3.1666666666666665
--    aproximacionPi 2        ==  3.1333333333333333
--    aproximacionPi 3        ==  3.145238095238095
--    aproximacionPi 4        ==  3.1396825396825396
--    aproximacionPi 5        ==  3.1427128427128426
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

aproximacionPi :: Int -> Double
aproximacionPi n = serieNilakantha !! n

serieNilakantha :: [Double]
serieNilakantha = scanl1 (+) terminosNilakantha

terminosNilakantha :: [Double]
terminosNilakantha = zipWith (/) numeradores denominadores
  where numeradores   = 3 : cycle [4,-4]
        denominadores = 1 : [n*(n+1)*(n+2) | n <- [2,4..]]

-- 2ª solución
-- ===========

aproximacionPi2 :: Int -> Double
aproximacionPi2 = aux 3 2 1
  where aux x _ _ 0 = x
        aux x y z m =
          aux (x+4/product[y..y+2]*z) (y+2) (negate z) (m-1)

-- 3ª solución
-- ===========

aproximacionPi3 :: Int -> Double
aproximacionPi3 x =
  3 + sum [(((-1)**(n+1))*4)/(2*n*(2*n+1)*(2*n+2))
          | n <- [1..fromIntegral x]]

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> aproximacionPi (10^6)
--    3.141592653589787
--    (1.35 secs, 729,373,160 bytes)
--    λ> aproximacionPi2 (10^6)
--    3.141592653589787
--    (2.96 secs, 2,161,766,096 bytes)
--    λ> aproximacionPi3 (10^6)
--    3.1415926535897913
--    (2.02 secs, 1,121,372,536 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 3.2 [1 punto]. Definir la función
--    tabla :: FilePath -> [Int] -> IO ()
-- tal que (tabla f ns) escribe en el fichero f las n-ésimas
-- aproximaciones de pi, donde n toma los valores de la lista ns, junto
-- con sus  errores. Por ejemplo, al evaluar la expresión
--    tabla "AproximacionesPi.txt" [0,10..100]
-- hace que el contenido del fichero "AproximacionesPi.txt" sea
--      +------+----------------+----------------+
--      | n    | Aproximación   | Error          |
--      +------+----------------+----------------+
--      |    0 | 3.000000000000 | 0.141592653590 |
--      |   10 | 3.141406718497 | 0.000185935093 |
--      |   20 | 3.141565734659 | 0.000026918931 |
--      |   30 | 3.141584272675 | 0.000008380915 |
--      |   40 | 3.141589028941 | 0.000003624649 |
--      |   50 | 3.141590769850 | 0.000001883740 |
--      |   60 | 3.141591552546 | 0.000001101044 |
--      |   70 | 3.141591955265 | 0.000000698325 |
--      |   80 | 3.141592183260 | 0.000000470330 |
--      |   90 | 3.141592321886 | 0.000000331704 |
--      |  100 | 3.141592410972 | 0.000000242618 |
--      +------+----------------+----------------+
-- ---------------------------------------------------------------------

tabla :: FilePath -> [Int] -> IO ()
tabla f ns = writeFile f (tablaAux ns)

tablaAux :: [Int] -> String
tablaAux ns =
     linea
  ++ cabecera
  ++ linea
  ++ concat [printf "| %4d | %.12f | %.12f |\n" n a e
            | n <- ns
            , let a = aproximacionPi n
            , let e = abs (pi - a)]
  ++ linea

linea :: String
linea = "+------+----------------+----------------+\n"

cabecera :: String
cabecera = "| n    | Aproximación   | Error          |\n"

-- ---------------------------------------------------------------------
-- Ejercicio 4.1 [1 punto]. El número 7 tiene 4 descomposiciones como
-- suma de cuadrados de enteros positivos 
--    7 = 1^2 + 1^2 + 1^2 + 2^2
--    7 = 1^2 + 1^2 + 2^2 + 1^2
--    7 = 1^2 + 2^2 + 1^2 + 1^2
--    7 = 2^2 + 1^2 + 1^2 + 1^2]]
-- 
-- Definir la función
--    nDescomposiciones       :: Int -> Int
-- tal que (nDescomposiciones x) es el número de listas de los cuadrados
-- de cuatro números enteros positivos cuya suma es x. Por ejemplo.  
--      nDescomposiciones 7      ==  4
--      nDescomposiciones 4      ==  1
--      nDescomposiciones 5      ==  0
--      nDescomposiciones 10     ==  6
--      nDescomposiciones 15     ==  12
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

nDescomposiciones :: Int -> Int
nDescomposiciones = length . descomposiciones

-- (descomposiciones x) es la lista de las listas de los cuadrados de
-- cuatro números enteros positivos cuya suma es x. Por  ejemplo. 
--    λ> descomposiciones 4
--    [[1,1,1,1]]
--    λ> descomposiciones 5
--    []
--    λ> descomposiciones 7
--    [[1,1,1,4],[1,1,4,1],[1,4,1,1],[4,1,1,1]]
--    λ> descomposiciones 10
--    [[1,1,4,4],[1,4,1,4],[1,4,4,1],[4,1,1,4],[4,1,4,1],[4,4,1,1]]
--    λ> descomposiciones 15
--    [[1,1,4,9],[1,1,9,4],[1,4,1,9],[1,4,9,1],[1,9,1,4],[1,9,4,1],
--     [4,1,1,9],[4,1,9,1],[4,9,1,1],[9,1,1,4],[9,1,4,1],[9,4,1,1]]
descomposiciones :: Int -> [[Int]]
descomposiciones x = aux x 4
  where 
    aux 0 1 = []
    aux 1 1 = [[1]]
    aux 2 1 = []
    aux 3 1 = []
    aux y 1 | esCuadrado y = [[y]]
            | otherwise    = []
    aux y n = [x^2 : zs | x <- [1..raizEntera y]
                        , zs <- aux (y - x^2) (n-1)]

-- (esCuadrado x) se verifica si x es un número al cuadrado. Por
-- ejemplo,
--    esCuadrado 25  ==  True
--    esCuadrado 26  ==  False
esCuadrado :: Int -> Bool
esCuadrado x = (raizEntera x)^2 == x

-- (raizEntera n) es el mayor entero cuya raíz cuadrada es menor o igual
-- que n. Por ejemplo,
--    raizEntera 15  ==  3
--    raizEntera 16  ==  4
--    raizEntera 17  ==  4
raizEntera :: Int -> Int
raizEntera = floor . sqrt . fromIntegral 

-- 2ª solución
-- =============

nDescomposiciones2 :: Int -> Int
nDescomposiciones2 = length . descomposiciones2

descomposiciones2 :: Int -> [[Int]]
descomposiciones2 x = a ! (x,4)
  where
    a = array ((0,1),(x,4)) [((i,j), f i j) | i <- [0..x], j <- [1..4]]
    f 0 1 = []
    f 1 1 = [[1]]
    f 2 1 = []
    f 3 1 = []
    f i 1 | esCuadrado i = [[i]]
          | otherwise    = []
    f i j = [x^2 : zs | x <- [1..raizEntera i]
                      , zs <- a ! (i - x^2,j-1)]

-- 3ª solución
-- ===========

nDescomposiciones3 :: Int -> Int
nDescomposiciones3 x = aux x 4
  where
    aux 0 1 = 0
    aux 1 1 = 1
    aux 2 1 = 0
    aux 3 1 = 0
    aux y 1 | esCuadrado y = 1
            | otherwise    = 0
    aux y n = sum [aux (y - x^2) (n-1) | x <- [1..raizEntera y]]

-- 4ª solución
-- ===========

nDescomposiciones4 :: Int -> Int
nDescomposiciones4 x = a ! (x,4)
  where
    a = array ((0,1),(x,4)) [((i,j), f i j) | i <- [0..x], j <- [1..4]]
    f 0 1 = 0
    f 1 1 = 1
    f 2 1 = 0
    f 3 1 = 0
    f i 1 | esCuadrado i = 1
          | otherwise    = 0
    f i j = sum [a ! (i- x^2,j-1) | x <- [1..raizEntera i]]

-- Comprobación de equivalencia
-- ============================

-- La propiedad es
prop_nDescomposiciones :: Positive Int -> Bool
prop_nDescomposiciones (Positive x) =
  all (== nDescomposiciones x) [f x | f <- [ nDescomposiciones2
                                           , nDescomposiciones3
                                           , nDescomposiciones4]]

-- La comprobación es
--    λ> quickCheck prop_nDescomposiciones
--    +++ OK, passed 100 tests.

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> nDescomposiciones 20000
--    1068
--    (3.69 secs, 3,307,250,128 bytes)
--    λ> nDescomposiciones2 20000
--    1068
--    (0.72 secs, 678,419,328 bytes)
--    λ> nDescomposiciones3 20000
--    1068
--    (3.94 secs, 3,485,725,552 bytes)
--    λ> nDescomposiciones4 20000
--    1068
--    (0.74 secs, 716,022,456 bytes)
--    
--    λ> nDescomposiciones2 50000
--    5682
--    (2.64 secs, 2,444,206,000 bytes)
--    λ> nDescomposiciones4 50000
--    5682
--    (2.77 secs, 2,582,443,448 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 4.2 [1.5 puntos]. Con la definición del apartado anterior,
-- evaluar (en menos de 2 segundos),
--    nDescomposiciones (2*10^4)
-- ---------------------------------------------------------------------

-- El cálculo es
--    λ> timeout (2*10^6) (return $! (nDescomposiciones4 (2*10^4)))
--    Just 1068
--    (1.13 secs, 715,951,808 bytes)
