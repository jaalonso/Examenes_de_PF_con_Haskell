-- Informática (1º del Grado en Matemáticas)
-- 5º examen de evaluación continua (21 de abril de 2017)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Librerías auxiliares
-- ---------------------------------------------------------------------

import Data.List
import I1M.PolOperaciones
import I1M.Pila
import Graphics.Gnuplot.Simple
import qualified Data.Matrix as M
import qualified Data.Set    as S
import qualified Data.Vector as V

-- ---------------------------------------------------------------------
-- Ejercicio 1. Los números 181 y 1690961 son ambos palíndromos y suma
-- de cuadrados consecutivos, pues 181 = 9^2 + 10^2 y
-- 1690961 = 919^2 + 920^2.
-- 
-- Definir la constante
--    sucesion :: [Integer]
-- tal que sucesion es la lista de los números que son suma de cuadrados
-- consecutivos y palíndromos. Por ejemplo,
--    take 10 sucesion
--    [1,5,181,313,545,1690961,3162613,3187813,5258525,5824285]
-- ---------------------------------------------------------------------

sucesion :: [Integer]
sucesion = filter palindromo sucSumaCuadradosConsecutivos

palindromo :: Integer -> Bool
palindromo n = xs == reverse xs
  where xs = show n

-- sucSumaCuadradosConsecutivos es la sucesión de los números que son
-- sumas de los cuadrados de dos números consecutivos. Por ejemplo,       
--    ghci> take 10 sucSumaCuadradosConsecutivos
--    [1,5,13,25,41,61,85,113,145,181]
sucSumaCuadradosConsecutivos :: [Integer]
sucSumaCuadradosConsecutivos = map (\x -> 2*x^2+2*x+1) [0..]

-- 2ª definición
sucSumaCuadradosConsecutivos2 :: [Integer]
sucSumaCuadradosConsecutivos2 =
  [x^2 + (x+1)^2 | x <- [0..]]

-- 3ª definición
sucSumaCuadradosConsecutivos3 :: [Integer]
sucSumaCuadradosConsecutivos3 =
  zipWith (+) cuadrados (tail cuadrados)

cuadrados :: [Integer]
cuadrados = map (^2) [0..]

-- Comparación de eficiencia
--    ghci> maximum (take (10^6) sucSumaCuadradosConsecutivos)
--    1999998000001
--    (1.47 secs, 912,694,568 bytes)
--    ghci> maximum (take (10^6) sucSumaCuadradosConsecutivos2)
--    1999998000001
--    (1.79 secs, 1,507,639,560 bytes)
--    ghci> maximum (take (10^6) sucSumaCuadradosConsecutivos3)
--    1999998000001
--    (1.29 secs, 840,488,376 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Se define la relación de orden entre las pilas como el
-- orden lexicográfico. Es decir, la pila p1 es "menor" que p2 si la
-- cima de p1 es menor que la cima de p2, o si son iguales, la pila que
-- resulta de desapilar p1 es "menor" que la pila que resulta de
-- desapilar p2. 
-- 
-- Definir la función
--    menorPila :: Ord a => Pila a -> Pila a -> Bool
-- tal que (menorPila p1 p2) se verifica si p1 es "menor" que p2. Por
-- ejemplo, para la pilas  
--    p1 = foldr apila vacia [1..20]
--    p2 = foldr apila vacia [1..5]
--    p3 = foldr apila vacia [3..10]
--    p4 = foldr apila vacia [4,-1,7,3,8,10,0,3,3,4]
-- se verifica que
--    menorPila p1 p2    == True
--    menorPila p2 p1    == False
--    menorPila p3 p4    == True
--    menorPila vacia p1 == True
--    menorPila p1 vacia == False
-- ---------------------------------------------------------------------

menorPila :: Ord a => Pila a -> Pila a -> Bool
menorPila p1 p2 | esVacia p1 = True
                | esVacia p2 = False
                | a1 < a2    = True
                | a1 > a2    = False
                | otherwise  = menorPila r1 r2
  where a1 = cima p1
        a2 = cima p2
        r1 = desapila p1
        r2 = desapila p2

p1, p2, p3, p4 :: Pila Int 
p1 = foldr apila vacia [1..20]
p2 = foldr apila vacia [1..5]
p3 = foldr apila vacia [3..10]
p4 = foldr apila vacia [4,-1,7,3,8,10,0,3,3,4]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función
--    polM:: M.Matrix Int -> Polinomio Int
-- tal que (polM m) es el polinomio cuyas raices son los elementos de la
-- diagonal de la matriz m. Por ejemplo, para la matriz definida por 
--    m1 :: M.Matrix Int
--    m1 = M.fromLists [[1, 2, 3,4],
--                      [0,-1, 0,5],
--                      [0, 0,-2,9],
--                      [0, 0, 0,3]]
-- se tiene
--    ghci> polM m1
--    x^4 + -1*x^3 + -7*x^2 + 1*x + 6
-- ---------------------------------------------------------------------

m1 :: M.Matrix Int
m1 = M.fromLists [[1, 2, 3,4],
                  [0,-1, 0,5],
                  [0, 0,-2,9],
                  [0, 0, 0,3]]

polM :: M.Matrix Int -> Polinomio Int
polM m = foldr multPol polUnidad ps
  where ds = V.toList (M.getDiag m)
        ps = [consPol 1 1 (consPol 0 (-d) polCero) | d <- ds]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    dibPol :: M.Matrix Int -> [Int] -> IO ()
-- tal que (dibPol m xs) dibuje la gráfica del polinomio (polM m)
-- tomando los valores en la lista xs. Evaluar (dibPol m1 [-10..10]).
-- ---------------------------------------------------------------------

dibPol :: M.Matrix Int -> [Int] -> IO ()
dibPol m xs =
  plotList [Key Nothing]
           (zip xs (map (valor (polM m)) xs))

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. La sucesión de Recamán está definida como sigue:
--    a(0) = 0
--    a(n) = a(n-1) - n, si a(n-1) > n y no figura ya en la sucesión
--    a(n) = a(n-1) + n, en caso contrario.
-- 
-- Definir la constante
--    sucRecaman :: [Int]
-- tal que sucRecaman es la sucesión anterior. Por ejemplo,
--    ghci> take 21 sucRecaman
--    [0,1,3,6,2,7,13,20,12,21,11,22,10,23,9,24,8,25,43,62,42]
--    ghci> sucRecaman !! (10^3)
--    3686
--    ghci> sucRecaman !! (10^4)
--    18658
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

sucRecaman1:: [Int]
sucRecaman1 = map suc1 [0..]

suc1 :: Int -> Int
suc1 0 = 0
suc1 n | y > n && y - n `notElem` ys = y - n
       | otherwise                   = y + n
  where y  = suc1 (n - 1)
        ys = [suc1 k | k <- [0..n - 1]]

-- 2ª solución (ev. perezosa)
-- ==========================  

sucRecaman2 :: [Int]
sucRecaman2 = 0:zipWith3 f sucRecaman [1..] (repeat sucRecaman)
  where f y n ys | y > n && y - n `notElem` take n ys = y - n
                 | otherwise                          = y + n

-- 3ª solución (ev. perezosa y conjuntos)
-- ======================================

sucRecaman3 :: [Int]
sucRecaman3 = 0 : recaman (S.singleton 0) 1 0

recaman :: S.Set Int -> Int -> Int -> [Int]
recaman s n x
  | x > n && (x-n) `S.notMember` s =
    (x-n) : recaman (S.insert (x-n) s) (n+1) (x-n)
  | otherwise =
    (x+n):recaman (S.insert (x+n) s) (n+1) (x+n) 

-- Comparación de eficiencia:
--    ghci> sucRecaman1 !! 25
--    17
--    (3.76 secs, 2,394,593,952 bytes)
--    ghci> sucRecaman2 !! 25
--    17
--    (0.00 secs, 0 bytes)
--    ghci> sucRecaman3 !! 25
--    17
--    (0.00 secs, 0 bytes)
--
--    ghci> sucRecaman2 !! (2*10^4)
--    14358
--    (2.69 secs, 6,927,559,784 bytes)
--    ghci> sucRecaman3 !! (2*10^4)
--    14358
--    (0.04 secs, 0 bytes)

-- En lo que sigue se usará la 3ª definición.
sucRecaman :: [Int]
sucRecaman = sucRecaman3

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Se ha conjeturado que cualquier entero positivo
-- aparece en la sucesión de Recaman.
-- 
-- Definir la función
--    conjetura_Recaman :: Int -> Bool
-- tal que (conjetura_Recaman n) comprueba la conjetura para x <= n. Por
-- ejemplo, 
--    conjeturaRecaman 30  == True
--    conjeturaRecaman 100 == True
-- ---------------------------------------------------------------------

-- 1ª definición
conjeturaRecaman :: Int -> Bool
conjeturaRecaman n =
  and [not $ null $ filter (==x) sucRecaman | x <- [1..n]]

-- 3ª definición
conjeturaRecaman2 :: Int -> Bool
conjeturaRecaman2 n =
  all (`elem` sucRecaman3) [1..n]

-- Comparación de eficiencia
--    ghci> conjeturaRecaman 100 == True
--    True
--    (0.44 secs, 249,218,152 bytes)
--    ghci> conjeturaRecaman2 100 == True
--    True
--    (0.02 secs, 0 bytes)

-- ---------------------------------------------------------------------  
-- Ejercicio 4.3. Definir la función 
--    invRecaman :: Int -> Int
-- tal que (invRecaman n) es la posición de n en la sucesión de
-- Recaman. Por ejemplo, 
--    invRecaman 10  == 12
--    invRecaman 100 == 387
-- ---------------------------------------------------------------------

-- 1ª definición
invRecaman :: Int -> Int
invRecaman n = head [m | m <- [0..], sucRecaman !! m == n]

-- 2ª definición
invRecaman2 :: Int -> Int
invRecaman2 n =
  length (takeWhile (/=n) sucRecaman)

-- Comparación de eficiencia
--    ghci> invRecaman 10400
--    50719
--    (3.13 secs, 42,679,336 bytes)
--    ghci> invRecaman2 10400
--    50719
--    (0.00 secs, 0 bytes)
