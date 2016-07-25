-- Informática (1º del Grado en Matemáticas)
-- Examen de la 1ª convocatoria (23 de junio de 2016)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Array
import Data.List
import Test.QuickCheck
import qualified Data.Set as S
import Data.Numbers.Primes

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    particiones :: [a] -> Int -> [[[a]]]
-- tal que (particiones xs k) es la lista de las particiones de xs en k
-- subconjuntos disjuntos. Por ejemplo,
--    ghci> particiones [2,3,6] 2
--    [[[2],[3,6]],[[2,3],[6]],[[3],[2,6]]]
--    ghci> particiones [2,3,6] 3
--    [[[2],[3],[6]]]
--    ghci> particiones [4,2,3,6] 3
--    [[[4],[2],[3,6]],[[4],[2,3],[6]],[[4],[3],[2,6]],
--     [[4,2],[3],[6]],[[2],[4,3],[6]],[[2],[3],[4,6]]]
--    ghci> particiones [4,2,3,6] 1
--    [[[4,2,3,6]]]
--    ghci> particiones [4,2,3,6] 4
--    [[[4],[2],[3],[6]]]
-- ---------------------------------------------------------------------

particiones :: [a] -> Int -> [[[a]]]
particiones [] _     = []
particiones _  0     = []
particiones xs 1     = [[xs]]
particiones (x:xs) k = [[x]:ys | ys <- particiones xs (k-1)] ++ 
                       concat [inserciones x ys | ys <- particiones xs k]

-- (inserciones x yss) es la lista obtenida insertando x en cada uno de los
-- conjuntos de yss. Por ejemplo,
--    inserciones 4 [[3],[2,5]]  ==  [[[4,3],[2,5]],[[3],[4,2,5]]]
inserciones :: a -> [[a]] -> [[[a]]]
inserciones x []       = []
inserciones x (ys:yss) = ((x:ys):yss) : [ys:zss | zss <- inserciones x yss]

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Las expresiones aritméticas con números enteros, sumas
-- y restas se puede representar con el tipo de datos Expr definido por
--    data Expr = N Int
--              | S Expr Expr
--              | R Expr Expr
--      deriving (Eq, Show)
-- Por ejemplo, la expresión 3+(4-2) se representa por
-- (S (N 3) (R (N 4) (N 2)))
--
-- Definir la función 
--    expresiones :: [Int] -> [Expr]
-- tal que (expresiones ns) es la lista de todas las expresiones
-- que se pueden construir intercalando las operaciones de suma o resta
-- entre los números de ns. Por ejemplo,
--   ghci> expresiones [2,3,5]
--   [S (N 2) (S (N 3) (N 5)),R (N 2) (S (N 3) (N 5)),S (N 2) (R (N 3) (N 5)),
--    R (N 2) (R (N 3) (N 5)),S (S (N 2) (N 3)) (N 5),R (S (N 2) (N 3)) (N 5),
--    S (R (N 2) (N 3)) (N 5),R (R (N 2) (N 3)) (N 5)]
--    > length (expresiones [2,3,5,9])
--    40
-- ---------------------------------------------------------------------

data Expr = N Int
          | S Expr Expr
          | R Expr Expr
  deriving (Eq, Show)

expresiones :: [Int] -> [Expr]
expresiones []  = []
expresiones [n] = [N n]
expresiones ns  = [e | (is,ds) <- divisiones ns
                     , i       <- expresiones is
                     , d       <- expresiones ds
                     , e       <- combina i d]

-- (divisiones xs) es la lista de las divisiones de xs en dos listas no
-- vacías. Por ejemplo,
--    divisiones "bcd"   ==  [("b","cd"),("bc","d")]
--    divisiones "abcd"  ==  [("a","bcd"),("ab","cd"),("abc","d")]
divisiones :: [a] -> [([a],[a])]
divisiones []     = []
divisiones [_]    = []
divisiones (x:xs) = ([x],xs) : [(x:is,ds) | (is,ds) <- divisiones xs]

-- (combina e1 e2) es la lista de las expresiones obtenidas combinando
-- las expresiones e1 y e2 con una operación. Por ejemplo,
--    combina (N 2) (N 3)  ==  [S (N 2) (N 3),R (N 2) (N 3)]
combina :: Expr -> Expr -> [Expr]
combina e1 e2 = [S e1 e2, R e1 e2]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función
--    valores :: [Int] -> [Int]
-- tal que (valores n) es el conjunto de los valores de las expresiones
-- que se pueden construir intercalando las operaciones de suma o resta 
-- entre los números de ns. Por ejemplo,
--    valores [2,3,5]    ==  [-6,0,4,10]
--    valores [2,3,5,9]  ==  [-15,-9,-5,1,3,9,13,19]
-- ---------------------------------------------------------------------

valores :: [Int] -> [Int]
valores ns = sort (nub (map valor (expresiones ns)))

-- (valor e) es el valor de la expresión e. Por ejemplo,
--    valor (S (R (N 2) (N 3)) (N 5))  ==  4
valor :: Expr -> Int
valor (N n)   = n 
valor (S i d) = valor i + valor d
valor (R i d) = valor i - valor d

-- ---------------------------------------------------------------------
-- Ejercicio 3. Dada una matriz cuyos elementos son 0 ó 1, su relleno es
-- la matriz obtenida haciendo iguales a 1 los elementos de las filas y
-- de las columna que contienen algún uno. Por ejemplo, el relleno de la
-- matriz de la izquierda es la de la derecha:
--    0 0 0 0 0    1 0 0 1 0
--    0 0 0 0 0    1 0 0 1 0
--    0 0 0 1 0    1 1 1 1 1
--    1 0 0 0 0    1 1 1 1 1
--    0 0 0 0 0    1 0 0 1 0
--
-- Las matrices se pueden representar mediante tablas cuyos índices son
-- pares de enteros
--    type Matriz = Array (Int,Int) Int
-- por ejemplo, la matriz de la izquierda de la figura anterior se
-- define por
--    ej :: Matriz                  
--    ej = listArray ((1,1),(5,5)) [0, 0, 0, 0, 0,
--                                  0, 0, 0, 0, 0,
--                                  0, 0, 0, 1, 0,
--                                  1, 0, 0, 0, 0,
--                                  0, 0, 0, 0, 0]
-- Definir la función 
--    relleno :: Matriz -> Matriz
-- tal que (relleno p) es el relleno de la matriz p. Por ejemplo,
--    ghci> elems (relleno ej)
--    [1,0,0,1,0,
--     1,0,0,1,0,
--     1,1,1,1,1,
--     1,1,1,1,1,
--     1,0,0,1,0]
-- ---------------------------------------------------------------------

type Matriz = Array (Int,Int) Int

ej :: Matriz                  
ej = listArray ((1,1),(5,5)) [0, 0, 0, 0, 0,
                              0, 0, 0, 0, 0,
                              0, 0, 0, 1, 0,
                              1, 0, 0, 0, 0,
                              0, 0, 0, 0, 0]

-- 1ª solución
-- ===========

relleno1 :: Matriz -> Matriz
relleno1 p = 
    array ((1,1),(m,n)) [((i,j),f i j) | i <- [1..m], j <- [1..n]]
    where (_,(m,n)) = bounds p
          f i j | 1 `elem` [p!(i,k) | k <- [1..n]] = 1
                | 1 `elem` [p!(k,j) | k <- [1..m]] = 1
                | otherwise                        = 0

-- 2ª solución
-- ===========

relleno2 :: Matriz -> Matriz
relleno2 p = 
    array ((1,1),(m,n)) [((i,j),f i j) | i <- [1..m], j <- [1..n]]
    where (_,(m,n)) = bounds p
          filas     = filasConUno p
          columnas  = columnasConUno p
          f i j | i `elem` filas    = 1
                | j `elem` columnas = 1
                | otherwise         = 0

-- (filasConUno p) es la lista de las filas de p que tienen algún
-- uno. Por ejemplo,
--    filasConUno ej  ==  [3,4]
filasConUno :: Matriz -> [Int]
filasConUno p = [i | i <- [1..m], filaConUno p i]
    where (_,(m,n)) = bounds p

-- (filaConUno p i) se verifica si p tiene algún uno en la fila i. Por
-- ejemplo, 
--    filaConUno ej 3  ==  True
--    filaConUno ej 2  ==  False
filaConUno :: Matriz -> Int -> Bool
filaConUno p i = any (==1) [p!(i,j) | j <- [1..n]]
    where (_,(_,n)) = bounds p

-- (columnasConUno p) es la lista de las columnas de p que tienen algún
-- uno. Por ejemplo,
--    columnasConUno ej  ==  [1,4]
columnasConUno :: Matriz -> [Int]
columnasConUno p = [j | j <- [1..n], columnaConUno p j]
    where (_,(m,n)) = bounds p

-- (columnaConUno p i) se verifica si p tiene algún uno en la columna i. Por
-- ejemplo, 
--    columnaConUno ej 1  ==  True
--    columnaConUno ej 2  ==  False
columnaConUno :: Matriz -> Int -> Bool
columnaConUno p j = any (==1) [p!(i,j) | i <- [1..m]]
    where (_,(m,_)) = bounds p

-- 3ª solución
-- ===========

relleno3 :: Matriz -> Matriz
relleno3 p = p // ([((i,j),1) | i <- filas,  j <- [1..n]] ++ 
                   [((i,j),1) | i <- [1..m], j <- columnas])
  where (_,(m,n)) = bounds p
        filas     = filasConUno p
        columnas  = columnasConUno p


-- Comparación de eficiencia
-- =========================

--    ghci> let f i j = if i == j then 1 else 0 
--    ghci> let q n = array ((1,1),(n,n)) [((i,j),f i j) | i <- [1..n], j <- [1..n]]
--    
--    ghci> sum (elems (relleno1 (q 200)))
--    40000
--    (6.90 secs, 1,877,369,544 bytes)
--    
--    ghci> sum (elems (relleno2 (q 200)))
--    40000
--    (0.46 secs, 57,354,168 bytes)
--    
--    ghci> sum (elems (relleno3 (q 200)))
--    40000
--    (0.34 secs, 80,465,144 bytes)
--    
--    ghci> sum (elems (relleno2 (q 500)))
--    250000
--    (4.33 secs, 353,117,640 bytes)
--    
--    ghci> sum (elems (relleno3 (q 500)))
--    250000
--    (2.40 secs, 489,630,048 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Un número natural n es una potencia perfecta si existen
-- dos números naturales m > 1 y k > 1 tales que n = m^k. Las primeras
-- potencias perfectas son
--    4 = 2², 8 = 2³, 9 = 3², 16 = 2⁴, 25 = 5², 27 = 3³, 32 = 2⁵, 
--    36 = 6², 49 = 7², 64 = 2⁶, ...
-- 
-- Definir la sucesión
--    potenciasPerfectas :: [Integer]
-- cuyos términos son las potencias perfectas. Por ejemplo,
--    take 10 potenciasPerfectas  ==  [4,8,9,16,25,27,32,36,49,64]
--    potenciasPerfectas !! 100   ==  6724
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

potenciasPerfectas1 :: [Integer]
potenciasPerfectas1 = filter esPotenciaPerfecta [4..]

-- (esPotenciaPerfecta x) se verifica si x es una potencia perfecta. Por
-- ejemplo, 
--    esPotenciaPerfecta 36  ==  True
--    esPotenciaPerfecta 72  ==  False
esPotenciaPerfecta :: Integer -> Bool
esPotenciaPerfecta = not . null. potenciasPerfectasDe 

-- (potenciasPerfectasDe x) es la lista de pares (a,b) tales que 
-- x = a^b. Por ejemplo,
--    potenciasPerfectasDe 64  ==  [(2,6),(4,3),(8,2)]
--    potenciasPerfectasDe 72  ==  []
potenciasPerfectasDe :: Integer -> [(Integer,Integer)]
potenciasPerfectasDe n = 
    [(m,k) | m <- takeWhile (\x -> x*x <= n) [2..]
           , k <- takeWhile (\x -> m^x <= n) [2..]
           , m^k == n]

-- 2ª solución
-- ===========

potenciasPerfectas2 :: [Integer]
potenciasPerfectas2 = [x | x <- [4..], esPotenciaPerfecta2 x]

-- (esPotenciaPerfecta2 x) se verifica si x es una potencia perfecta. Por
-- ejemplo, 
--    esPotenciaPerfecta2 36  ==  True
--    esPotenciaPerfecta2 72  ==  False
esPotenciaPerfecta2 :: Integer -> Bool
esPotenciaPerfecta2 x = mcd (exponentes x) > 1

-- (exponentes x) es la lista de los exponentes de l factorización prima
-- de x. Por ejemplos,
--    exponentes 36  ==  [2,2]
--    exponentes 72  ==  [3,2]
exponentes :: Integer -> [Int]
exponentes x = [length ys | ys <- group (primeFactors x)] 

-- (mcd xs) es el máximo común divisor de la lista xs. Por ejemplo,
--    mcd [4,6,10]  ==  2
--    mcd [4,5,10]  ==  1
mcd :: [Int] -> Int
mcd = foldl1 gcd

-- 3ª definición
-- =============

potenciasPerfectas3 :: [Integer]
potenciasPerfectas3 = mezclaTodas potencias

-- potencias es la lista las listas de potencias de todos los números
-- mayores que 1 con exponentes mayores que 1. Por ejemplo,
--    ghci> map (take 3) (take 4 potencias)
--    [[4,8,16],[9,27,81],[16,64,256],[25,125,625]]
potencias:: [[Integer]]
potencias = [[n^k | k <- [2..]] | n <- [2..]]

-- (mezclaTodas xss) es la mezcla ordenada sin repeticiones de las
-- listas ordenadas xss. Por ejemplo,
--    take 7 (mezclaTodas potencias)  ==  [4,8,9,16,25,27,32]
mezclaTodas :: Ord a => [[a]] -> [a]
mezclaTodas = foldr1 xmezcla
    where xmezcla (x:xs) ys = x : mezcla xs ys

-- (mezcla xs ys) es la mezcla ordenada sin repeticiones de las
-- listas ordenadas xs e ys. Por ejemplo,
--    take 7 (mezcla [2,5..] [4,6..])  ==  [2,4,5,6,8,10,11]
mezcla :: Ord a => [a] -> [a] -> [a]
mezcla (x:xs) (y:ys) | x < y  = x : mezcla xs (y:ys)
                     | x == y = x : mezcla xs ys
                     | x > y  = y : mezcla (x:xs) ys

-- Comparación de eficiencia
-- =========================

--    ghci> potenciasPerfectas1 !! 100
--    6724
--    (3.39 secs, 692758212 bytes)
--    ghci> potenciasPerfectas2 !! 100
--    6724
--    (0.29 secs, 105,459,200 bytes)
--    ghci> potenciasPerfectas3 !! 100
--    6724
--    (0.01 secs, 1582436 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    minimales :: Ord a => S.Set (S.Set a) -> S.Set (S.Set a)
-- tal que (minimales xss) es el conjunto de los elementos de xss que no
-- están contenidos en otros elementos de xss. Por ejemplo,
--    ghci> minimales (S.fromList (map S.fromList [[1,3],[2,3,1],[3,2,5]]))
--    fromList [fromList [1,3],fromList [2,3,5]]
--    ghci> minimales (S.fromList (map S.fromList [[1,3],[2,3,1],[3,1],[3,2,5]]))
--    fromList [fromList [1,3],fromList [2,3,5]]
--    ghci> minimales (S.fromList (map S.fromList [[1,3],[2,3,1],[3,1,3],[3,2,5]]))
--    fromList [fromList [1,3],fromList [2,3,5]]
-- ---------------------------------------------------------------------

minimales :: Ord a => S.Set (S.Set a) -> S.Set (S.Set a)
minimales xss = S.filter esMinimal xss
    where esMinimal xs = S.null (S.filter (`S.isProperSubsetOf` xs) xss)

