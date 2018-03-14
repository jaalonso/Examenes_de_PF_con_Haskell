-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 4º examen de evaluación continua (14 de marzo de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías                                                        --
-- ---------------------------------------------------------------------

import Data.List
import Data.Matrix
import Test.QuickCheck
import Graphics.Gnuplot.Simple
import Data.Function

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función
--    menorPotencia :: Integer -> (Integer,Integer)
-- tal que (menorPotencia n) es el par (k,m) donde m es la menor
-- potencia de 2 que empieza por n y k es su exponentes (es decir,
-- 2^k = m). Por ejemplo, 
--    menorPotencia 3             ==  (5,32)
--    menorPotencia 7             ==  (46,70368744177664)
--    fst (menorPotencia 982)     ==  3973
--    fst (menorPotencia 32627)   ==  28557
--    fst (menorPotencia 158426)  ==  40000
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============

menorPotencia :: Integer -> (Integer,Integer)
menorPotencia n =
  head [(k,m) | (k,m) <- zip [0..] potenciasDe2
              , cs `isPrefixOf` show m]
  where cs = show n

-- potenciasDe 2 es la lista de las potencias de dos. Por ejemplo,
--    take 12 potenciasDe2  ==  [1,2,4,8,16,32,64,128,256,512,1024,2048]
potenciasDe2 :: [Integer]
potenciasDe2 = iterate (*2) 1

-- 2ª definición 
-- =============

menorPotencia2 :: Integer -> (Integer,Integer)
menorPotencia2 n = aux (0,1)
  where aux (k,m) | cs `isPrefixOf` show m = (k,m)
                  | otherwise              = aux (k+1,2*m)
        cs = show n

-- 3ª definición 
-- =============

menorPotencia3 :: Integer -> (Integer,Integer)
menorPotencia3 n =
  until (isPrefixOf n1 . show . snd) (\(x,y) -> (x+1,2*y)) (0,1)
  where n1 = show n

-- Comparación de eficiencia
-- =========================

--    λ> maximum [fst (menorPotencia n) | n <- [1..1000]]
--    3973
--    (3.69 secs, 1,094,923,696 bytes)
--    λ> maximum [fst (menorPotencia2 n) | n <- [1..1000]]
--    3973
--    (5.13 secs, 1,326,382,872 bytes)
--    λ> maximum [fst (menorPotencia3 n) | n <- [1..1000]]
--    3973
--    (4.71 secs, 1,240,498,128 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función
--    graficaMenoresExponentes :: Integer -> IO () 
-- tal que (graficaMenoresExponentes n) dibuja la gráfica de los
-- exponentes de 2 en las menores potencias de los n primeros números
-- enteros positivos. 
-- ---------------------------------------------------------------------

graficaMenoresExponentes :: Integer -> IO ()
graficaMenoresExponentes n =
  plotList [ Key Nothing
           , PNG "Menor_potencia_de_2_que_comienza_por_n.png"
           ]
           (map (fst . menorPotencia) [1..n])

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función 
--    raizEnt :: Integer -> Integer -> Integer
-- tal que (raizEnt x n) es la raíz entera n-ésima de x; es decir, el
-- mayor número entero y tal que y^n <= x. Por ejemplo,
--    raizEnt  8 3      ==  2
--    raizEnt  9 3      ==  2
--    raizEnt 26 3      ==  2
--    raizEnt 27 3      ==  3
--    raizEnt (10^50) 2 ==  10000000000000000000000000
--
-- Comprobar con QuickCheck que para todo número natural n, 
--     raizEnt (10^(2*n)) 2 == 10^n
-- ---------------------------------------------------------------------

-- 1ª definición
raizEnt1 :: Integer -> Integer -> Integer
raizEnt1 x n =
  last (takeWhile (\y -> y^n <= x) [0..])

-- 2ª definición         
raizEnt2 :: Integer -> Integer -> Integer
raizEnt2 x n =
  floor ((fromIntegral x)**(1 / fromIntegral n))

-- Nota. La definición anterior falla para números grandes. Por ejemplo,
--    λ> raizEnt2 (10^50) 2 == 10^25
--    False
          
-- 3ª definición          
raizEnt3 :: Integer -> Integer -> Integer
raizEnt3 x n = aux (1,x)
  where aux (a,b) | d == x    = c
                  | c == a    = c
                  | d < x     = aux (c,b)
                  | otherwise = aux (a,c) 
          where c = (a+b) `div` 2
                d = c^n

-- Comparación de eficiencia
--    λ> raizEnt1 (10^14) 2
--    10000000
--    (6.15 secs, 6,539,367,976 bytes)
--    λ> raizEnt2 (10^14) 2
--    10000000
--    (0.00 secs, 0 bytes)
--    λ> raizEnt3 (10^14) 2
--    10000000
--    (0.00 secs, 25,871,944 bytes)
--    
--    λ> raizEnt2 (10^50) 2
--    9999999999999998758486016
--    (0.00 secs, 0 bytes)
--    λ> raizEnt3 (10^50) 2
--    10000000000000000000000000
--    (0.00 secs, 0 bytes)
                        
-- La propiedad es                        
prop_raizEnt :: (Positive Integer) -> Bool
prop_raizEnt (Positive n) =
  raizEnt3 (10^(2*n)) 2 == 10^n

-- La comprobación es              
--    λ> quickCheck prop_raizEnt
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3. Los árboles se pueden representar mediante el siguiente
-- tipo de datos  
--    data Arbol a = N a [Arbol a]
--                   deriving Show
-- Por ejemplo, los árboles
--      1               3      
--     / \             /|\     
--    2   3           / | \    
--        |          5  4  7   
--        4          |    /|\  
--                   6   2 8 6 
-- se representan por
--    ejArbol1, ejArbol2 :: Arbol Int
--    ejArbol1 = N 1 [N 2 [], N 3 [N 4 []]]
--    ejArbol2 = N 3 [N 5 [N 6 []], 
--                    N 4 [], 
--                    N 7 [N 2 [], N 8 [], N 6 []]]
--
-- Definir la función
--    nodosSumaMaxima :: (Num t, Ord t) => Arbol t -> [t]
-- tal que (nodosSumaMaxima a) es la lista de los nodos del
-- árbol a cuyos hijos tienen máxima suma. Por ejemplo,
--    nodosSumaMaxima ejArbol1  ==  [1]
--    nodosSumaMaxima ejArbol2  ==  [7,3]
-- ---------------------------------------------------------------------

data Arbol a = N a [Arbol a]
  deriving Show

ejArbol1, ejArbol2 :: Arbol Int
ejArbol1 = N 1 [N 2 [], N 3 [N 4 []]]
ejArbol2 = N 3 [N 5 [N 6 []], 
                N 4 [], 
                N 7 [N 2 [], N 8 [], N 6 []]]

-- 1ª solución
-- ===========

nodosSumaMaxima :: (Num t, Ord t) => Arbol t -> [t]
nodosSumaMaxima a =
  [x | (s,x) <- ns, s == m]
  where ns = reverse (sort (nodosSumas a))
        m  = fst (head ns)

-- (nodosSumas x) es la lista de los pares (s,n) donde n es un nodo del
-- árbol x y s es la suma de sus hijos. Por ejemplo,  
--    λ> nodosSumas ejArbol1
--    [(5,1),(0,2),(4,3),(0,4)]
--    λ> nodosSumas ejArbol2
--    [(16,3),(6,5),(0,6),(0,4),(16,7),(0,2),(0,8),(0,6)]
nodosSumas :: Num t => Arbol t -> [(t,t)]
nodosSumas (N x []) = [(0,x)]
nodosSumas (N x as) = (sum (raices as),x) : concatMap nodosSumas as

-- (raices b) es la lista de las raíces del bosque b. Por ejemplo,
--    raices [ejArbol1,ejArbol2]  ==  [1,3]
raices :: [Arbol t] -> [t]
raices = map raiz

-- (raiz a) es la raíz del árbol a. Por ejemplo,
--    raiz ejArbol1  ==  1
--    raiz ejArbol2  ==  3
raiz :: Arbol t -> t
raiz (N x _) = x

-- 2ª solución
-- ===========

nodosSumaMaxima2 :: (Num t, Ord t) => Arbol t -> [t]
nodosSumaMaxima2 a =
  [x | (s,x) <- ns, s == m]
  where ns = sort (nodosOpSumas a)
        m  = fst (head ns)

-- (nodosOpSumas x) es la lista de los pares (s,n) donde n es un nodo del
-- árbol x y s es el opuesto de la suma de sus hijos. Por ejemplo,  
--    λ> nodosOpSumas ejArbol1
--    [(-5,1),(0,2),(-4,3),(0,4)]
--    λ> nodosOpSumas ejArbol2
--    [(-16,3),(-6,5),(0,6),(0,4),(-16,7),(0,2),(0,8),(0,6)]
nodosOpSumas :: Num t => Arbol t -> [(t,t)]
nodosOpSumas (N x []) = [(0,x)]
nodosOpSumas (N x as) = (-sum (raices as),x) : concatMap nodosOpSumas as


-- 3ª solución
-- ===========

nodosSumaMaxima3 :: (Num t, Ord t) => Arbol t -> [t]
nodosSumaMaxima3 a =
  [x | (s,x) <- ns, s == m]
  where ns = sort (nodosOpSumas a)
        m  = fst (head ns)

-- 4ª solución
-- ===========

nodosSumaMaxima4 :: (Num t, Ord t) => Arbol t -> [t]
nodosSumaMaxima4 a =
  map snd (head (groupBy (\p q -> fst p == fst q)
                         (sort (nodosOpSumas a))))

-- 5ª solución
-- ===========

nodosSumaMaxima5 :: (Num t, Ord t) => Arbol t -> [t]
nodosSumaMaxima5 a =
  map snd (head (groupBy ((==) `on` fst)
                         (sort (nodosOpSumas a))))

-- 6ª solución
-- ===========

nodosSumaMaxima6 :: (Num t, Ord t) => Arbol t -> [t]
nodosSumaMaxima6 =
  map snd
  . head
  . groupBy ((==) `on` fst)
  . sort
  . nodosOpSumas

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    ampliaMatriz :: Matrix a -> Int -> Int -> Matrix a
-- tal que (ampliaMatriz p f c) es la matriz obtenida a partir de p
-- repitiendo cada fila f veces y cada columna c veces. Por ejemplo, si
-- ejMatriz es la matriz definida por
--    ejMatriz :: Matrix Char
--    ejMatriz = fromLists [" x ",
--                          "x x",
--                          " x "]
-- entonces 
--    λ> ampliaMatriz ejMatriz 1 2
--    ( ' ' ' ' 'x' 'x' ' ' ' ' )
--    ( 'x' 'x' ' ' ' ' 'x' 'x' )
--    ( ' ' ' ' 'x' 'x' ' ' ' ' )
--    
--    λ> (putStr . unlines . toLists) (ampliaMatriz ejMatriz 1 2)
--      xx  
--    xx  xx
--      xx  
--    λ> (putStr . unlines . toLists) (ampliaMatriz ejMatriz 2 1)
--     x 
--     x 
--    x x
--    x x
--     x 
--     x 
--    λ> (putStr . unlines . toLists) (ampliaMatriz ejMatriz 2 2)
--      xx  
--      xx  
--    xx  xx
--    xx  xx
--      xx  
--      xx  
--    λ> (putStr . unlines . toLists) (ampliaMatriz ejMatriz 2 3)
--       xxx   
--       xxx   
--    xxx   xxx
--    xxx   xxx
--       xxx   
--       xxx   
-- ---------------------------------------------------------------------

ejMatriz :: Matrix Char
ejMatriz = fromLists [" x ",
                      "x x",
                      " x "]

-- 1ª definición
-- =============

ampliaMatriz :: Matrix a -> Int -> Int -> Matrix a
ampliaMatriz p f c =
  ampliaColumnas (ampliaFilas p f) c

ampliaFilas :: Matrix a -> Int -> Matrix a
ampliaFilas p f =
  matrix (f*m) n (\(i,j) -> p!(1 + (i-1) `div` f, j))
  where m = nrows p
        n = ncols p

ampliaColumnas :: Matrix a -> Int -> Matrix a
ampliaColumnas p c =
  matrix m (c*n) (\(i,j) -> p!(i,1 + (j-1) `div` c))
  where m = nrows p
        n = ncols p

-- 2ª definición
-- =============

ampliaMatriz2 :: Matrix a -> Int -> Int -> Matrix a
ampliaMatriz2 p f c =
  ( fromLists
  . concatMap (map (concatMap (replicate c)) . replicate f)
  . toLists) p

-- Comparación de eficiencia
-- =========================

ejemplo :: Int -> Matrix Int
ejemplo n = fromList n n [1..]

--    λ> maximum (ampliaMatriz (ejemplo 10) 100 200)
--    100
--    (6.44 secs, 1,012,985,584 bytes)
--    λ> maximum (ampliaMatriz2 (ejemplo 10) 100 200)
--    100
--    (2.38 secs, 618,096,904 bytes)

