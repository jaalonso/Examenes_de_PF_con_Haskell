-- I1M-G5: 5º examen de evaluación continua (5 de mayo de 2016)

-- ---------------------------------------------------------------------
-- Apellidos:                                      Nombre:
-- ---------------------------------------------------------------------

import Data.Numbers.Primes
import I1M.Pila
import I1M.Pol
import Data.Matrix

-- ---------------------------------------------------------------------
-- Ejercicio 1. (1.5 puntos) Un entero positivo se dirá muy primo si 
-- tanto él como todos sus segmentos iniciales son números primos. 
-- Por ejemplo, 7193 es muy primo pues 7,71,719 y 7193 son todos primos. 
-- Define la lista 
--      muyPrimos :: [Integer]
-- de todos los números muy primos. Por ejemplo, 
--   take 20 muyPrimos ==>
--      [2,3,5,7,23,29,31,37,53,59,71,73,79,233,239,293,311,313,317,373]     

muyPrimos :: [Integer]
muyPrimos = filter muyPrimo [1..] 
    where muyPrimo = all isPrime . takeWhile (/=0) . iterate (`div`10)
-- -----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 2. (1.5 puntos) Un bosque es una lista de árboles binarios.
-- Representaremos los bosques mediante el siguiente tipo de dato:

data Arbol a = H a | N a (Arbol a) (Arbol a) deriving Show
data Bosque a = B [Arbol a] deriving Show

-- Define la función
--   cuentaBosque :: Eq a => a - > Bosque a -> Int
-- tal que (cuentaBosque x b) devuelve cuántas veces aparece el elemento
-- x en el bosque b. Por ejemplo:

t1,t2,t3 :: Arbol Int
t1 = N 7 (N 9 (H 1) (N 5 (H 7) (H 6))) (H 5)
t2 = N 8 (H 7) (H 7)
t3 = N 0 (H 4) (N 6 (H 8) (H 9))

b1,b2 :: Bosque Int
b1 = B [t1,t2,t3]
b2 = B [t2,t2,t3,t3]

--  cuentaBosque 7 b1 ==> 4
--  cuentaBosque 4 b2 ==> 2

cuentaBosque :: Eq a => a -> Bosque a -> Int
cuentaBosque x (B xs) = sum $ map (cuentaArbol x) xs 

cuentaArbol :: Eq a => a -> Arbol a -> Int
cuentaArbol x (H z) 
               | x == z = 1  
               | otherwise = 0
cuentaArbol x (N z i d) 
               | x == z = 1 + cuentaArbol x i + cuentaArbol x d
               | otherwise = cuentaArbol x i + cuentaArbol x d
-- ---------------------------------------------------------------------
-- Ejercicio 3. (1.5 puntos) Representamos las pilas mediante el TAD de 
-- las pilas (I1M.Pila). Define la función
--    alternaPila :: (a -> b) -> (a -> b) -> Pila a -> Pila b
-- tal que (alternaPila f g p) devuelve la pila obtenida al aplicar las 
-- funciones f y g, alternativamente y empezando por f, a los elementos
-- de la pila p. Por ejemplo:
--   alternaPila (*2) (+1) (foldr apila vacia [1,2,3,4,5,6]) ==>
--   2|3|6|5|10|7|-

alternaPila :: (a -> b) -> (a -> b) -> Pila a -> Pila b
alternaPila f g p 
  |esVacia p = vacia
  |otherwise = apila (f (cima p)) (alternaPila g f (desapila p))
        
-- ------------------------------------------------------------------
-- Ejercicio 4. (2.5 puntos) Representaremos las matrices mediante la 
-- librería de Haskell Data.Matrix.  

-- a) Define la función
--     acumula :: Num a => Matrix a -> Matrix a
-- tal que (acumula p) devuelve la matriz obtenida al sustituir cada 
-- elemento x de la matriz p por la suma del resto de elementos que 
-- aparecen en la fila y en la columna de x. Por ejemplo:

--          ( 1  2  2  2 )        (  8  4 10 12 )
--  acumula ( 1  0  1  0 )  ==>   (  3  3  7 11 )
--          ( 1 -1  4  7 )        ( 12 14 10  6 )

acumula :: Num a => Matrix a -> Matrix a
acumula p = matrix n m f where
    n = nrows p
    m = ncols p
    f (i,j) = sum [p!(k,j) | k <- [1..n], k /= i] + 
              sum [p!(i,k) | k <- [1..m], k /= j]

-- b) Define la función
--     filasEnComun :: Eq a => Matrix a -> Matrix a -> Matrix a
-- tal que (filasEnComun p q) devuelve la matriz formada por las filas
-- comunes a p y q, ordenadas según aparecen en la matriz p (suponemos 
-- que ambas matrices tienen el mismo número de columnas y que tienen 
-- alguna fila en común). Por ejemplo

--               ( 1 2 1  2 )    ( 9 3 7 -1 )      ( 1 2 1  2 )
-- filasEnComun  ( 0 1 1  0 )    ( 5 7 6  0 ) ==>  ( 9 3 7 -1 )
--               ( 9 3 7 -1 )    ( 1 2 1  2 ) 
--               ( 5 7 0  0 )

filasEnComun :: Eq a => Matrix a -> Matrix a ->Matrix a
filasEnComun p q = fromLists $ filter (\xs -> elem xs (toLists q)) (toLists p)
-- ------------------------------------------------------------------
-- Ejercicio 5. (3 puntos) Representamos los polinomios mediante el 
-- TAD de los Polinomios (I1M.Pol).

-- a) Un polinomio se dirá completo si los exponentes de su variable van 
-- sucesivamente desde su grado hasta cero, sin faltar ninguno. 
-- Por ejemplo, 5*x^3+x^2-7*x+1 es completo. Define el predicado
--             completo :: (Num a,Eq a) => Polinomio a -> Bool
-- tal que (completo p) se verifica si p es completo. 

completo :: (Num a,Eq a) => Polinomio a -> Bool
completo p 
  |esPolCero p = True
  |grado p == 0 = True
  |otherwise = grado (restoPol p) == grado p - 1 && completo (restoPol p)

-- b) Define la función 
--  interPol ::(Num a,Eq a) => Polinomio a -> Polinomio a -> Polinomio a
-- tal que (interPol p q) devuelve el polinomio formado por términos
-- comunes a los polinomios p y q. Por ejemplo:

pol1,pol2 :: Polinomio Int
pol1 = consPol 6 2 (consPol 4 (-1) (consPol 2 (-8) 
         (consPol 0 10 polCero)))                    -- 2*x^6-x^4-8*x^2+10
pol2 = consPol 6 2 (consPol 4 1 (consPol 0 10 polCero)) -- 2*x^6+x^4+10

--  interPol pol1 pol2 ==> 2*x^6+10

interPol ::(Num a,Eq a)=> Polinomio a -> Polinomio a -> Polinomio a
interPol p q
  |esPolCero p || esPolCero q  = polCero
  |n > m = interPol (restoPol p) q
  |m > n = interPol p (restoPol q)
  |b == c =  consPol n b (interPol (restoPol p) (restoPol q))
  |otherwise = interPol (restoPol p) (restoPol q)
     where n = grado p
           m = grado q
           b = coefLider p
           c = coefLider q
