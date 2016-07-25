-- Informática (1º del Grado en Matemáticas, Grupo 5)
-- 4º examen de evaluación continua (10 de marzo de 2016)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.List
import Data.Numbers.Primes
import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la lista infinita
--    paresRel :: [(Int,Int)]
-- tal que paresRel enumera todos los pares de enteros positivos (a,b), 
-- con 1 <= a < b, tales que a y b tienes los mismos divisores primos. 
-- Por ejemplo,
--    ghci> take 10 paresRel
--    [(2,4),(2,8),(4,8),(3,9),(6,12),(2,16),(4,16),(8,16),(6,18),(12,18)]
-- ---------------------------------------------------------------------

paresRel :: [(Int,Int)]
paresRel = [(a,b) | b <- [1..], a <- [1..b-1], p a b]
    where p a b = nub (primeFactors a) == nub (primeFactors b) 

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. ¿Qué lugar ocupa el par (750,1080) en la lista
-- infinita paresRel? 
-- ---------------------------------------------------------------------

-- El cálculo es
--    ghci> 1 + length (takeWhile (/=(750,1080)) paresRel)
--    1492

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir la función
--    segmento :: Eq a => [a] -> [a]
-- tal que (segmento xs) es el mayor segmento inicial de xs que no
-- contiene ningún elemento repetido. Por ejemplo: 
--    segmento [1,2,3,2,4,5] == [1,2,3] 
--    segmento "caceres"     == "ca"
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

segmento1 :: Eq a => [a] -> [a]
segmento1 = last . filter (\ys -> nub ys == ys) . inits

-- 2ª solución
-- ===========

segmento2 :: Eq a => [a] -> [a]
segmento2 xs = aux xs []
    where aux [] ys = reverse ys
          aux (x:xs) ys | x `elem` ys = reverse ys
                        | otherwise   = aux xs (x:ys)

-- Comparación de eficiencia
-- =========================

--    ghci> last (segmento1 [1..10^3])
--    1000
--    (6.19 secs, 59,174,640 bytes)
--    ghci> last (segmento2 [1..10^3])
--    1000
--    (0.04 secs, 0 bytes)

-- Solución
-- ========

-- En lo que sigue usaremos la 2ª definición
segmento :: Eq a => [a] -> [a]
segmento = segmento2

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Se observa que 10! = 3628800 comienza con 4 dígitos
-- distintos (después se repite el dígito 8). 
-- 
-- Calcular el menor número natural cuyo factorial comienza con 9
-- dígitos distintos.  
-- ---------------------------------------------------------------------

factorial :: Integer -> Integer
factorial x = product [1..x]

-- El cálculo es
--    ghci> head [x | x <- [0..], length (segmento (show (factorial x))) == 9]
--    314

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Las expresiones aritméticas se pueden definir mediante
-- el siguiente tipo de dato
--    data Expr  = N Int | V Char | Sum Expr Expr | Mul Expr Expr 
--                 deriving Show
-- Por ejemplo, (x+3)+(7*y) se representa por
--    ejExpr :: Expr
--    ejExpr = Sum (Sum (V 'x') (N 3))(Mul (N 7) (V 'y')) 
-- 
-- Definir el predicado
--    numerico :: Expr -> Bool
-- tal que (numerico e) se verifica si la expresión e no contiene ninguna 
-- variable. Por ejemplo,
--   numerico ejExpr            == False
--   numerico (Sum (N 7) (N 9)) == True
-- ---------------------------------------------------------------------

data Expr  = N Int | V Char | Sum Expr Expr | Mul Expr Expr 
             deriving Show

ejExpr :: Expr
ejExpr = Sum (Sum (V 'x') (N 3))(Mul (N 7) (V 'y')) 

numerico :: Expr -> Bool
numerico (N _)       = True
numerico (V _)       = False
numerico (Sum e1 e2) = numerico e1 && numerico e2
numerico (Mul e1 e2) = numerico e1 && numerico e2

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función 
--    evalua :: Expr -> Maybe Int
-- tal que (evalua e) devuelve 'Just v' si la expresión e es numérica y
-- v es su valor, o bien 'Nothing' si e no es numérica. Por ejemplo: 
--    evalua ejExpr            == Nothing
--    evalua (Sum (N 7) (N 9)) == Just 16
-- ---------------------------------------------------------------------

-- 1ª solución
evalua1 :: Expr -> Maybe Int
evalua1 e | null (aux e) = Nothing
          | otherwise    = Just (head (aux e))
    where aux (N x)       = [x]
          aux (V _)       = []
          aux (Sum e1 e2) = [x+y| x <- aux e1, y <- aux e2]
          aux (Mul e1 e2) = [x*y| x <- aux e1, y <- aux e2]

-- 2ª solución
evalua2 :: Expr -> Maybe Int
evalua2 e | numerico e = Just (valor e)
          | otherwise  = Nothing
    where valor (N x)       = x
          valor (Sum e1 e2) = valor e1 + valor e2
          valor (Mul e1 e2) = valor e1 * valor e2

-- ------------------------------------------------------------------
-- Ejercicio 4.1. Los vectores y matrices se definen usando tablas como
-- sigue: 
--    type Vector a = Array Int a
--    type Matriz a = Array (Int,Int) a
-- 
-- Un elemento de un vector es un máximo local si no tiene ningún
-- elemento adyacente mayor o igual que él. 
-- 
-- Definir la función 
--    posMaxVec :: Ord a => Vector a -> [Int]
-- tal que (posMaxVec p) devuelve las posiciones del vector p en las que 
-- p tiene un máximo local. Por ejemplo:
--    posMaxVec (listArray (1,6) [3,2,6,7,5,3]) == [1,4]
-- ---------------------------------------------------------------------

type Vector a = Array Int a
type Matriz a = Array (Int,Int) a

-- 1ª definición
posMaxVec :: Ord a => Vector a -> [Int]
posMaxVec p = 
    (if p!1 > p!2 then [1] else []) ++ 
    [i | i <- [2..n-1], p!(i-1) < p!i && p!(i+1) < p!i] ++
    (if p!(n-1) < p!n then [n] else [])
    where (_,n) = bounds p

-- 2ª definición
posMaxVec2 :: Ord a => Vector a -> [Int]
posMaxVec2 p = 
    [1 | p ! 1 > p ! 2] ++ 
    [i | i <- [2..n-1], p!(i-1) < p!i && p!(i+1) < p!i] ++
    [n | p ! (n - 1) < p ! n]
    where (_,n) = bounds p

-- 3ª definición
posMaxVec3 :: Ord a => Vector a -> [Int]
posMaxVec3 p = 
    [i | i <- [1..n],
         all (<p!i) [p!j | j <- vecinos i]]
    where (_,n) = bounds p
          vecinos 1 = [2]
          vecinos j | j == n    = [n-1]
                    | otherwise = [j-1,j+1]

-- ---------------------------------------------------------------------
-- Ejercicio 4.2.  Un elemento de una matriz es un máximo local si no
-- tiene ningún vecino mayor o igual que él. 
-- 
-- Definir la función 
--    posMaxMat :: Ord a => Matriz a -> [(Int,Int)]
-- tal que (posMaxMat p) es la lista de las posiciones de la matriz p en
-- las que p tiene un máximo local. Por ejemplo,
--    ghci> posMaxMat (listArray ((1,1),(3,3)) [1,20,15,4,5,6,9,8,7])
--    [(1,2),(3,1)]
-- ---------------------------------------------------------------------

posMaxMat :: Ord a => Matriz a -> [(Int,Int)]
posMaxMat p = [(x,y) | (x,y) <- indices p, all (<(p!(x,y))) (vs x y)]
   where (_,(n,m)) = bounds p
         vs x y = [p!(x+i,y+j) | i <- [-1..1], j <- [-1..1], 
                                 (i,j) /= (0,0), 
                                 (x+i) `elem` [1..n],
                                 (y+j) `elem` [1..m]]

-- -------------------------------------------------------------------
-- Ejercicio 5. Escribir una función Haskell
--    fun :: (Int -> Int) -> IO ()
-- que actúe como la siguente función programada en Maxima:
--    fun(f) := block([a,b], a:0, b:0,
--                    x:read("Escribe un natural" ),
--                    while b <= x do (a:a+f(b),b:b+1),
--                    print(a))$  
-- ---------------------------------------------------------------------

fun :: (Int -> Int) -> IO ()
fun f = do putStr "Escribe un natural: "
           xs <- getLine
           let a = sum [f i | i <- [0..read xs]]
           print a

-- Por ejemplo,
--    ghci> fun (^2)
--    Escribe un natural: 5
--    55
