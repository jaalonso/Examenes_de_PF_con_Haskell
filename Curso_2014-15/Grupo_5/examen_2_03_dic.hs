-- Informática (1º del Grado en Matemáticas)
-- 2º examen de evaluación continua (3 de diciembre de 2014)
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función 
--    trenza :: [a] -> [a] -> [a]
-- tal que (trenza xs ys) es la lista obtenida intercalando los
-- elementos de xs e ys. Por ejemplo,
--    trenza [5,1] [2,7,4]             ==  [5,2,1,7]
--    trenza [5,1,7] [2..]             ==  [5,2,1,3,7,4]
--    trenza [2..] [5,1,7]             ==  [2,5,3,1,4,7]
--    take 8 (trenza [2,4..] [1,5..])  ==  [2,1,4,5,6,9,8,13]
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
trenza :: [a] -> [a] -> [a]
trenza xs ys = concat [[x,y] | (x,y) <- zip xs ys]

-- 2ª definición (por zipWith):
trenza2 :: [a] -> [a] -> [a]
trenza2 xs ys = concat (zipWith par xs ys)
    where par x y = [x,y]

-- 3ª definición (por zipWith y sin argumentos):
trenza3 :: [a] -> [a] -> [a]
trenza3 = (concat .) . zipWith par
    where par x y = [x,y]

-- 4ª definición (por recursión):
trenza4 :: [a] -> [a] -> [a]
trenza4 (x:xs) (y:ys) = x : y : trenza xs ys
trenza4 _      _      = []

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck que el número de elementos de
-- (trenza xs ys) es el doble del mínimo de los números de elementos de
-- xs e ys.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_trenza :: [Int] -> [Int] -> Bool
prop_trenza xs ys =
    length (trenza xs ys) == 2 * min (length xs) (length ys)
            
-- La comprobación es
--    ghci> quickCheck prop_trenza
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Dado un número cualquiera, llamamos MDI de ese número
-- a su mayor divisor impar. Así, el MDI de 12 es 3 y el MDI de 15 es 15. 
-- 
-- Definir la función 
--    mdi :: Int -> Int
-- tal que (mdi n) es el mayor divisor impar de n. Por ejemplo,
--    mdi 12  ==  3
--    mdi 15  ==  15
-- ----------------------------------------------------------------------

mdi :: Int -> Int
mdi n | odd n     = n
      | otherwise = head [x | x <- [n-1,n-3..1], n `rem` x == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Comprobar con QuickCheck que la suma de los MDI de los
-- números n+1, n+2, ..., 2n de cualquier entero positivo n siempre da
-- n^2. 
-- 
-- Nota. Al hacer la comprobación limitar el tamaño de las pruebas como
-- se indica a continuación
--    ghci> quickCheckWith (stdArgs {maxSize=5}) prop_mdi
--    +++ OK, passed 100 tests.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_mdi :: Int -> Property
prop_mdi n =
    n > 0 ==> sum [mdi x | x <- [n+1..2*n]] == n^2


prop_mdi2 :: Int -> Property
prop_mdi2 n =
    n > 0 ==> sum (map mdi [n+1..2*n]) == n^2

-- La comprobación es
--    ghci> quickCheckWith (stdArgs {maxSize=5}) prop_mdi
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función 
--    reiteracion :: Int -> (a -> a) -> a -> a
-- tal que (reiteracion n f x) es el resultado de aplicar n veces la
-- función f a x. Por ejemplo,
--    reiteracion 10 (+1) 5  ==  15
--    reiteracion 10 (+5) 0  ==  50
--    reiteracion  4 (*2) 1  ==  16
--    reiteracion  4 (5:) [] ==  [5,5,5,5]
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión):
reiteracion :: Int -> (a -> a) -> a -> a
reiteracion 0 f x = x
reiteracion n f x = f (reiteracion (n-1) f x)

-- 2ª definición (por recursión sin el 3ª argumento):
reiteracion2 :: Int -> (a -> a) -> a -> a
reiteracion2 0 f = id
reiteracion2 n f = f . reiteracion2 (n-1) f

-- 3ª definición (con iterate):
reiteracion3 :: Int -> (a -> a) -> a -> a
reiteracion3 n f x = (iterate f x) !! n

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Comprobar con QuickCheck que se verifican las
-- siguientes propiedades 
--    reiteracion 10 (+1) x  == 10 + x 
--    reiteracion 10 (+x) 0  == 10 * x 
--    reiteracion 10 (x:) [] == replicate 10 x  
-- ---------------------------------------------------------------------

-- La propiedad es
prop_reiteracion :: Int -> Bool
prop_reiteracion x =
    reiteracion 10 (+1) x  == 10 + x &&  
    reiteracion 10 (+x) 0  == 10 * x &&
    reiteracion 10 (x:) [] == replicate 10 x  
    
-- La comprobación es
--    ghci> quickCheck prop_reiteracion
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la constante
--    cadenasDe0y1 :: [String]
-- tal que cadenasDe0y1 es la lista de todas las cadenas de ceros y
-- unos. Por ejemplo, 
--    ghci> take 10 cadenasDe0y1
--    ["","0","1","00","10","01","11","000","100","010"]
-- ---------------------------------------------------------------------

cadenasDe0y1 :: [String]
cadenasDe0y1 = "" : concat [['0':cs, '1':cs] | cs <- cadenasDe0y1]

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función
--      posicion :: String -> Int
-- tal que (posicion cs) es la posición de la cadena cs en la lista  
-- cadenasDe0y1. Por ejemplo,
--      posicion "1"   == 2
--      posicion "010" == 9
-- ---------------------------------------------------------------------

posicion :: String -> Int
posicion cs = 
    length (takeWhile (/= cs) cadenasDe0y1)

-- ---------------------------------------------------------------------
-- Ejercicio 5. El siguiente tipo de dato representa expresiones
-- construidas con números, variables, sumas y productos
--    data Expr = N Int
--              | V String
--              | S Expr Expr
--              | P Expr Expr
-- Por ejemplo, x*(5+z) se representa por (P (V "x") (S (N 5) (V "z"))) 
-- 
-- Definir la función
--    reducible :: Expr -> Bool
-- tal que (reducible a) se verifica si a es una expresión reducible; es
-- decir, contiene una operación en la que los dos operandos son números. 
-- Por ejemplo,
--    reducible (S (N 3) (N 4))             == True
--    reducible (S (N 3) (V "x"))           == False
--    reducible (S (N 3) (P (N 4) (N 5)))   == True
--    reducible (S (V "x") (P (N 4) (N 5))) == True
--    reducible (S (N 3) (P (V "x") (N 5))) == False
--    reducible (N 3)                       == False
--    reducible (V "x")                     == False
-- ---------------------------------------------------------------------

data Expr = N Int
          | V String
          | S Expr Expr
          | P Expr Expr

reducible :: Expr -> Bool
reducible (N _)           = False
reducible (V _)           = False
reducible (S (N _) (N _)) = True
reducible (S a b)         = reducible a || reducible b
reducible (P (N _) (N _)) = True
reducible (P a b)         = reducible a || reducible b
