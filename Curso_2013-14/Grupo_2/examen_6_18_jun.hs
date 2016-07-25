-- Informática: 6º examen de evaluación continua (18 de junio de 2014)
-- ---------------------------------------------------------------------

-- Librería auxiliar
-- =================
import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1 [2 puntos]. Definir la función
--    divisiblesPorPrimero :: [Int] -> Bool
-- tal que (divisibles xs) se verifica si todos los elementos positivos
-- de xs son divisibles por el primero. Por ejemplo,
--    divisiblesPorPrimero [2,6,-3,0,18,-17,10]  ==  True
--    divisiblesPorPrimero [-13]                 ==  True
--    divisiblesPorPrimero [-3,6,1,-3,9,18]      ==  False
--    divisiblesPorPrimero [5,-2,-6,3]           ==  False
--    divisiblesPorPrimero []                    ==  False
--    divisiblesPorPrimero [0,2,4]               ==  False
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión)
divisiblesPorPrimero1 :: [Int] -> Bool
divisiblesPorPrimero1 []     = False
divisiblesPorPrimero1 (0:_)  = False
divisiblesPorPrimero1 (x:xs) = and [y `rem` x == 0 | y <- xs, y > 0]

-- 2ª definición (por recursión)
divisiblesPorPrimero2 :: [Int] -> Bool
divisiblesPorPrimero2 []     = False
divisiblesPorPrimero2 (0:_)  = False
divisiblesPorPrimero2 (x:xs) = aux xs
    where aux [] = True
          aux (y:ys) | y > 0     = y `rem` x == 0 && aux ys
                     | otherwise = aux ys 

-- ---------------------------------------------------------------------
-- Ejercicio 2 [2 puntos]. Definir la constante
--    primosConsecutivosConMediaCapicua :: [(Int,Int,Int)]
-- tal que primosConsecutivosConMediaCapicua es la lista de las ternas 
-- (x,y,z) tales que x e y son primos consecutivos tales que su media,
-- z, es capicúa. Por ejemplo,
--    ghci> take 5 primosConsecutivosConMediaCapicua
--    [(3,5,4),(5,7,6),(7,11,9),(97,101,99),(109,113,111)]
-- Calcular cuántos hay anteriores a 2014.
-- ---------------------------------------------------------------------

primosConsecutivosConMediaCapicua :: [(Int,Int,Int)]
primosConsecutivosConMediaCapicua =
    [(x,y,z) | (x,y) <- zip primos (tail primos),
               let z = (x + y) `div` 2,
               capicua z]

-- (primo x) se verifica si x es primo. Por ejemplo,
--    primo 7  ==  True
--    primo 8  ==  False
primo :: Int -> Bool
primo x = [y | y <- [1..x], x `rem` y == 0] == [1,x]

-- primos es la lista de los números primos mayores que 2. Por ejemplo,
--    take 10 primos  ==  [3,5,7,11,13,17,19,23,29]
primos :: [Int]
primos = [x | x <- [3,5..], primo x]

-- (capicua x) se verifica si x es capicúa. Por ejemplo,
capicua :: Int -> Bool
capicua x = ys == reverse ys
    where ys = show x

-- El cálculo es
--    ghci> length (takeWhile (\(x,y,z) -> y < 2014) primosConsecutivosConMediaCapicua)
--    20

-- ---------------------------------------------------------------------
-- Ejercicio 3 [2 puntos]. Un elemento x de un conjunto xs es minimal
-- respecto de una relación r si no existe ningún elemento y en xs tal
-- que (r y x). Por ejemplo,  
-- 
-- Definir la función
--    minimales :: Eq a => (a -> a -> Bool) -> [a] -> [a]
-- tal que (minimales xss) es la lista de los elementos minimales de
-- xs. Por ejemplo, 
--    ghci> minimales (\x y -> y `rem` x == 0) [2,3,6,12,18]
--    [2,3]
--    ghci> minimales (\x y -> x `rem` y == 0) [2,3,6,12,18]
--    [12,18]
--    ghci> minimales (\x y -> maximum x < maximum y) ["ac","cd","aacb"]
--    ["ac","aacb"]
--    ghci> minimales (\xs ys -> all (`elem` ys) xs) ["ab","c","abc","d","dc"]
--    ["ab","c","d"]
-- ---------------------------------------------------------------------

minimales :: Eq a => (a -> a -> Bool) -> [a] -> [a]
minimales r xs = [x | x <- xs, esMinimal r xs x]

-- (esMinimal r xs x) s verifica si xs no tiene ningún elemento menor
-- que x respecto de la relación r. 
esMinimal :: Eq a => (a -> a -> Bool) -> [a] -> a -> Bool
esMinimal r xs x = null [y | y <- xs, y /= x, r y x]

-- ---------------------------------------------------------------------
-- Ejercicio 4 [2 puntos]. Una matriz es monomial si en cada una de sus
-- filas y columnas todos los elementos son nulos excepto 1. Por
-- ejemplo, de las matrices 
--    |0  0 3 0|     |0  0 3 0|
--    |0 -2 0 0|     |0 -2 0 0|
--    |1  0 0 0|     |1  0 0 0|
--    |0  0 0 1|     |0  1 0 1|
-- la primera es monomial y la segunda no lo es.
--
-- Las matrices puede representarse mediante tablas cuyos
-- índices son pares de números naturales:    
--    type Matriz = Array (Int,Int) Int
-- Por ejemplo, las matrices anteriores se pueden definir por
--    ej1, ej2 :: Matriz
--    ej1 = listArray ((1,1),(4,4)) [0,  0, 3, 0,
--                                   0, -2, 0, 0,
--                                   1,  0, 0, 0,
--                                   0,  0, 0, 1]
--    ej2 = listArray ((1,1),(4,4)) [0,  0, 3, 0,
--                                   0, -2, 0, 0,
--                                   1,  0, 0, 0,
--                                   0,  1, 0, 1]
-- Definir la función 
--    esMonomial :: Matriz -> Bool
-- tal que (esMonomial p) se verifica si la matriz p es monomial. Por
-- ejemplo, 
--    esMonomial ej1  ==  True
--    esMonomial ej2  ==  False
-- ---------------------------------------------------------------------

type Matriz = Array (Int,Int) Int

ej1, ej2 :: Matriz
ej1 = listArray ((1,1),(4,4)) [0,  0, 3, 0,
                               0, -2, 0, 0,
                               1,  0, 0, 0,
                               0,  0, 0, 1]
ej2 = listArray ((1,1),(4,4)) [0,  0, 3, 0,
                               0, -2, 0, 0,
                               1,  0, 0, 0,
                               0,  1, 0, 1]

esMonomial :: Matriz -> Bool
esMonomial p = all esListaMonomial (filas p ++ columnas p)

-- (filas p) es la lista de las filas de la matriz p. Por ejemplo,
--    filas ej1  ==  [[0,0,3,0],[0,-2,0,0],[1,0,0,0],[0,0,0,1]]
filas :: Matriz -> [[Int]]
filas p = [[p!(i,j) | j <- [1..n]] | i <- [1..m]]
    where (_,(m,n)) = bounds p

-- (columnas p) es la lista de las columnas de la matriz p. Por ejemplo,
--    columnas ej1  ==  [[0,0,1,0],[0,-2,0,0],[3,0,0,0],[0,0,0,1]]
columnas :: Matriz -> [[Int]]
columnas p = [[p!(i,j) | i <- [1..m]] | j <- [1..n]]
    where (_,(m,n)) = bounds p

-- (esListaMonomial xs) se verifica si todos los elementos de xs excepto
-- uno son nulos. Por ejemplo,
--    esListaMonomial [0,3,0,0]  ==  True
--    esListaMonomial [0,3,0,2]  ==  False
--    esListaMonomial [0,0,0,0]  ==  False
esListaMonomial :: [Int] -> Bool
esListaMonomial xs = length (filter (/=0) xs) == 1

-- ---------------------------------------------------------------------
-- Ejercicio 5 [2 puntos]. Se consideran las expresiones vectoriales
-- formadas por un vector, la suma de dos expresiones vectoriales o el
-- producto de un entero por una expresión vectorial. El siguiente tipo
-- de dato define las expresiones vectoriales 
--    data ExpV = Vec Int Int
--              | Sum ExpV ExpV
--              | Mul Int ExpV
--              deriving Show
-- Definir la función 
--    valor :: ExpV -> (Int,Int)
-- tal que (valor e) es el valor de la expresión vectorial c. Por
-- ejemplo, 
--    valor (Vec 1 2)                                  ==  (1,2)
--    valor (Sum (Vec 1 2 ) (Vec 3 4))                 ==  (4,6)
--    valor (Mul 2 (Vec 3 4))                          ==  (6,8)
--    valor (Mul 2 (Sum (Vec 1 2 ) (Vec 3 4)))         ==  (8,12)
--    valor (Sum (Mul 2 (Vec 1 2)) (Mul 2 (Vec 3 4)))  ==  (8,12)
-- ---------------------------------------------------------------------

data ExpV = Vec Int Int
          | Sum ExpV ExpV
          | Mul Int ExpV
          deriving Show

-- 1ª solución
-- ===========
valor :: ExpV -> (Int,Int)
valor (Vec x y)   = (x,y)
valor (Sum e1 e2) = (x1+x2,y1+y2) where (x1,y1) = valor e1  
                                        (x2,y2) = valor e2  
valor (Mul n e)   = (n*x,n*y) where (x,y) = valor e  

-- 2ª solución
-- ===========
valor2 :: ExpV -> (Int,Int)
valor2 (Vec a b)   = (a, b)
valor2 (Sum e1 e2) = suma (valor2 e1) (valor2 e2)
valor2 (Mul n e1)  = multiplica n (valor2 e1)

suma :: (Int,Int) -> (Int,Int) -> (Int,Int)
suma (a,b) (c,d) = (a+c,b+d)

multiplica :: Int -> (Int, Int) -> (Int, Int)
multiplica n (a,b) = (n*a,n*b)
