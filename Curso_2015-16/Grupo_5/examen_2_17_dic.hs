-- Informática (1º del Grado en Matemáticas)
-- 1º examen de evaluación continua (7 de diciembre de 2015)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por comprensión, la función
--    sumaDivC :: Int -> [Int] -> Int
-- tal que (SumaDivC x xs) es la suma de los cuadrados de los 
-- elementos de xs que son divisibles por x. Por ejemplo,
--    sumaDivC 3 [1..7] == 45
--    sumaDivC 2 [1..7] == 56
-- ---------------------------------------------------------------------

sumaDivC :: Int -> [Int] -> Int
sumaDivC x xs = sum [y^2 | y <- xs, rem y x == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, usando funciones de orden superior, la
-- función 
--    sumaDivS :: Int -> [Int] -> Int
-- tal que (SumaDivS x xs) es la suma de los cuadrados de los 
-- elementos de xs que son divisibles por x. Por ejemplo,
--    sumaDivS 3 [1..7] == 45
--    sumaDivS 2 [1..7] == 56
-- ---------------------------------------------------------------------

sumaDivS :: Int -> [Int] -> Int
sumaDivS x =   sum . map (^2) . filter (\ y -> rem y x == 0)

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir, por recursión, la función 
--    sumaDivR :: Int -> [Int] -> Int
-- tal que (SumaDivR x xs) es la suma de los cuadrados de los 
-- elementos de xs que son divisibles por x. Por ejemplo,
--    sumaDivR 3 [1..7] == 45
--    sumaDivR 2 [1..7] == 56
-- ---------------------------------------------------------------------

sumaDivR :: Int -> [Int] -> Int
sumaDivR _ [] = 0
sumaDivR x (y:xs) | rem y x == 0 = y^2+sumaDivR x xs
                  | otherwise = sumaDivR x xs

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Definir, por plegado, la función 
--    sumaDivP :: Int -> [Int] -> Int
-- tal que (SumaDivP x xs) es la suma de los cuadrados de los 
-- elementos de xs que son divisibles por x. Por ejemplo,
--    sumaDivP 3 [1..7] == 45
--    sumaDivP 2 [1..7] == 56
-- ---------------------------------------------------------------------

sumaDivP :: Int -> [Int] -> Int
sumaDivP x = foldr (\y z -> if rem y x == 0 then y^2+z else z)  0

-- ---------------------------------------------------------------------
-- Ejercicio 2. Una lista de enteros positivos se dirá encadenada si el
-- último dígito de cada elemento coincide con el primer dígito del
-- siguiente; y el último dígito del último número coincide con el
-- primer dígito del primer número.  
-- 
-- Definir la función
--    encadenada :: [Int] -> Bool 
-- tal que (encadenada xs) se verifica si xs está encadenada. Por
-- ejemplo, 
--    encadenada [92,205,77,72]  == False
--    encadenada [153,32,207,71] == True
--    encadenada [153,32,207,75] == False 
--    encadenada [123]           == False
--    encadenada [121]           == True
-- ---------------------------------------------------------------------

encadenada :: [Int] -> Bool
encadenada (x:xs) = 
    and [last (show a) == head (show b) | (a,b) <- zip (x:xs) (xs++[x])]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Un entero positivo se dirá especial si la suma de sus
-- dígitos coincide con el número de sus divisores. Por ejemplo, 2015 es
-- especial, puesto que la suma de sus dígitos es 2+0+1+5=8 y tiene 8
-- divisores (1,5,13,31,65,155,403 y 2015).  
-- 
-- Definir la sucesión infinita
--    especiales :: [Int]
-- formada por todos los números especiales. Por ejemplo:
--    take 12 especiales == [1,2,11,22,36,84,101,152,156,170,202,208]
-- ---------------------------------------------------------------------

especiales :: [Int]
especiales = [x | x <- [1..], sum (digitos x) == length (divisores x)]

divisores :: Int -> [Int]
divisores x =[y | y <- [1..x], rem x y == 0]

digitos :: Int -> [Int]
digitos x = [read [c]| c <- show x]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    siguienteEspecial :: Int -> Int
-- tal que (siguienteEspecial x) es el primer entero mayor que x que es
-- especial. Por ejemplo, 
--    siguienteEspecial 2015 == 2101
-- ---------------------------------------------------------------------

siguienteEspecial :: Int -> Int
siguienteEspecial x =  head (dropWhile (<=x) especiales)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función 
--    palabras :: String -> [String]
-- tal que (palabras xs) es la lista de las palabras que aparecen en xs,
-- eliminando los espacios en blanco. Por ejemplo, 
--    ghci> palabras " el lagarto   esta    llorando " 
--    ["el","lagarto","esta","llorando"]
-- ---------------------------------------------------------------------

palabras :: String -> [String]
palabras [] = []
palabras (x:xs) 
   | x ==' '   = palabras (dropWhile (==' ') xs)
   | otherwise = (x:takeWhile (/=' ') xs):palabras (dropWhile (/=' ') xs) 

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Los árboles binarios se representan mediante el tipo de
-- datos 
--    data Arbol a = H a | N a (Arbol a) (Arbol a) deriving Show
-- 
-- Definir la función 
--    maxHojas :: Ord a => Arbol a -> a
-- tal que (maxHojas t) es el mayor valor que aparece en una hoja del
-- árbol t. Por ejemplo, 
--    ghci> maxHojas (N 14 (N 2 (H 7) (H 10)) (H 3))
--    10
-- ---------------------------------------------------------------------

data Arbol a = H a | N a (Arbol a) (Arbol a) deriving Show

maxHojas :: Ord a => Arbol a -> a
maxHojas (H x)     = x
maxHojas (N _ i d) = max (maxHojas i) (maxHojas d)

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir la función
--    sumaValor :: Num a => a -> Arbol a -> Arbol a
-- tal que (sumaValor v t) es el árbol obtenido a partir de t al sumar v
-- a todos sus nodos. Por ejemplo, 
--    ghci> sumaValor 7 (N 14 (N 2 (H 7) (H 10)) (H 3))
--    N 21 (N 9 (H 14) (H 17)) (H 10)
-- ---------------------------------------------------------------------

sumaValor :: Num a => a -> Arbol a -> Arbol a
sumaValor v (H x)     = H (v+x)
sumaValor v (N x i d) = N (v+x) (sumaValor v i) (sumaValor v d)
