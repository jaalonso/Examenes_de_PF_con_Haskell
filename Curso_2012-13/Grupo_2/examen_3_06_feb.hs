-- Informática (1º del Grado en Matemáticas)
-- 3º examen de evaluación continua (6 de febrero de 2013)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por recursión, la función 
--    sumaR :: Num a => [[a]] -> a
-- tal que (sumaR xss) es la suma de todos los elementos de todas las
-- listas de xss. Por ejemplo,
--    sumaR [[1,3,5],[2,4,1],[3,7,9]]  ==  35
-- ---------------------------------------------------------------------

sumaR :: Num a => [[a]] -> a
sumaR []       = 0
sumaR (xs:xss) = sum xs + sumaR xss

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, por plegado, la función 
--    sumaP :: Num a => [[a]] -> a
-- tal que (sumaP xss) es la suma de todos los elementos de todas las
-- listas de xss. Por ejemplo,
--    sumaP [[1,3,5],[2,4,1],[3,7,9]]  ==  35
-- ---------------------------------------------------------------------

sumaP :: Num a => [[a]] -> a
sumaP = foldr (\x y -> (sum x) + y) 0

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    raicesEnteras :: Int -> Int -> Int -> [Int]
-- tal que (raicesEnteras a b c) es la lista de las raices enteras de la
-- ecuación ax^2+bx+c = 0. Por ejemplo,
--    raicesEnteras 1 (-6) 9     ==  [3]
--    raicesEnteras 1 (-6) 0     ==  [0,6]
--    raicesEnteras 5 (-6) 0     ==  [0]
--    raicesEnteras 1 1 (-6)     ==  [2,-3]
--    raicesEnteras 2 (-1) (-6)  ==  [2]
--    raicesEnteras 2 0 0        ==  [0]
--    raicesEnteras 6 5 (-6)     ==  []
-- Usando raicesEnteras calcular las raíces de la ecuación
-- 7x^2-11281x+2665212 = 0. 
-- ---------------------------------------------------------------------

raicesEnteras :: Int -> Int -> Int -> [Int]
raicesEnteras a b c 
  | b == 0 && c == 0       = [0]
  | c == 0 && rem b a /= 0 = [0]
  | c == 0 && rem b a == 0 = [0,-b `div` a]
  | otherwise              = [x | x <- divisores c, a*(x^2) + b*x + c == 0]

-- (divisores n) es la lista de los divisores enteros de n. Por ejemplo, 
--    divisores (-6)  ==  [1,2,3,6,-1,-2,-3,-6]
divisores :: Int -> [Int]
divisores n = ys ++ (map (0-) ys)
  where ys = [x | x <-[1..abs n], mod n x == 0] 

-- Una definición alternativa es
raicesEnteras2 a b c = [floor x | x <- raices a b c, esEntero x]

-- (esEntero x) se verifica si x es un número entero.
esEntero x = ceiling x == floor x

-- (raices a b c) es la lista de las raices reales de la ecuación
-- ax^2+b*x+c = 0.
raices a b c | d < 0     = []
             | d == 0    = [y1]
             | otherwise = [y1,y2]
  where d = b^2 - 4*a*c
        y1 = ((-b) + sqrt d)/(2*a)
        y2 = ((-b) - sqrt d)/(2*a)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    segmentos :: (a -> Bool) -> [a] -> [[a]]
-- tal que (segmentos p xs) es la lista de los segmentos de xs cuyos
-- elementos no verifican la propiedad p. Por ejemplo,
--    segmentos odd [1,2,0,4,5,6,48,7,2]   ==  [[],[2,0,4],[6,48],[2]]
--    segmentos odd [8,6,1,2,0,4,5,6,7,2]  ==  [[8,6],[2,0,4],[6],[2]]
-- ---------------------------------------------------------------------

segmentos :: (a -> Bool) -> [a] -> [[a]]
segmentos _ [] = []
segmentos p xs = 
  takeWhile (not.p) xs : (segmentos p (dropWhile p (dropWhile (not.p) xs)))

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Un número n es especial si al concatenar n y n+1 se
-- obtiene otro número que es divisible entre la suma de n y n+1. Por
-- ejemplo, 1, 4, 16 y 49 son especiales ya que
--        1+2  divide a   12     -       12/3  =  4
--        4+5  divide a   45     -       45/9  =  5
--       16+17 divide a 1617     -     1617/33 = 49
--       49+50 divide a 4950     -     4950/99 = 50
-- Definir la función
--    esEspecial :: Integer -> Bool
-- tal que (esEspecial n) se verifica si el número obtenido concatenando
-- n y n+1 es divisible entre la suma de n y n+1. Por ejemplo,
--    esEspecial 4  ==  True
--    esEspecial 7  ==  False
-- ---------------------------------------------------------------------

esEspecial :: Integer -> Bool
esEspecial n = pegaNumeros n (n+1) `rem` (2*n+1) == 0

-- (pegaNumeros x y) es el número resultante de "pegar" los
-- números x e y. Por ejemplo, 
--    pegaNumeros 12 987   ==  12987
--    pegaNumeros 1204 7   ==  12047
--    pegaNumeros 100 100  ==  100100
pegaNumeros :: Integer -> Integer -> Integer
pegaNumeros x y
    | y < 10    = 10*x+y
    | otherwise = 10 * pegaNumeros x (y `div` 10) + (y `mod` 10)  

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función
--    especiales :: Int -> [Integer]
-- tal que (especiales n) es la lista de los n primeros números
-- especiales. Por ejemplo, 
--    especiales 5  ==  [1,4,16,49,166]
-- ---------------------------------------------------------------------

especiales :: Int -> [Integer]
especiales n = take n [x | x <- [1..], esEspecial x]
