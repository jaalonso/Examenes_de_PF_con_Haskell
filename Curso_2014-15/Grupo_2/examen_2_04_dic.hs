-- Informática (1º del Grado en Matemáticas)
-- 2º examen de evaluación continua (4 de diciembre de 2014)   
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    vuelta :: [Int] -> [a] -> [a]
-- tal que (vuelta ns xs) es la lista que resulta de repetir cada
-- elemento de xs tantas veces como indican los elementos de ns
-- respectivamente. Por ejemplo,
--    vuelta [1,2,3,2,1] "ab"       == "abbaaabba"
--    vuelta [2,3,1,5] [6,5,7]      == [6,6,5,5,5,7,6,6,6,6,6]
--    take 13 (vuelta [1 ..] [6,7]) == [6,7,7,6,6,6,7,7,7,7,6,6,6]
-- ---------------------------------------------------------------------

-- 1ª definición (por recursión):
vuelta :: [Int] -> [a] -> [a]
vuelta (n:ns) (x:xs) = replicate n x ++ vuelta ns (xs++[x])
vuelta [] _ = []

-- 2ª definición (por comprensión):
vuelta2 :: [Int] -> [a] -> [a]
vuelta2 ns xs = concat [replicate n x | (n,x) <- zip ns (rep xs)]

-- (rep xs) es la lista obtenida repitiendo los elementos de xs. Por
-- ejemplo, 
--    take 20 (rep "abbccc")  ==  "abbcccabbcccabbcccab"
rep xs = xs ++ rep xs

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir (por comprensión, recursión y plegado por la
-- derecha,  acumulador y plegado por la izquierda) la funcion  
--    posit :: ([Int] -> Int) -> [[Int]] ->  [[Int]]
-- tal que (posit f xss) es la lista formada por las listas de xss tales
-- que, al evaluar f sobre ellas, devuelve un valor positivo. Por
-- ejemplo, 
--    posit head [[1,2],[0,-4],[2,-3]]   ==  [[1,2],[2,-3]]
--    posit sum  [[1,2],[9,-4],[-8,3]]   ==  [[1,2],[9,-4]]
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
positC :: ([Int] -> Int) -> [[Int]] ->  [[Int]]
positC f xss = [xs | xs <- xss, f xs > 0]

-- 2ª definición (por recursión):
positR :: ([Int] -> Int) -> [[Int]] ->  [[Int]]
positR f [] = []
positR f (xs:xss) | f xs > 0  = xs : positR f xss
                  | otherwise = positR f xss

-- 3ª definición (por plegado por la derecha):
positP :: ([Int] -> Int) -> [[Int]] ->  [[Int]]
positP f = foldr g []
    where g xs yss | f xs > 0  = xs : yss 
                   | otherwise = yss

-- 4ª definición (con acumulador):
positAC :: ([Int] -> Int) -> [[Int]] ->  [[Int]] 
positAC f xss = reverse (aux f xss [])  
    where aux f [] yss = yss
          aux f (xs:xss) yss | f xs > 0  = aux f xss (xs:yss)
                             | otherwise = aux f xss yss

-- 5ª definición (por plegado por la izquierda):
positPL :: ([Int] -> Int) -> [[Int]] ->  [[Int]] 
positPL f xss = reverse (foldl g [] xss)
    where g yss xs | f xs > 0  = xs : yss
                   | otherwise = yss
                        
-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, usando la función posit,  
--    p1 :: [[Int]]
-- tal que p1 es la lista de listas de [[1,2,-3],[4,-5,-1],[4,1,2,-5,-6]]
-- que cumplen que la suma de los elementos que ocupan posiciones pares
-- es negativa o cero.
-- ---------------------------------------------------------------------

p1 :: [[Int]]
p1 = [xs | xs <- l, xs `notElem` positP f l] 
    where l = [[1,2,-3],[4,-5,-1],[4,1,2,-5,-6]]
          f []       = 0
          f [x]      = x
          f (x:_:xs) = x + f xs
          
-- El cálculo es
--    ghci> p1
--    [[1,2,-3],[4,1,2,-5,-6]]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función 
--    maxCumplen :: (a -> Bool) -> [[a]] -> [a]
-- tal que (maxCumplen p xss) es la lista de xss que tiene más elementos
-- que cumplen el predicado p. Por ejemplo, 
--    maxCumplen even [[3,2],[6,8,7],[5,9]]  ==  [6,8,7]
--    maxCumplen odd  [[3,2],[6,8,7],[5,9]]  ==  [5,9]
--    maxCumplen (<5) [[3,2],[6,8,7],[5,9]]  ==  [3,2]
-- ---------------------------------------------------------------------

maxCumplen :: (a -> Bool) -> [[a]] -> [a]
maxCumplen p xss = head [xs | xs <- xss, f xs == m]
    where m    = maximum [f xs | xs <- xss]
          f xs = length (filter p xs)

