-- Informática (1º del Grado en Matemáticas)
-- 1º examen de evaluación continua (6 de noviembre de 2015)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por comprensión, la función
--    tieneRepeticionesC :: Eq a => [a] -> Bool
-- tal que (tieneRepeticionesC xs) se verifica si xs tiene algún
-- elemento repetido. Por ejemplo, 
--    tieneRepeticionesC [3,2,5,2,7]          ==  True
--    tieneRepeticionesC [3,2,5,4]            ==  False
--    tieneRepeticionesC (5:[1..2000000000])  ==  True
--    tieneRepeticionesC [1..20000]           ==  False
-- ---------------------------------------------------------------------

tieneRepeticionesC :: Eq a => [a] -> Bool
tieneRepeticionesC xs =
  or [ocurrencias x xs > 1 | x <- xs] 
  where ocurrencias x xs = length [y | y <- xs, x == y]

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por comprensión, la función
--    tieneRepeticionesR :: Eq a => [a] -> Bool
-- tal que (tieneRepeticionesR xs) se verifica si xs tiene algún
-- elemento repetido. Por ejemplo, 
--    tieneRepeticionesR [3,2,5,2,7]          ==  True
--    tieneRepeticionesR [3,2,5,4]            ==  False
--    tieneRepeticionesR (5:[1..2000000000])  ==  True
--    tieneRepeticionesR [1..20000]           ==  False
-- ---------------------------------------------------------------------

tieneRepeticionesR :: Eq a => [a] -> Bool
tieneRepeticionesR []     = False
tieneRepeticionesR (x:xs) = elem x xs || tieneRepeticionesR xs

-- --------------------------------------------------------------------
-- Ejercicio 2. Definir, por recursión, la función
--    sinRepetidos :: Eq a => [a] -> [a]
-- tal que (sinRepetidos xs) es la lista xs sin repeticiones, da igual
-- el orden. Por ejemplo,
--    sinRepetidos [1,1,1,2]             == [1,2]
--    sinRepetidos [1,1,2,1,1]           == [2,1]
--    sinRepetidos [1,2,4,3,4,2,5,3,4,2] == [1,5,3,4,2]
-- ---------------------------------------------------------------------

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs)
  | x `elem` xs = sinRepetidos xs
  | otherwise   = x : sinRepetidos xs

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir, por recursión, la función
--    reparte :: [a] -> [Int] -> [[a]]
-- tal que (reparte xs ns) es la partición de xs donde las longitudes de
-- las partes las indican los elementos de ns. Por ejemplo, 
--    reparte [1..10] [2,5,0,3]  ==  [[1,2],[3,4,5,6,7],[],[8,9,10]] 
--    reparte [1..10] [1,4,2,3]  ==  [[1],[2,3,4,5],[6,7],[8,9,10]]
-- ---------------------------------------------------------------------

reparte :: [a] -> [Int] -> [[a]]
reparte [] _     = []
reparte _ []     = []
reparte xs (n:ns) = take n xs : reparte (drop n xs) ns

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función 
--    agrupa :: Eq a => (b -> a) -> [b] -> [(a, [b])]
-- tal que (agrupa f xs) es la lista de pares  obtenida agrupando los
-- elementos de xs según sus valores mediante la función f. Por ejemplo,  
-- si queremos agrupar los elementos de la lista ["voy", "ayer", "ala",
-- "losa"] por longitudes, saldrá que hay dos palabras de longitud 3 y
-- otras dos de longitud 4
--    ghci> agrupa length ["voy", "ayer", "ala", "losa"]
--    [(3,["voy","ala"]),(4,["ayer","losa"])]
-- Si queremos agrupar las palabras de la lista ["claro", "ayer", "ana",
-- "cosa"] por su inicial, salen dos por la 'a' y otras dos por la 'c'.
--    ghci> agrupa head ["claro", "ayer", "ana", "cosa"]
--    [('a',["ayer","ana"]),('c',["claro","cosa"])]
-- ---------------------------------------------------------------------

agrupa :: Eq a => (b -> a) -> [b] -> [(a, [b])]
agrupa f xs =
  [(i,[y | y <- xs, f y == i]) | i <- yss]
  where yss = sinRepetidos [f y | y <- xs]
