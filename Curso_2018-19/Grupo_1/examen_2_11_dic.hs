-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 1º examen de evaluación continua (11 de noviembre de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función: 
--   deshacer :: [(a,b)] -> ([a],[b])
-- tal que (deshacer ps) es el par de listas que resultan de separar los
-- pares de ps. Por ejemplo,
--   ghci> deshacer [(3,'l'),(2,'u'),(5,'i'),(9,'s')]
--   ([3,2,5,9],"luis")
--
-- Comprobar con QuickCheck que deshacer es equivalente a la función
-- predefinida unzip.
-- ---------------------------------------------------------------------

-- 1ª definición
deshacer :: [(a,b)] -> ([a],[b])
deshacer ps = ([x | (x,_) <- ps], [y | (_,y) <- ps])
 
-- 2ª definición:
deshacer2 :: [(a,b)] -> ([a],[b])
deshacer2 ps = (map fst ps, map snd ps)
 
-- 3ª definición:
deshacer3 :: [(a,b)] -> ([a],[b])
deshacer3 []         = ([],[])
deshacer3 ((x,y):ps) = (x:xs,y:ys)
  where (xs,ys) = deshacer3 ps 
 
-- 4ª definición:
deshacer4 :: [(a,b)] -> ([a],[b])
deshacer4 = foldr f ([],[])
  where f (x,y) (xs,ys) = (x:xs, y:ys)

-- 5ª definición:
deshacer5 :: [(a,b)] -> ([a],[b])
deshacer5 = aux ([],[])
  where aux r []               = r
        aux (as,bs) ((x,y):ps) = aux (as++[x],bs++[y]) ps

-- 6ª definición:
deshacer6 :: [(a,b)] -> ([a],[b])
deshacer6 ps = (reverse us, reverse vs)
  where (us,vs) = foldl f ([],[]) ps
        f (xs,ys) (x,y) = (x:xs,y:ys)

-- La propiedad es
prop_deshacer :: [(Int,Int)] -> Bool
prop_deshacer ps =
  all (== unzip ps) [f ps | f <- [ deshacer 
                                 , deshacer2
                                 , deshacer3
                                 , deshacer4
                                 , deshacer5
                                 , deshacer6
                                 ]]

-- La comprobación es
--    λ> quickCheck prop_deshacer
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    nPlica :: Int -> [a] -> [a]
-- tal que (nPlica n xs) es la lista obtenida repitiendo cada elemento
-- de xs n veces. Por ejemplo, 
--    nPlica 3 [5,2,7,4]  ==  [5,5,5,2,2,2,7,7,7,4,4,4]
-- ---------------------------------------------------------------------

-- 1ª definición:
nPlica :: Int -> [a] -> [a]
nPlica k xs = concat [replicate k x | x <- xs]
 
-- 2ª definición:
nPlica2 :: Int -> [a] -> [a]
nPlica2 k xs = concat (map (replicate k) xs)
 
-- 3ª definición:
nPlica3 :: Int -> [a] -> [a]
nPlica3 k = concatMap (replicate k)
 
-- 4ª definición:
nPlica4 :: Int -> [a] -> [a]
nPlica4 k []     = []
nPlica4 k (x:xs) = replicate k x ++ nPlica4 k xs
 
-- 5ª definición:
nPlica5 :: Int -> [a] -> [a]
nPlica5 k = foldr ((++) . replicate k) []

-- 6ª definición:
nPlica6 :: Int -> [a] -> [a]
nPlica6 k = foldr f [] 
  where f prim recur = replicate k prim ++ recur

-- 7ª definición:
nPlica7 :: Int -> [a] -> [a]
nPlica7 k = aux [] 
  where aux ys []     = ys
        aux ys (x:xs) = aux (ys ++ replicate k x) xs

-- 8ª definición:
nPlica8 :: Int -> [a] -> [a]
nPlica8 k xs = foldl f [] xs
  where f ys prim = ys ++ replicate k prim

-- Equivalencia:
prop_nPlica :: Int -> [Int] -> Bool
prop_nPlica n xs =
  all (== nPlica n xs) [f n xs | f <- [ nPlica2
                                      , nPlica3
                                      , nPlica4
                                      , nPlica5
                                      , nPlica6
                                      , nPlica7
                                      , nPlica8
                                      ]]

-- La comprobación es
--    λ> quickCheck prop_nPlica
--    +++ OK, passed 100 tests.
