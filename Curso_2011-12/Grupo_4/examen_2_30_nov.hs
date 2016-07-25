-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 2º examen de evaluación continua (30 de noviembre de 2011)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Un número es tipo repunit si todos sus dígitos son
-- 1. Por ejemplo, el número 1111 es repunit.
--
-- Definir la función 
--    menorRepunit :: Integer -> Integer
-- tal que (menorRepunit n) es el menor repunit que es múltiplo de
-- n. Por ejemplo, 
--    menorRepunit 3  == 111
--    menorRepunit 7  == 111111
-- ---------------------------------------------------------------------

menorRepunit :: Integer -> Integer
menorRepunit n = head [x | x <- [n,n*2..], repunit x]

-- (repunit n) se verifica si n es un repunit. Por ejemplo,
--    repunit 1111  ==  True
--    repunit 1121  ==  False
repunit :: Integer -> Bool
repunit n = and [x == 1 | x <- cifras n]

-- (cifras n) es la lista de las cifras de n. Por ejemplo,
--    cifras 325  ==  [3,2,5]
cifras :: Integer -> [Integer]
cifras n = [read [d] | d <- show n]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función 
--    maximos :: [Float -> Float] -> [Float] -> [Float]
-- tal que (maximos fs xs) es la lista de los máximos de aplicar
-- cada función de fs a los elementos de xs. Por ejemplo,
--    maximos [(/2),(2/)] [5,10]                 == [5.0,0.4]
--    maximos [(^2),(/2),abs,(1/)] [1,-2,3,-4,5] == [25.0,2.5,5.0,1.0]
-- ---------------------------------------------------------------------

-- 1ª definición:
maximos :: [Float -> Float] -> [Float] -> [Float]
maximos fs xs = [maximum [f x | x <- xs] | f <- fs]

-- 2ª definición:
maximos2 :: [Float -> Float] -> [Float] -> [Float]
maximos2 fs xs = map maximum [ map f xs | f <- fs]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función 
--    reduceCifras :: Integer -> Integer
-- tal que (reduceCifras n) es el resultado de la reducción recursiva de
-- sus cifras; es decir, a partir del número n, se debe calcular la suma
-- de las cifras de n (llamémosle c), pero si c es a su vez mayor que 9,
-- se debe volver a calcular la suma de cifras de c y así sucesivamente
-- hasta que el valor obtenido sea menor o igual que 9. Por ejemplo,
--    reduceCifras   5   ==  5
--    reduceCifras 123   ==  6 
--    reduceCifras 190   ==  1
--    reduceCifras 3456  ==  9
-- ---------------------------------------------------------------------

reduceCifras :: Integer -> Integer
reduceCifras n | m <= 9    = m
               | otherwise = reduceCifras m
               where m = sum (cifras n)

-- (sumaCifras n) es la suma de las cifras de n. Por ejemplo,
--    sumaCifras 3456  ==  18
sumaCifras :: Integer -> Integer 
sumaCifras n = sum (cifras n)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Las bases de datos con el nombre, profesión, año de
-- nacimiento y año de defunción de una serie de personas se puede
-- representar como sigue 
--    personas :: [(String,String,Int,Int)]
--    personas = [("Cervantes","Literatura",1547,1616),
--                ("Velazquez","Pintura",1599,1660),
--                ("Picasso","Pintura",1881,1973),
--                ("Beethoven","Musica",1770,1823),
--                ("Poincare","Ciencia",1854,1912),
--                ("Quevedo","Literatura",1580,1654),
--                ("Goya","Pintura",1746,1828),
--                ("Einstein","Ciencia",1879,1955),
--                ("Mozart","Musica",1756,1791),
--                ("Botticelli","Pintura",1445,1510),
--                ("Borromini","Arquitectura",1599,1667),
--                ("Bach","Musica",1685,1750)]
-- Es decir, se indica que por ejemplo Mozart se dedicó a la Música y
-- vivió entre 1756 y 1791. 
-- 
-- Definir la función 
--    coetaneos :: [(String,String,Int,Int)] -> String -> [String]
-- tal que (coetaneos bd p) es la lista de nombres de personas que
-- fueron coetáneos con la persona p; es decir que al menos alguno
-- de los años vividos por ambos coincidan. Se considera que una persona
-- no es coetanea a sí misma. Por ejemplo,
--    coetaneos personas "Einstein"   == ["Picasso", "Poincare"]
--    coetaneos personas "Botticelli" == []
-- ---------------------------------------------------------------------

personas :: [(String,String,Int,Int)]
personas = [("Cervantes","Literatura",1547,1616),
              ("Velazquez","Pintura",1599,1660),
              ("Picasso","Pintura",1881,1973),
              ("Beethoven","Musica",1770,1823),
              ("Poincare","Ciencia",1854,1912),
              ("Quevedo","Literatura",1580,1654),
              ("Goya","Pintura",1746,1828),
              ("Einstein","Ciencia",1879,1955),
              ("Mozart","Musica",1756,1791),
              ("Botticelli","Pintura",1445,1510),
              ("Borromini","Arquitectura",1599,1667),
              ("Bach","Musica",1685,1750)]

-- 1ª solución:
coetaneos :: [(String,String,Int,Int)] -> String -> [String]
coetaneos bd p = 
    [n | (n,_,fn,fd) <- bd, 
         n /= p, 
         not ((fd < fnp) || (fdp < fn))]
    where (fnp,fdp) = head [(fn,fd) | (n,_,fn,fd) <- bd, n == p]

-- 2ª solución:

coetaneos2 :: [(String,String,Int,Int)] -> String -> [String]
coetaneos2 bd p = 
    [n | (n,_,fn,fd) <- bd, 
         n /= p,
         not (null (inter [fn..fd] [fnp..fdp]))]
    where (fnp,fdp) = head [(fn,fd) | (n,_,fn,fd) <- bd, n == p]

-- (inter xs ys) es la interseción de xs e ys. Por ejemplo,
--    inter [2,5,3,6] [3,7,2]  ==  [2,3]
inter :: Eq a => [a] -> [a] -> [a]
inter xs ys = [x | x <- xs, x `elem` ys]


