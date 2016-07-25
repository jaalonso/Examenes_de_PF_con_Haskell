-- Informática (1º del Grado en Matemáticas)
-- 1º examen de evaluación continua (3 de noviembre de 2014)   
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. El módulo de un vector de n dimensiones, 
-- xs = (x1,x2,...,xn), es la raíz cuadrada de la suma de los cuadrados
-- de sus componentes. Por ejemplo,
--    el módulo de (1,2)   es 2.236068
--    el módulo de (1,2,3) es 3.7416575
--
-- El normalizado de un vector de n dimensiones, xs = (x1,x2,...,xn), es
-- el vector que se obtiene dividiendo cada componente por su módulo. De
-- esta forma, el módulo del normalizado de un vector siempre es 1. Por
-- ejemplo, 
--    el normalizado de (1,2)   es (0.4472136,0.8944272)
--    el normalizado de (1,2,3) es (0.26726124,0.5345225,0.8017837)
-- 
-- Definir, por comprensión, la función 
--    modulo :: [Float] -> Float
-- tal que (modulo xs) es el módulo del vector xs. Por ejemplo,
--    modulo [1,2]      ==  2.236068
--    modulo [1,2,3]    ==  3.7416575
--    modulo [1,2,3,4]  ==  5.477226
-- ---------------------------------------------------------------------

modulo :: [Float] -> Float
modulo xs = sqrt (sum [x^2 | x <- xs])

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, por comprensión, la función
--    normalizado :: [Float] -> [Float]
-- tal que (normalizado xs) es el vector resultado de normalizar el
-- vector xs. Por ejemplo,
--    normalizado [1,2]      ==  [0.4472136,0.8944272]
--    normalizado [1,2,3]    ==  [0.26726124,0.5345225,0.8017837]
--    normalizado [1,2,3,4]  ==  [0.18257418,0.36514837,0.5477225,0.73029673]
-- ---------------------------------------------------------------------

normalizado :: [Float] -> [Float]
normalizado xs = [x / modulo xs | x <- xs]

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. En un bloque de pisos viven "Ana", "Beatriz", "Carlos"
-- y "Daniel", cada uno de ellos tiene ciertos alimentos en sus
-- respectivas despensas. Esta información está almacenada en una lista
-- de asociación de la siguiente forma: (<nombre>,<despensa>)
--    datos = [("Ana",["Leche","Huevos","Sal"]),
--             ("Beatriz",["Jamon","Lechuga"]),
--             ("Carlos",["Atun","Tomate","Jamon"]),
--             ("Daniel",["Salmon","Huevos"])]
-- 
-- Definir, por comprensión, la función
--    tienenProducto :: String -> [(String,[String])] -> [String]
-- tal que (tienenProducto x ys) es la lista de las personas que tienen el
-- producto x en sus despensas. Por ejemplo,
--    tienenProducto "Lechuga" datos  ==  ["Beatriz"]
--    tienenProducto "Huevos"  datos  ==  ["Ana","Daniel"]
--    tienenProducto "Pan"     datos  ==  []
-- ---------------------------------------------------------------------

datos = [("Ana",["Leche","Huevos","Sal"]),
         ("Beatriz",["Jamon","Lechuga"]),
         ("Carlos",["Atun","Tomate","Jamon"]),
         ("Daniel",["Salmon","Huevos"])]

tienenProducto :: String -> [(String,[String])] -> [String]
tienenProducto x ys = [z | (z,ds) <- ys, x `elem` ds]

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, por comprensión, la función
--    proveedores :: String -> [(String,[String])] -> [String]
-- tal que (proveedores xs ys) es la lista de las personas que pueden
-- proporcionar algún producto de los de la lista xs. Por ejemplo, 
--    proveedores ["Leche","Jamon"] datos == ["Ana","Beatriz","Carlos"]
--    proveedores ["Sal","Atun"]    datos == ["Ana","Carlos"]
--    proveedores ["Leche","Sal"]   datos == ["Ana"]
-- ---------------------------------------------------------------------

proveedores :: [String] -> [(String,[String])] -> [String]
proveedores xs ys =
    [z | (z,ds) <- ys, not (null [d | d <- ds, d `elem` xs])]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir, por recursión, la función
--    intercalaNumeros :: Integer -> Integer -> Integer
-- tal que (intercalaNumeros n m) es el número resultante de
-- "intercalar" las cifras de los números 'n' y 'm'. Por ejemplo, el
-- resultado de intercalar las cifras de los números 123 y 768 sería:
--      1 2 3
--       7 6 8
--    ----------
--      172638
-- Si uno de los dos números tiene más cifras que el otro, simplemente
-- se ponen todas al principio en el mismo orden. Por ejemplo, el
-- resultado de intercalar las cifras de los números 1234 y 56 sería:
--      1 2 3 4
--           5 6
--    ------------
--      1 2 3546
-- De esta forma:
--    intercalaNumeros 123 768  ==  172638
--    intercalaNumeros 1234 56  ==  123546
--    intercalaNumeros 56 1234  ==  125364
-- ---------------------------------------------------------------------

-- 1ª definición:
intercalaNumeros :: Integer -> Integer -> Integer
intercalaNumeros 0 y = y
intercalaNumeros x 0 = x
intercalaNumeros x y =
    let rx = mod x 10
        dx = div x 10
        ry = mod y 10
        dy = div y 10
    in (intercalaNumeros dx dy)*100 + rx*10 + ry

-- 2ª definición:
intercalaNumeros2 :: Integer -> Integer -> Integer
intercalaNumeros2 0 y = y
intercalaNumeros2 x 0 = x
intercalaNumeros2 x y = 100 * intercalaNumeros2 dx dy + 10*rx + ry
    where (dx,rx) = divMod x 10
          (dy,ry) = divMod y 10

-- ---------------------------------------------------------------------
-- Ejercicio 4. La carga de una lista es el número de elementos
-- estrictamente positivos menos el número de elementos estrictamente
-- negativos. 
-- 
-- Definir, por recursión, la función  
--    carga :: [Integer] -> Integer
-- tal que (carga xs) es la carga de la lista xs. Por ejemplo,
--    carga [1,2,0,-1]     ==  1
--    carga [1,0,2,0,3]    ==  3
--    carga [1,0,-2,0,3]   ==  1
--    carga [1,0,-2,0,-3]  ==  -1
--    carga [1,0,-2,2,-3]  ==  0
-- ---------------------------------------------------------------------

carga :: [Integer] -> Integer
carga [] = 0
carga (x:xs)
    | x > 0     = 1 + carga xs
    | x == 0    = carga xs
    | otherwise = carga xs - 1


