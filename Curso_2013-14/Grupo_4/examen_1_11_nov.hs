-- Informática (1º del Grado en Matemáticas y en Estadística)
-- 1º examen de evaluación continua (11 de noviembre de 2013)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función listaIgualParidad tal que al
-- evaluarla sobre una lista de números naturales devuelva la lista de
-- todos los elementos con la misma paridad que la posición que ocupan
-- (contada desde 0); es decir, todos los pares en una posición par y
-- todos los impares en una posición impar. Por ejemplo, 
--    listaIgualParidad [1,3,5,7]  ==  [3,7]
--    listaIgualParidad [2,4,6,8]  ==  [2,6]
--    listaIgualParidad [1..10]    ==  []
--    listaIgualParidad [0..10]    ==  [0,1,2,3,4,5,6,7,8,9,10]
--    listaIgualParidad []         ==  []
-- ---------------------------------------------------------------------

listaIgualParidad xs = [x | (x,i) <- zip xs [0..], even x == even i]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Decimos que una lista está equilibrada si el número de
-- elementos de la lista que son menores que la media es igual al número
-- de elementos de la lista que son mayores. 
-- 
-- Definir la función listaEquilibrada que comprueba dicha propiedad
-- para una lista. Por ejemplo, 
--    listaEquilibrada [1,7,1,6,2]  ==  False
--    listaEquilibrada [1,7,4,6,2]  ==  True
--    listaEquilibrada [8,7,4,6,2]  ==  False
--    listaEquilibrada []           ==  True
-----------------------------------------------------------------------

listaEquilibrada xs = 
    length [y | y <- xs, y < media xs] == 
    length [y | y <- xs, y > media xs]

-- (media xs) es la media de xs. Por ejemplo,
--    media [6, 3, 9]  ==  6.0
media xs = sum xs / fromIntegral (length xs)

-- ---------------------------------------------------------------------
-- Ejercicio 3. El trozo inicial de los elementos de una lista que
-- cumplen una propiedad es la secuencia de elementos de dicha lista
-- desde la posición 0 hasta el primer elemento que no cumple la
-- propiedad, sin incluirlo.  
-- 
-- Definirla función trozoInicialPares que devuelve el trozo inicial de
-- los elementos de una lista que son pares. Por ejemplo,
--    trozoInicialPares []         ==  []
--    trozoInicialPares [1,2,3,4]  ==  []
--    trozoInicialPares [2,4,3,2]  ==  [2,4]
--    trozoInicialPares [2,4,6,8]  ==  [2,4,6,8]
-- ---------------------------------------------------------------------

trozoInicialPares xs = take (posicionPrimerImpar xs) xs

-- (posicionPrimerImpar xs) es la posición del primer elemento impar de
-- la lista xs o su longitud si no hay ninguno. Por ejemplo,
--    posicionPrimerImpar [2,4,3,2]  ==  2
--    posicionPrimerImpar [2,4,6,2]  ==  4
posicionPrimerImpar xs = 
    head ([i | (x,i) <- zip xs [0..], odd x] ++ [length xs])

-- La función anterior se puede definir por recursión
posicionPrimerImpar2 [] = 0
posicionPrimerImpar2 (x:xs) 
    | odd x     = 0 
    | otherwise = 1 + posicionPrimerImpar2 xs

-- 2ª definición (por recursión).
trozoInicialPares2 [] = []
trozoInicialPares2 (x:xs) | odd x     = []
                          | otherwise = x : trozoInicialPares2 xs 

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. El registro de entradas vendidas de un cine se
-- almacena en una lista en la que cada elemento tiene el título de una
-- película, a continuación el número de entradas vendidas sin promoción
-- a 6 euros y por último el número de entradas vendidas con alguna de las
-- promociones del cine (menores de 4 años, mayores de 60, estudiantes
-- con carnet) a 4 euros. Por ejemplo,
--    entradas = [("Gravity",22,13),("Séptimo",18,6), ("Turbo",19,0),
--                ("Gravity",10,2), ("Séptimo",22,10),("Turbo",32,10),
--                ("Gravity",18,8), ("Séptimo",20,14),("Turbo",18,10)]
-- 
-- Definir la función ingresos tal que (ingresos bd) sea el total de
-- ingresos obtenidos según la información sobre entradas vendidas
-- almacenada en la lista 'bd'. Por ejemplo,
--    ingressos entradas  =  1366
-- ---------------------------------------------------------------------

entradas = [("Gravity",22,13),("Séptimo",18,6), ("Turbo",19,0),
            ("Gravity",10,2), ("Séptimo",22,10),("Turbo",32,10),
            ("Gravity",18,8), ("Séptimo",20,14),("Turbo",18,10)]

ingresos bd = sum [6*y+4*z | (_,y,z) <- bd]

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función ingresosPelicula tal que 
-- (ingresos bd p) sea el total de ingresos obtenidos en las distintas
-- sesiones de la película p según la información sobre entradas
-- vendidas almacenada en la lista bd. Por ejemplo,
--    ingresosPelicula entradas "Gravity"  ==  392
--    ingresosPelicula entradas "Séptimo"  ==  480
--    ingresosPelicula entradas "Turbo"    ==  494
-- ---------------------------------------------------------------------

ingresosPelicula bd p = sum [6*y+4*z | (x,y,z) <- bd, x == p]

-- ============================================================================
