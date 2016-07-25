-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 1º examen de evaluación continua (7 de noviembre de 2011)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    conjuntosIguales :: Eq a => [a] -> [a] -> Bool
-- tal que (conjuntosIguales xs ys) se verifica si xs e ys contienen los
-- mismos elementos independientemente del orden y posibles
-- repeticiones. Por ejemplo,
--    conjuntosIguales [1,2,3] [2,3,1]               == True
--    conjuntosIguales "arroz" "zorra"               == True
--    conjuntosIguales [1,2,2,3,2,1] [1,3,3,2,1,3,2] == True
--    conjuntosIguales [1,2,2,1] [1,2,3,2,1]         == False
--    conjuntosIguales [(1,2)] [(2,1)]               == False
-- ---------------------------------------------------------------------

conjuntosIguales :: Eq a => [a] -> [a] -> Bool
conjuntosIguales xs ys = 
    and [x `elem` ys | x <- xs] && and [y `elem` xs | y <- ys]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    puntoInterior :: (Float,Float) -> Float -> (Float,Float) -> Bool
-- tal que (puntoInterior c r p) se verifica si p es un punto interior
-- del círculo de centro c y radio r. Por ejemplo,
--    puntoInterior (0,0) 1 (1,0)   == True
--    puntoInterior (0,0) 1 (1,1)   == False
--    puntoInterior (0,0) 2 (-1,-1) == True
-- ---------------------------------------------------------------------

puntoInterior :: (Float,Float) -> Float -> (Float,Float) -> Bool
puntoInterior (cx,cy) r (px,py) = distancia (cx,cy) (px,py) <= r

-- (distancia p1 p2) es la distancia del punto p1 al p2. Por ejemplo,
--    distancia (0,0) (3,4)  ==  5.0
distancia :: (Float,Float) -> (Float,Float) -> Float
distancia (x1,y1) (x2,y2) = sqrt ( (x2-x1)^2 + (y2-y1)^2)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función 
--    tripletes :: Int -> [(Int,Int,Int)]
-- tal que (tripletes n) es la lista de tripletes (tuplas de tres
-- elementos) con todas las combinaciones posibles de valores numéricos 
-- entre 1 y n en cada posición del triplete, pero de forma que no haya
-- ningún valor repetido dentro de cada triplete. Por ejemplo,
--    tripletes 3 == [(1,2,3),(1,3,2),(2,1,3),(2,3,1),(3,1,2),(3,2,1)]
--    tripletes 4 == [(1,2,3),(1,2,4),(1,3,2),(1,3,4),(1,4,2),(1,4,3),
--                    (2,1,3),(2,1,4),(2,3,1),(2,3,4),(2,4,1),(2,4,3),
--                    (3,1,2),(3,1,4),(3,2,1),(3,2,4),(3,4,1),(3,4,2),
--                    (4,1,2),(4,1,3),(4,2,1),(4,2,3),(4,3,1),(4,3,2)]
--    tripletes 2 == []
-- ---------------------------------------------------------------------

tripletes :: Int -> [(Int,Int,Int)]
tripletes n = [(x,y,z) | x <- [1..n], 
                         y <- [1..n], 
                         z <- [1..n],
                         x /= y,
                         x /= z,
                         y /= z]

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Las bases de datos de alumnos matriculados por
-- provincia y por especialidad se pueden representar como sigue 
--    matriculas :: [(String,String,Int)]
--    matriculas = [("Almeria","Matematicas",27),
--                  ("Sevilla","Informatica",325),
--                  ("Granada","Informatica",296),
--                  ("Huelva","Matematicas",41),
--                  ("Sevilla","Matematicas",122),
--                  ("Granada","Matematicas",131),
--                  ("Malaga","Informatica",314)]
-- Es decir, se indica que por ejemplo en Almería hay 27 alumnos
-- matriculados en Matemáticas. 
-- 
-- Definir la función 
--    totalAlumnos :: [(String,String,Int)] -> Int
-- tal que (totalAlumnos bd) es el total de alumnos matriculados,
-- incluyendo todas las provincias y todas las especialidades, en la
-- base de datos bd. Por ejemplo,
--    totalAlumnos matriculas == 1256
-- ---------------------------------------------------------------------

matriculas :: [(String,String,Int)]
matriculas = [("Almeria","Matematicas",27),
              ("Sevilla","Informatica",325),
              ("Granada","Informatica",296),
              ("Huelva","Matematicas",41),
              ("Sevilla","Matematicas",122),
              ("Granada","Matematicas",131),
              ("Malaga","Informatica",314)]

totalAlumnos :: [(String,String,Int)] -> Int
totalAlumnos bd = sum [ n | (_,_,n) <- bd]

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir la función 
--    totalMateria :: [(String,String,Int)] -> String -> Int
-- tal que (totalMateria bd m) es el número de alumnos de la base de
-- datos bd matriculados en la materia m. Por ejemplo, 
--    totalMateria matriculas "Informatica" == 935
--    totalMateria matriculas "Matematicas" == 321
--    totalMateria matriculas "Fisica"      == 0
-- ---------------------------------------------------------------------

totalMateria :: [(String,String,Int)] -> String -> Int
totalMateria bd m = sum [ n | (_,m',n) <- bd, m == m']
