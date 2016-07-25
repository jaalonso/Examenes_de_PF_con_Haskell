-- Informática (1º del Grado en Matemáticas)
-- 1º examen de evaluación continua (7 de noviembre de 2013)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1 [Del problema 21 del Proyecto Euler]. Sea d(n) la suma de
-- los divisores propios de n. Si d(a) = b y d(b) = a, siendo a ≠ b,
-- decimos que a y b son un par de números amigos. Por ejemplo, los
-- divisores propios de 220 son 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 y
-- 110; por tanto, d(220) = 284. Los divisores propios de 284 son 1, 2,
-- 4, 71 y 142; por tanto,  d(284) = 220.  Luego, 220 y 284 son dos
-- números amigos. 
-- 
-- Definir la función amigos tal que (amigos a b) se verifica si a y b
-- son números amigos. Por ejemplo,
--    amigos 6 6       == False
--    amigos 220 248   == False
--    amigos 220 284   == True
--    amigos 100 200   == False
--    amigos 1184 1210 == True
-- ---------------------------------------------------------------------

amigos a b = sumaDivisores a == b && sumaDivisores b == a
    where sumaDivisores n = sum [x | x<-[1..n-1], n `rem` x == 0]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Una representación de 20 en base 2 es [0,0,1,0,1] pues
-- 20 = 1*2^2 + 1*2^4. Y una representación de 46 en base 3 es [1,0,2,1]
-- pues 46 = 1*3^0 + 0*3^1 + 2*3^2 + 1*3^3.
-- 
-- Definir la función deBaseABase10 tal que (deBaseABase10 b xs) es el
-- número n tal que su representación en base b es xs. Por ejemplo,
--    deBaseABase10 2 [0,0,1,0,1]      == 20
--    deBaseABase10 2 [1,1,0,1]        == 11
--    deBaseABase10 3 [1,0,2,1]        == 46
--    deBaseABase10 5 [0,2,1,3,1,4,1]  == 29160
-- ---------------------------------------------------------------------

deBaseABase10 b xs = sum [y*b^n | (y,n) <- zip xs [0..]]

-- ---------------------------------------------------------------------
-- Ejercicio 3. [De la IMO-1996-S-21]. Una sucesión [a(0),a(1),...,a(n)] 
-- se denomina cuadrática si para cada i ∈ {1, 2,..., n} se cumple que 
--    |a(i) − a(i−1)| = i^2 .
-- Definir una función esCuadratica tal que (esCuadratica xs) se
-- verifica si xs cuadrática.  Por ejemplo,
--    esCuadratica [2,1,-3,6]                      == True
--    esCuadratica [2,1,3,5]                       == False
--    esCuadratica [3,4,8,17,33,58,94,45,-19,-100] == True
-- ---------------------------------------------------------------------

esCuadratica xs = 
    and [abs (y-x) == i^2 | ((x,y),i) <- zip (adyacentes xs) [1..]]

adyacentes xs = zip xs (tail xs)

-- -------------------------------------------------------------
-- Ejercicio 4.1. Sea t una lista de pares de la forma 
--    (nombre, [(asig1, nota1),...,(asigk, notak)])
-- Por ejemplo, 
--    t1 = [("Ana",[("Algebra",1),("Calculo",3),("Informatica",8),("Fisica",2)]),
--          ("Juan",[("Algebra",5),("Calculo",1),("Informatica",2),("Fisica",9)]),
--          ("Alba",[("Algebra",5),("Calculo",6),("Informatica",6),("Fisica",5)]),
--          ("Pedro",[("Algebra",9),("Calculo",5),("Informatica",3),("Fisica",1)])]
-- Definir la función calificaciones tal que (calificaciones t p) es la
-- lista de las calificaciones de la persona p en la lista t. Por
-- ejemplo, 
--    ghci> calificaciones t1 "Pedro"
--    [("Algebra",9),("Calculo",5),("Informatica",3),("Fisica",1)]
-- ---------------------------------------------------------------------

t1 = [("Ana",[("Algebra",1),("Calculo",3),("Informatica",8),("Fisica",2)]),
      ("Juan",[("Algebra",5),("Calculo",1),("Informatica",2),("Fisica",9)]),
      ("Alba",[("Algebra",5),("Calculo",6),("Informatica",6),("Fisica",5)]),
      ("Pedro",[("Algebra",9),("Calculo",5),("Informatica",3),("Fisica",1)])]

calificaciones t p = head [xs | (x,xs) <-t, x == p]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función todasAprobadas tal que 
-- (todasAprobadas t p) se cumple si en la lista t, p tiene todas las
-- asignaturas aprobadas. Por ejemplo,
--    todasAprobadas t1 "Alba"  ==  True
--    todasAprobadas t1 "Pedro" ==  False
-- ---------------------------------------------------------------------

todasAprobadas t p = numeroAprobados t p == numeroAsignaturas t p
    
numeroAprobados t p = length [n | (_,n) <- calificaciones t p, n >= 5]

numeroAsignaturas t p = length (calificaciones t p)

apruebanTodo t = [p | (p,_) <- t, todasAprobadas t p]
