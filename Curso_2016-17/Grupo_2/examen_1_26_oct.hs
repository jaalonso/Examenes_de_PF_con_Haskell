-- Informática (1º del Grado en Matemáticas, Grupo 2)
-- 1º examen de evaluación continua (26 de octubre de 2015)
-- ---------------------------------------------------------------------

-----------------------------------------------------------------------
-- Ejercicio 1. Consideremos las siguientes regiones del plano: 
--    R1: Puntos (x,y) tales que |x| + |y| <= 1    (rombo unidad)
--    R2: Puntos (x,y) tales que x^2 + y^2 <= 1    (círculo unidad)
--    R3: Puntos (x,y) tales que max(|x|,|y|) <= 1 (cuadrado unidad)
-- donde |x| es el valor absoluto del número x.
--
-- Se cumple que la región R1 está contenida en la región R2 y la región
-- R2 está contenida en la región R3, pero ninguna de ellas son iguales.  
--
-- Definir la función 
--    numeroRegiones :: (Float,Float) -> Int
-- tal que (numeroRegiones p) es el número de regiones R1, R2 y R3 en
-- las que está contenido el punto p. Por ejemplo,
--    numeroRegiones (0.2,0.3)    ==  3
--    numeroRegiones (-0.5,0.6)   ==  2
--    numeroRegiones (0.8,-0.8)   ==  1
--    numeroRegiones (-0.9,-1.2)  ==  0
-- ---------------------------------------------------------------------

-- 1ª solución
numeroRegiones :: (Float,Float) -> Int
numeroRegiones (x,y) 
  | abs x + abs y <= 1        =  3
  | x^2 + y^2 <= 1            =  2
  | max (abs x) (abs y) <= 1  =  1
  | otherwise                 =  0

-- 2ª solución
numeroRegiones2 :: (Float,Float) -> Int
numeroRegiones2 (x,y) =
  sum [1 | c <- [ abs x + abs y <= 1
                , x^2 + y^2 <= 1
                , max (abs x) (abs y) <= 1],
           c]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Dos números A y B son coprimos si no tienen ningún
-- factor primo común. Por ejemplo los números 15 y 49 son coprimos, ya
-- que los factores primos de 15 son 3 y 5; y el único factor primo de
-- 49 es 7. Por otro lado, los números 15 y 35 no son coprimos, ya que
-- ambos son divisibles por 5.
--
-- Definir por comprensión la función
--    primerParCoprimos :: [Integer] -> (Integer,Integer)
-- tal que (primerParCoprimos xs) es el primer par de números
-- consecutivos en la lista xs que son coprimos entre sí. Si en la
-- lista no hay números coprimos consecutivos el resultado debe ser
-- (0,0). Por ejemplo, 
--    primerParCoprimos [3,9,13,26]   ==  (9,13)
--    primerParCoprimos [3,6,1,7,14]  ==  (6,1)
--    primerParCoprimos [3,6,9,12]    ==  (0,0)
-- ---------------------------------------------------------------------

primerParCoprimos :: [Integer] -> (Integer,Integer)
primerParCoprimos xs =
  head ([(x,y) | (x,y) <- zip xs (tail xs), gcd x y == 1] ++ [(0,0)])

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Una forma de aproximar el logaritmo de 2 es usando la
-- siguiente igualdad: 
--
--                      1     1     1     1     
--          ln 2 = 1 - --- + --- - --- + --- - ....
--                      2     3     4     5
--
-- Es decir, la serie cuyo término general n-ésimo es el cociente entre
-- (-1)^(n+1) y el propio número n:
--
--                    (-1)^(n+1)
--           s(n) =  ------------
--                         n
--
-- Definir por comprensión la función:
--    aproximaLn2C :: Double -> Double
-- tal que (aproximaLn2C n) es la aproximación del logaritmo de 2
-- calculada con la serie anterior hasta el término n-ésimo. Por
-- ejemplo, 
--    aproximaLn2C 10  ==  0.6456349206349207
--    aproximaLn2C 30  ==  0.6687714031754279
--    aproximaLn2C 50  ==  0.6767581376913979
-- ---------------------------------------------------------------------

aproximaLn2C :: Double -> Double
aproximaLn2C n =
  sum [(-1)**(i+1) / i | i <- [1..n] ]

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir por recursión la función:
--    aproximaLn2R :: Double -> Double
-- tal que (aproximaLn2R n) es la aproximación del logaritmo de 2
-- calculada con la serie anterior hasta el término n-ésimo. Por
-- ejemplo, 
--    aproximaLn2R 10  ==  0.6456349206349207
--    aproximaLn2R 30  ==  0.6687714031754279
--    aproximaLn2R 50  ==  0.6767581376913979
-- ---------------------------------------------------------------------

aproximaLn2R :: Double -> Double
aproximaLn2R 1 = 1
aproximaLn2R n = ((-1)**(n+1) / n) + aproximaLn2R (n-1)
    
-- ---------------------------------------------------------------------
-- Ejercicio 4. Dado un número natural cualquiera, N, podemos formar
-- otros dos números, uno a partir de las cifras pares de N y otro a
-- partir de las cifras impares de N, a los que llamaremos
-- respectivamente componente par de N y componente impar de N. Por
-- ejemplo, la componente par del número 1235678 es 268 y la componente
-- impar es 1357. 
-- 
-- Definir por recursión la función 
--    componentePar :: Integer -> Integer
-- tal que (componentePar n) es la componente par del número natural
-- n. En el caso en que n no tenga cifras pares el resultado debe ser
-- 0. Por ejemplo,
--    componentePar 1235678  ==  268
--    componentePar 268      ==  268
--    componentePar 375      ==  0
-- ---------------------------------------------------------------------

componentePar :: Integer -> Integer
componentePar n
  | null pares = 0
  | otherwise  = read pares
  where pares = [c | c <- show n
                   , c `elem` "02468"]
