-- Informática (1º del Grado en Matemáticas, Grupo 5)
-- 1º examen de evaluación continua (30 de octubre de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    numDivisores :: Int -> Int -> [Int]
-- tal que (numDivisores n k) es la lista de los números enteros
-- positivos menores que n que tengan, al menos, k divisores. Por
-- ejemplo, 
--    numDivisores  100 11 == [60,72,84,90,96]
--    numDivisores  500 30 == []
--    numDivisores 1000 30 == [720,840]
-- --------------------------------------------------------------------

numDivisores :: Int -> Int -> [Int]
numDivisores n k = [x | x <- [1..n-1], length (divisores x) >= k]

-- (divisores n) es la lista de los divisores de n. Por ejemplo,
--    divisores 30  ==  [1,2,3,5,6,10,15,30]
divisores :: Int -> [Int]
divisores x = [y | y <- [1..x], rem x y == 0]

-- 2ª definición:
-- ==============

numDivisores2 :: Int -> Int -> [Int]
numDivisores2 n k = reverse (aux n)
  where aux 0 = []
        aux x | length (divisores x) >= k = x : aux (x-1)
              | otherwise                 = aux (x-1)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    posiciones :: [a] -> [Int] -> [a]
-- tal que (posiciones xs ns) es la lista de elementos de xs que
-- aparecen en las posiciones indicadas por la lista de enteros ns,
-- descartando valores de posiciones no válidas (negativos o más allá
-- del tamaño de la lista). Por ejemplo,
--    posiciones [2,7,9,31,5,0] [3,1,10,-2] == [31,7]
--    posiciones "zamora" [2,3,7,0,3]       == "mozo"
-- ---------------------------------------------------------------------

posiciones :: [a] -> [Int] -> [a]
posiciones xs ns = [xs!!k | k <- ns, 0 <= k, k < length xs]

-- ----------------------------------------------------------------------
-- Ejercicio 3.1. Un triángulo de lados (a,b,c) es válido si cualquiera
-- de sus lados es menor que la suma de los otros dos.
-- 
-- Definir la función
--    valido :: (Float,Float,Float) -> Bool
-- tal que (valido (a,b,c)) se verifica si el triángulo de lados (a,b,c)
-- es válido. Por ejemplo,
--    valido (3,5,4) == True
--    valido (1,4,7) == False
-- ---------------------------------------------------------------------

valido ::(Float,Float,Float) -> Bool
valido (a,b,c) = a < b+c && b < a+c && c < a+b

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. La fórmula de Herón establece que el área de un
-- triángulo de lados a,b,c, y semiperimetro s=(a+b+c)/2 es igual a la
-- raíz cuadrada de s(s-a)(s-b)(s-c).
--
-- Definir la función
--    area :: (Float,Float,Float) -> Float
-- tal que (area t) es el área del triángulo t (o bien -1 si el
-- triángulo no es válido). Por ejemplo, 
--    area (4,5,7)  == 9.797959
--    area (4,5,10) == -1.0
-- ---------------------------------------------------------------------

area :: (Float,Float,Float) -> Float
area (a,b,c)
  | valido (a,b,c) = sqrt (s*(s-a)*(s-b)*(s-c))
  | otherwise      = -1  
  where s = (a+b+c)/2

-- ------------------------------------------------------------------------
-- Ejercicio 4.1. Una variante del clásico juego "piedra, papel,
-- tijeras" se obtiene añadiendo al juego "lagarto y Spock". A
-- continuación, se asigna a cada objeto del juego un número y se
-- especifica cuándo gana cada uno. 
--   tijeras = 0 -- Tijeras cortan papel y decapitan lagarto
--   papel   = 1 -- Papel tapa a piedra y desautoriza a Spock
--   piedra  = 2 -- Aplasta a lagarto y a tijeras
--   lagarto = 3 -- Envenena a Spock y devora el papel
--   spock   = 4 -- Rompe tijeras y vaporiza piedra
-- 
-- Definir la función
--    tirada :: Int -> Int -> Int
-- tal que (tirada j1 j2) es el jugador (1 ó 2) gana la partida del
-- juego piedra, papel, tijeras, lagarto, Spock o 0, en caso de
-- empate. Por ejemplo,
--    tirada tijeras lagarto == 1
--    tirada piedra papel    == 2
--    tirada lagarto spock   == 1
--    tirada piedra piedra   == 0
-- -----------------------------------------------------------------

tijeras, papel, piedra, lagarto, spock :: Int
tijeras = 0 
papel   = 1 
piedra  = 2 
lagarto = 3 
spock   = 4 

-- 1ª definición
-- =============

tirada :: Int -> Int -> Int
tirada j1 j2
  | j1 == j2                                          = 0
  | j1 == tijeras && (j2 == papel   || j2 == lagarto) = 1
  | j1 == papel   && (j2 == piedra  || j2 == spock)   = 1
  | j1 == piedra  && (j2 == lagarto || j2 == tijeras) = 1
  | j1 == lagarto && (j2 == spock   || j2 == papel)   = 1
  | j1 == spock   && (j2 == tijeras || j2 == piedra)  = 1
  | otherwise                                         = 2

-- 2ª definición
-- =============

tirada2 :: Int -> Int -> Int
tirada2 j1 j2 
  | j1 == j2    = 0
  | gana1 j1 j2 = 1
  | otherwise   = 2

gana1 :: Int -> Int -> Bool
gana1 j1 j2 =
  (j1,j2) `elem` [ (tijeras, papel)
                 , (tijeras, lagarto)
                 , (papel,   piedra)
                 , (papel,   spock)
                 , (piedra,  lagarto)
                 , (piedra,  tijeras)
                 , (lagarto, spock)
                 , (lagarto, papel)
                 , (spock,   tijeras)
                 , (spock,   piedra)]

-- ------------------------------------------------------------------
-- Ejercicio 4.2. Una partida es una lista de pares (donde el primer y
-- el segundo elemento del par son las jugadas del primer y segundo
-- jugador, respectivamente).
-- 
-- Definir la función
--    ganador :: [(Int,Int)] -> Int
-- tal que (ganador js) es el jugador que gana la mayoría de las jugadas
-- de la lista js (o bien 0 si hay un empate). Por ejemplo, para la
-- partida definida por
--    partida :: [(Int,Int)]
--    partida = [(tijeras,papel),(papel,tijeras),(piedra,spock),
--               (lagarto,papel),(piedra,papel)]
-- se tiene
--    ganador partida        = 2
--    ganador (init partida) = 0
--    ganador (take 1 partida)  ==  1
-- ------------------------------------------------------------------------

partida :: [(Int,Int)]
partida = [(tijeras,papel),(papel,tijeras),(piedra,spock),
           (lagarto,papel),(piedra,papel)]

ganador :: [(Int,Int)] -> Int
ganador js
  | ganadasPor 1 > ganadasPor 2 = 1
  | ganadasPor 1 < ganadasPor 2 = 2
  | otherwise                   = 0  
  where ganadasPor j = length [(a,b) | (a,b) <- js, tirada a b == j]

