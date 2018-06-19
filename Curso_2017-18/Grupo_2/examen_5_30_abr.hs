-- Informática (1º del Grado en Matemáticas) Grupo 2
-- 5º examen de evaluación continua (30 de abril de 2018)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Librerías auxiliares
-- ---------------------------------------------------------------------

import Data.List
import I1M.Grafo
import I1M.Pila
import I1M.PolOperaciones

-- ---------------------------------------------------------------------
-- Ejercicio 1. El número 545 es a la vez capicúa y suma de dos
-- cuadrados consecutivos: 545 = 16^2 + 17^2
-- 
-- Definir la lista
--    sucesion :: [Integer]
-- cuyos elementos son los números que son suma de cuadrados
-- consecutivos y capicúas. Por ejemplo, 
--    λ> take 10 sucesion
--    [1,5,181,313,545,1690961,3162613,3187813,5258525,5824285]
-- ---------------------------------------------------------------------

sucesion :: [Integer]
sucesion = filter capicua sucSumaCuadConsec

-- sucSumaCuadConsec es la sucesión de los números que son
-- suma de los cuadrados de dos números consecutivos. Por ejemplo,       
--    ghci> take 10 sucSumaCuadConsec
--    [1,5,13,25,41,61,85,113,145,181]
sucSumaCuadConsec :: [Integer]
sucSumaCuadConsec =
  [x^2 + (x+1)^2 | x <- [0..]]

-- (capicua n) se verifica si n es capicúa. Por ejemplo,
--    capicua 252    ==  True
--    capicua 2552   ==  True
--    capicua 25352  ==  True
--    capicua 25342  ==  False
capicua :: Integer -> Bool
capicua n = xs == reverse xs
  where xs = show n

-- ---------------------------------------------------------------------
-- Ejercicio 2. Consideremos las pilas ordenadas según el orden
-- lexicográfico. Es decir, la pila p1 es "menor" que p2 si la
-- cima de p1 es menor que la cima de p2 y, en caso de coincidir, la
-- pila que resulta de desapilar p1 es "menor" que la pila que resulta
-- de desapilar p2. 
-- 
-- Definir la función
--    esPilaMenor :: Ord a => Pila a -> Pila a -> Bool
-- tal que (esPilaMenor p1 p2) se verifica si p1 es "menor" que p2. Por
-- ejemplo, para la pilas  
--    p1 = foldr apila vacia [1..20]
--    p2 = foldr apila vacia [1..5]
--    p3 = foldr apila vacia [3..10]
--    p4 = foldr apila vacia [4,-1,7,3,8,10,0,3,3,4]
-- se verifica que
--    esPilaMenor p1 p2    == False
--    esPilaMenor p2 p1    == True
--    esPilaMenor p3 p4    == True
--    esPilaMenor vacia p1 == True
--    esPilaMenor p1 vacia == False
-- ---------------------------------------------------------------------

p1, p2, p3, p4 :: Pila Int 
p1 = foldr apila vacia [1..20]
p2 = foldr apila vacia [1..5]
p3 = foldr apila vacia [3..10]
p4 = foldr apila vacia [4,-1,7,3,8,10,0,3,3,4]

esPilaMenor :: Ord a => Pila a -> Pila a -> Bool
esPilaMenor p1 p2
  | esVacia p1 = True
  | esVacia p2 = False
  | a1 < a2    = True
  | a1 > a2    = False
  | otherwise  = esPilaMenor r1 r2
  where a1 = cima p1
        a2 = cima p2
        r1 = desapila p1
        r2 = desapila p2

-- ---------------------------------------------------------------------
-- Ejercicio 3. Dados dos polinomios P y Q, la suma en grados de P y Q
-- es el polinomio que resulta de conservar los términos de ambos
-- polinomios que coinciden en grado pero sumando sus coeficientes y
-- eliminar el resto. Por ejemplo, dados los polinomios
--    pol1  =  4*x^4 + 5*x^3 + 1
--    pol2  =  6*x^5 + 5*x^4 + 4*x^3 + 3*x^2 + 2*x + 1
-- la suma en grados de pol1 y pol2 será: 9*x^4 + 9*x^3 + 2
-- 
-- Definir la función 
--    sumaEnGrados :: Polinomio Int -> Polinomio Int -> Polinomio Int
-- tal que (sumaEnGrados p q) es la suma en grados de los polinomios
-- p y q. Por ejemplo, dados los polinomios
--    pol1  =  4*x^4 + 5*x^3 + 1
--    pol2  =  6*x^5 + 5*x^4 + 4*x^3 + 3*x^2 + 2*x + 1
--    pol3  =  -3*x^7 + 3*x^6 + -2*x^4 + 2*x^3 + -1*x + 1
--    pol4  =  -1*x^6 + 3*x^4 + -3*x^2 
-- se tendrá:
--    sumaEnGrados pol1 pol1  =>  8*x^4 + 10*x^3 + 2
--    sumaEnGrados pol1 pol2  =>  9*x^4 + 9*x^3 + 2
--    sumaEnGrados pol1 pol3  =>  2*x^4 + 7*x^3 + 2
--    sumaEnGrados pol3 pol4  =>  2*x^6 + x^4
-- ---------------------------------------------------------------------

listaApol :: [Int] -> Polinomio Int
listaApol xs = foldr (\ (n,c) p -> consPol n c p) 
                     polCero 
                     (zip [0..] xs)

pol1, pol2, pol3, pol4 :: Polinomio Int
pol1 = listaApol [1,0,0,5,4,0]
pol2 = listaApol [1,2,3,4,5,6]
pol3 = listaApol [1,-1,0,2,-2,0,3,-3]
pol4 = listaApol [0,0,-3,0,3,0,-1]

sumaEnGrados :: Polinomio Int -> Polinomio Int -> Polinomio Int
sumaEnGrados p q
  | esPolCero p = polCero
  | esPolCero q = polCero
  | gp < gq     = sumaEnGrados p rq
  | gq < gp     = sumaEnGrados rp q
  | otherwise   = consPol gp (cp+cq) (sumaEnGrados rp rq)
  where gp = grado p
        gq = grado q
        cp = coefLider p
        cq = coefLider q
        rp = restoPol p
        rq = restoPol q

-- ---------------------------------------------------------------------
-- Ejercicio 4. Un clique de un grafo no dirigido G es un conjunto de
-- vértices V tal que para todo par de vértices de V, existe una arista
-- en G que los conecta. Por ejemplo, en el grafo:
--    6 
--     \
--      4 ---- 5 
--      |      | \
--      |      |  1
--      |      | /
--      3 ---- 2   
-- el conjunto de vértices {1,2,5} es un clique y el conjunto {2,3,4,5}
-- no lo es.
-- 
-- En Haskell se puede representar el grafo anterior por
--    g1 :: Grafo Int Int
--    g1 = creaGrafo ND
--                   (1,6) 
--                   [(1,2,0),(1,5,0),(2,3,0),(3,4,0),(5,2,0),(4,5,0),(4,6,0)]
-- 
-- Definir la función
--    esClique :: Grafo Int Int -> [Int] -> Bool
-- tal que (esClique g xs) se verifica si xs es un clique de g. Por
-- ejemplo, 
--    esClique g1 [1,2,5]   == True
--    esClique g1 [2,3,4,5] == False
-- ---------------------------------------------------------------------

g1 :: Grafo Int Int
g1 = creaGrafo ND
               (1,6) 
               [(1,2,0),(1,5,0),(2,3,0),(3,4,0),(5,2,0),(4,5,0),(4,6,0)]

esClique :: Grafo Int Int -> [Int] -> Bool
esClique g xs = all (aristaEn g) [(x,y) | x <- ys, y <- ys, y < x]
  where ys = sort xs


