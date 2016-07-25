-- Informática (1º del Grado en Matemáticas)
-- 4º examen de evaluación continua (24 de marzo de 2014)
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List (nub, sort)

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Los polinomios se pueden representar mediante el
-- siguiente tipo algebraico
--    data Polinomio = Indep Int | Monomio Int Int Polinomio 
--                     deriving (Eq, Show)
-- Por ejemplo, el polinomio 3x^2+2x-5 se representa por
--    ej1 = Monomio 3 2 (Monomio 2 1 (Indep (-5)))
-- y el polinomio 4x^3-2x por
--    ej2 = Monomio 4 3 (Monomio (-2) 1 (Indep 0))
-- Observa que si un monomio no aparece en el polinomio, en su
-- representación tampoco aparece; es decir, el coeficiente de un
-- monomio en la representación no debe ser cero.
-- 
-- Definir la función  
--    sPol :: Polinomio -> Polinomio -> Polinomio
-- tal que (sPol p q) es la suma de p y q. Por ejemplo,
--    sPol ej1 ej2 == Monomio 4 3 (Monomio 3 2 (Indep (-5)))
--    sPol ej1 ej1 == Monomio 6 2 (Monomio 4 1 (Indep (-10)))
-- ---------------------------------------------------------------------

data Polinomio = Indep Int | Monomio Int Int Polinomio 
                 deriving (Eq, Show)

ej1 = Monomio 3 2 (Monomio 2 1 (Indep (-5)))
ej2 = Monomio 4 3 (Monomio (-2) 1 (Indep 0))

sPol :: Polinomio -> Polinomio -> Polinomio
sPol (Indep 0) q = q
sPol p (Indep 0) = p
sPol (Indep n) (Indep m) = Indep (m+n)
sPol (Indep n) (Monomio c g p)= Monomio c g (sPol p (Indep n))
sPol (Monomio c g p) (Indep n) = Monomio c g (sPol p (Indep n))
sPol p1@(Monomio c1 g1 r1) p2@(Monomio c2 g2 r2) 
    | g1 >  g2   = Monomio c1 g1 (sPol r1 p2)
    | g1 <  g2   = Monomio c2 g2 (sPol p1 r2)
    | c1+c2 /= 0 = Monomio (c1+c2) g1 (sPol r1 r2)
    | otherwise  = sPol r1 r2

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Los polinomios también se pueden representar mediante
-- la lista de sus coeficientes. Por ejemplo, el polinomio ej1 se
-- representa por [3,2,-5] y el polinomio ej2 vendrá por [4,0,-2,0].
-- 
-- Definir la función  
--    cambia :: Polinomio -> [Int]
-- tal que (cambia p) es la lista de los coeficientes de p. Por ejemplo,
--    cambia ej1 == [3,2,-5]
--    cambia ej2 == [4,0,-2,0]
-- ---------------------------------------------------------------------

cambia :: Polinomio -> [Int]
cambia (Indep n) = [n]
cambia (Monomio c g (Indep n)) = (c:(replicate 0 (g-1)))++[n]
cambia (Monomio c1 g1 (Monomio c2 g2 p)) = 
    (c1:(replicate  (g1-g2-1) 0)) ++ cambia (Monomio c2 g2 p)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Los árboles binarios se pueden representar mediante el
-- siguiente tipo de datos
--    data Arbol a = H a
--                 | N a (Arbol a) (Arbol a)
--                 deriving (Show, Eq)
-- 
-- Definir la función 
--    profundidades :: (Num t, Eq t) => Arbol t -> t -> [t]
-- tal que (profundidades a x) es la lista de las profundidades que ocupa x
-- en el árbol a. Por ejemplo,
--    profundidades (N 1 (H 1) (N 3 (H 1) (H 2))) 1  ==  [1,2,3]
--    profundidades (N 1 (H 1) (N 3 (H 1) (H 2))) 2  ==  [3]
--    profundidades (N 1 (H 1) (N 3 (H 1) (H 2))) 3  ==  [2]
--    profundidades (N 1 (H 1) (N 3 (H 1) (H 2))) 4  ==  []
-- ---------------------------------------------------------------------

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
             deriving (Show, Eq)

profundidades :: (Num t, Eq t) => Arbol t -> t -> [t]
profundidades (H y) x | x == y    = [1]
                      | otherwise = []
profundidades (N y i d) x 
    | x == y    = 1:[n+1 | n <- profundidades i x ++  profundidades d x]
    | otherwise =   [n+1 | n <- profundidades i x ++  profundidades d x]

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. La sucesión de Phill esta definida por
--    x_0 = 2
--    x_1 = 7
--    x_3 = 2*x_(n-1) - x_(n-2), si n > 1.
-- 
-- Definir, por recursión, la función
--    phill :: Integer -> Integer
-- tal que (phill n) es el n-ésimo término de la sucesión de Phill. Por
-- ejemplo, 
--    phill 8  ==  42
-- ---------------------------------------------------------------------

phill :: Integer -> Integer
phill 0 = 2
phill 1 = 7
phill n = 2*(phill (n-1)) - phill (n-2)

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir, por comprensión, la función
--    phills :: [Integer]
-- tal que phills es la sucesión de Phill. Por ejemplo,
--    take 8 phills  ==  [2,7,12,17,22,27,32,37]
-- ---------------------------------------------------------------------

phills :: [Integer]
phills = [phill n | n <- [0..]]

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir, por recursión (evaluación perezosa) la
-- función 
--    phills1 :: [Integer]
-- tal que phills1 es la sucesion de Phill. Por ejemplo,
--    take 8 phills1  ==  [2,7,12,17,22,27,32,37]
-- Nota: Dar dos definiciones, una sin usar zipWith o otra usándola. 
-- ---------------------------------------------------------------------

-- Sin zipWith:
phills1 :: [Integer]
phills1  = aux 2 7
    where aux x y = x : aux y (2*y-x)

-- Con zipWith:
phills2 :: [Integer]
phills2 = 2:7:zipWith f phills2 (tail phills2)
    where f x y = 2*y-x       

-- ---------------------------------------------------------------------
-- Ejercicio 3.4. Definir la función 
--    unidades :: [Integer] 
-- tal que unidades es la lista de los últimos dígitos de cada término de
-- la sucesión de Phills. Por ejemplo,
--    take 15 unidades == [2,7,2,7,2,7,2,7,2,7,2,7,2,7,2]
-- ---------------------------------------------------------------------

unidades :: [Integer] 
unidades = map (`mod` 10) phills2 

-- ---------------------------------------------------------------------
-- Ejercicio 3.5. Definir, usando unidades, la propiedad 
--    propPhill :: Int -> Bool
-- tal que (propPhill n) se verifica si el término n-ésimo de la
-- sucesión de Phill termina en 2 o en 7 según n sea par o impar.
--
-- Comprobar la propiedad para los 500 primeros términos.
-- ---------------------------------------------------------------------

propPhill :: Int -> Bool
propPhill n | even n    = unidades !! n == 2
            | otherwise = unidades !! n == 7

-- La comprobación es
--    ghci> and [propPhill n |n <- [0 .. 499]]
--    True
