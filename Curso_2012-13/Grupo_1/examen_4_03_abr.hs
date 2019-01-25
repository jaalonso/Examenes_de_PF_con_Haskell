-- Informática (1º del Grado en Matemáticas, Grupo 1)
-- 4º examen de evaluación continua (3 de abril de 2013)
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Se denomina resto de una lista a una sublista no vacia
-- formada el último o últimos elementos. Por ejemplo, [3,4,5] es un
-- resto de lista [1,2,3,4,5].
--      
-- Definir la función 
--    restos :: [a] -> [[a]]  
-- tal que (restos xs) es la lista de los restos de la lista xs. Por
-- ejemplo, 
--    restos [2,5,6]  ==  [[2,5,6],[5,6],[6]]  
--    restos [4,5]    ==  [[4,5],[5]]  
--    restos []       ==  []  
-- ---------------------------------------------------------------------
  
restos :: [a] -> [[a]]  
restos []     = []  
restos (x:xs) = (x:xs) : restos xs  
  
-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Se denomina corte de una lista a una sublista no vacía 
-- formada por el primer elemento y los siguientes hasta uno dado.    
-- Por ejemplo, [1,2,3] es un corte de [1,2,3,4,5].   
-- 
-- Definir, por recursión, la función 
--    cortesR :: [a] -> [[a]]   
-- tal que (cortesR xs) es la lista de los cortes de la lista xs. Por
-- ejemplo,   
--    cortesR []         ==  []   
--    cortesR [2,5]      ==  [[2],[2,5]]   
--    cortesR [4,8,6,0]  ==  [[4],[4,8],[4,8,6],[4,8,6,0]]  
-- ---------------------------------------------------------------------

-- 1ª definición:  
cortesR :: [a] -> [[a]]   
cortesR []     = []   
cortesR (x:xs) = [x]: [x:y | y <- cortesR xs]   
   
-- 2ª definición:  
cortesR2 :: [a] -> [[a]]   
cortesR2 []     = []   
cortesR2 (x:xs) = [x] : map (\y -> x:y) (cortesR2 xs)   
  
-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir, por composición, la función 
--    cortesC :: [a] -> [[a]]   
-- tal que (cortesC xs) es la lista de los cortes de la lista xs. Por
-- ejemplo,   
--    cortesC []         ==  []   
--    cortesC [2,5]      ==  [[2],[2,5]]   
--    cortesC [4,8,6,0]  ==  [[4],[4,8],[4,8,6],[4,8,6,0]]  
-- ---------------------------------------------------------------------

cortesC :: [a] -> [[a]]  
cortesC = reverse . map reverse . restos . reverse  
  
-- ---------------------------------------------------------------------
-- Ejercicio 2. Los árboles binarios se pueden representar con el de
-- dato algebraico 
--    data Arbol = H Int   
--               | N Arbol Int Arbol  
--               deriving (Show, Eq)  
-- Por ejemplo, los árboles 
--         9                9                
--        / \              / \    
--       /   \            /   \   
--      8     6          7     3 
--     / \   / \        / \   / \ 
--    3   2 4   5      3   2 4   7
-- se pueden representar por
--    ej1, ej2:: Arbol
--    ej1 = N (N (H 3) 8 (H 2)) 9 (N (H 4) 6 (H 5))
--    ej2 = N (N (H 3) 7 (H 2)) 9 (N (H 4) 3 (H 7))
-- 
-- Decimos que un árbol binario es par si la mayoría de sus nodos son
-- pares e impar en caso contrario. Por ejemplo, el primer ejemplo es
-- par y el segundo es impar.
-- 
-- Para representar la paridad se define el tipo Paridad
--    data Paridad = Par | Impar deriving Show  
-- 
-- Definir la función 
--    paridad :: Arbol -> Paridad
-- tal que (paridad a) es la paridad del árbol a. Por ejemplo,
--    paridad ej1  ==  Par
--    paridad ej2  ==  Impar
-- ---------------------------------------------------------------------

data Arbol = H Int   
           | N Arbol Int Arbol  
           deriving (Show, Eq)  

ej1, ej2:: Arbol
ej1 = N (N (H 3) 8 (H 2)) 9 (N (H 4) 6 (H 5))
ej2 = N (N (H 3) 7 (H 2)) 9 (N (H 4) 3 (H 7))

data Paridad = Par | Impar deriving Show  
 
paridad :: Arbol -> Paridad
paridad a | x > y     = Par 
          | otherwise = Impar
          where (x,y) = paridades a

-- (paridades a) es un par (x,y) donde x es el número de valores pares
-- en el árbol a e i es el número de valores impares en el árbol a. Por
-- ejemplo,  
--    paridades ej1  ==  (4,3)
--    paridades ej2  ==  (2,5)
paridades :: Arbol -> (Int,Int)
paridades (H x) | even x    = (1,0)
                | otherwise = (0,1)
paridades (N i x d) | even x    = (1+a1+a2,b1+b2)
                    | otherwise = (a1+a2,1+b1+b2)
                    where (a1,b1) = paridades i
                          (a2,b2) = paridades d          

-- ---------------------------------------------------------------------
-- Ejercicio 3. Según la Wikipedia, un número feliz se define por el
-- siguiente proceso. Se comienza reemplazando el número por la suma del
-- cuadrado de sus cifras y se repite el proceso hasta que se obtiene el
-- número 1 o se entra en un ciclo que no contiene al 1. Aquellos
-- números para los que el proceso termina en 1 se llaman números
-- felices y los que entran en un ciclo sin 1 se llaman números
-- desgraciados. 
-- 
-- Por ejemplo, 7 es un número feliz porque
--       7 ~> 7^2                          =  49
--         ~> 4^2 + 9^2       = 16 + 81    =  97
--         ~> 9^2 + 7^2       = 81 + 49    = 130
--         ~> 1^2 + 3^2 + 0^2 =  1 + 9 + 0 =  10
--         ~> 1^2 + 0^2       =  1 + 0     =   1
-- Pero 17 es un número desgraciado porque
--    17 ~> 1^2 + 7^2       =  1 + 49      =  50
--       ~> 5^2 + 0^2       = 25 +  0      =  25
--       ~> 2^2 + 5^2       =  4 + 25      =  29
--       ~> 2^2 + 9^2       =  4 + 81      =  85
--       ~> 8^2 + 5^2       = 64 + 25      =  89
--       ~> 8^2 + 9^2       = 64 + 81      = 145
--       ~> 1^2 + 4^2 + 5^2 =  1 + 16 + 25 =  42
--       ~> 4^2 + 2^2       = 16 +  4      =  20
--       ~> 2^2 + 0^2       =  4 +  0      =   4
--       ~> 4^2                            =  16
--       ~> 1^2 + 6^2       = 1 + 36       =  37
--       ~> 3^2 + 7^2       = 9 + 49       =  58
--       ~> 5^2 + 8^2       = 25 + 64      =  89
-- que forma un bucle al repetirse el 89.
-- 
-- El objetivo del ejercicio es definir una función que calcule todos
-- los números felices hasta un límite dado. 
-- --------------------------------------------------------------------- 

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función  
--    sumaCuadrados :: Int -> Int
-- tal que (sumaCuadrados n) es la suma de los cuadrados de los dígitos
-- de n. Por ejemplo,
--    sumaCuadrados 145  ==  42
-- ---------------------------------------------------------------------   

sumaCuadrados :: Int -> Int
sumaCuadrados n = sum [x^2 | x <- digitos n]

-- (digitos n) es la lista de los dígitos de n. Por ejemplo,
--    digitos 145  ==  [1,4,5]
digitos :: Int -> [Int]  
digitos n = [read [x]|x<-show n] 

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    caminoALaFelicidad :: Int -> [Int]
-- tal que (caminoALaFelicidad n) es la lista de los números obtenidos
-- en el proceso de la determinación si n es un número feliz: se
-- comienza con la lista [n], ampliando la lista con la suma del
-- cuadrado de las cifras de su primer elemento y se repite el proceso
-- hasta que se obtiene el número 1 o se entra en un ciclo que no
-- contiene al 1. Por ejemplo,
--    ghci> take 20 (caminoALaFelicidad 7)
--    [7,49,97,130,10,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
--    ghci> take 20 (caminoALaFelicidad 17)
--    [17,50,25,29,85,89,145,42,20,4,16,37,58,89,145,42,20,4,16,37]
-- ---------------------------------------------------------------------

caminoALaFelicidad :: Int -> [Int]
caminoALaFelicidad n = 
    n : [sumaCuadrados x | x <- caminoALaFelicidad n]

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. En el camino a la felicidad, pueden ocurrir dos casos:
-- + aparece un 1 y a continuación solo aparece 1,
-- + llegamos a 4 y se entra en el ciclo 4,16,37,58,89,145,42,20.
-- 
-- Definir la función 
--    caminoALaFelicidadFundamental :: Int -> [Int]
-- tal que (caminoALaFelicidadFundamental n) es el camino de la
-- felicidad de n hasta que aparece un 1 o un 4. Por ejemplo,  
--    caminoALaFelicidadFundamental 34    == [34,25,29,85,89,145,42,20,4]  
--    caminoALaFelicidadFundamental 203   == [203,13,10,1] 
--    caminoALaFelicidadFundamental 23018 == [23018,78,113,11,2,4]
-- ---------------------------------------------------------------------

caminoALaFelicidadFundamental :: Int -> [Int]
caminoALaFelicidadFundamental n = selecciona (caminoALaFelicidad n)
  
-- (selecciona xs) es la lista de los elementos hasta que aparece un 1 o
-- un 4. Por ejemplo,
--    selecciona [3,2,1,5,4]  ==  [3,2,1]
--    selecciona [3,2]        ==  [3,2]
selecciona [] = []
selecciona (x:xs) | x == 1 || x == 4 = [x]
                  | otherwise        = x : selecciona xs
 
-- ---------------------------------------------------------------------
-- Ejercicio 3.4. Definir la función 
--    esFeliz :: Int -> Bool
-- tal que (esFeliz n) s verifica si n es feliz. Por ejemplo, 
--    esFeliz  7  ==  True
--    esFeliz 17  ==  False
-- ---------------------------------------------------------------------
 
esFeliz :: Int -> Bool
esFeliz n = last (caminoALaFelicidadFundamental n) == 1  

-- ---------------------------------------------------------------------
-- Ejercicio 3.5. Comprobar con QuickCheck que si n es es feliz,
-- entonces todos los números de (caminoALaFelicidadFundamental n)
-- también lo son. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_esFeliz :: Int -> Property
prop_esFeliz n = 
    n>0 && esFeliz n 
    ==> and [esFeliz x | x <- caminoALaFelicidadFundamental n]

-- La comprobación es
--    ghci> quickCheck prop_esFeliz
--    *** Gave up! Passed only 38 tests.
