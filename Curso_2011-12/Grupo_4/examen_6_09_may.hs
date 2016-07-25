-- Informática (1º del Grado en Matemáticas, Grupo 4)
-- 6º examen de evaluación continua (7 de mayo de 2012)
-- ---------------------------------------------------------------------

import Data.List
import Data.Array
import Test.QuickCheck
import PolRepTDA
import TablaConMatrices

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    aplicaT :: (Ix a, Num b) => Array a b -> (b -> c) -> Array a c
-- tal que (aplicaT t f) es la tabla obtenida aplicado la función f a
-- los elementos de la tabla t. Por ejemplo,
--    ghci> aplicaT (array (1,5) [(1,6),(2,3),(3,-1),(4,9),(5,20)]) (+1)
--    array (1,5) [(1,7),(2,4),(3,0),(4,10),(5,21)]
--    ghci> :{
--    *Main| aplicaT (array ((1,1),(2,3)) [((1,1),3),((1,2),-1),((1,3),0),
--    *Main|                               ((2,1),0),((2,2),0),((2,3),-1)])
--    *Main|         (*2)
--    *Main| :}
--    array ((1,1),(2,3)) [((1,1),6),((1,2),-2),((1,3),0),
--                         ((2,1),0),((2,2),0),((2,3),-2)]
-- ---------------------------------------------------------------------

aplicaT :: (Ix a, Num b) => Array a b -> (b -> c) -> Array a c
aplicaT t f = listArray (bounds t) [f e | e <- elems t]

-- ---------------------------------------------------------------------
-- Ejercicio 2. En este ejercicio se usará el TAD de polinomios (visto
-- en el tema 21) y el de tabla (visto en el tema 18). Para ello, se
-- importan la librerías PolRepTDA y TablaConMatrices.
-- 
-- Definir la función 
--    polTabla :: Num a => Polinomio a -> Tabla Integer a
-- tal que (polTabla p) es la tabla con los grados y coeficientes de los
-- términos del polinomio p; es decir, en la tabla el valor del índice n
-- se corresponderá con el coeficiente del grado n del mismo
-- polinomio. Por ejemplo, 
--    ghci> polTabla (consPol 5 2 (consPol 3 (-1) polCero))
--    Tbl (array (0,5) [(0,0),(1,0),(2,0),(3,-1),(4,0),(5,2)])
-- -- ---------------------------------------------------------------------

polTabla :: Num a => Polinomio a -> Tabla Integer a
polTabla p = tabla (zip [0..] [coeficiente c p | c <- [0..grado p]])

-- (coeficiente k p) es el coeficiente del término de grado k
-- del polinomio p. Por ejemplo,
--    let pol = consPol 5 2 (consPol 3 1 (consPol 0 (-1) polCero))
--    pol                ==  2*x^5 + x^3 + -1
--    coeficiente 5 pol  ==  2
--    coeficiente 6 pol  ==  0
--    coeficiente 4 pol  ==  0
--    coeficiente 3 pol  ==  1
coeficiente :: Num a => Int -> Polinomio a -> a
coeficiente k p | k > n     = 0
                | k == n    = c
                | otherwise = coeficiente k r
                where n = grado p
                      c = coefLider p
                      r = restoPol p

-- ---------------------------------------------------------------------
-- Ejercicio 3. Diremos que una matriz es creciente si para toda
-- posición (i,j), el valor de dicha posición es menor o igual que los
-- valores en las posiciones adyacentes de índice superior, es decir,
-- (i+1,j), (i,j+1) e (i+1,j+1) siempre y cuando dichas posiciones
-- existan en la matriz. 
--
-- Definir la función 
--    matrizCreciente :: (Num a,Ord a) =>  Array (Int,Int) a -> Bool
-- tal que (matrizCreciente p) se verifica si la matriz p es
-- creciente. Por ejemplo, 
--    matrizCreciente p1 == True
--    matrizCreciente p2 == False
-- donde las matrices p1 y p2 están definidas por
--    p1, p2 :: Array (Int,Int) Int
--    p1 = array ((1,1),(3,3)) [((1,1),1),((1,2),2),((1,3),3),
--                              ((2,1),2),((2,2),3),((2,3),4),
--                              ((3,1),3),((3,2),4),((3,3),5)]
--    p2 = array ((1,1),(3,3)) [((1,1),1),((1,2),2),((1,3),3),
--                              ((2,1),2),((2,2),1),((2,3),4),
--                              ((3,1),3),((3,2),4),((3,3),5)]
-- ---------------------------------------------------------------------

p1, p2 :: Array (Int,Int) Int
p1 = array ((1,1),(3,3)) [((1,1),1),((1,2),2),((1,3),3),
                          ((2,1),2),((2,2),3),((2,3),4),
                          ((3,1),3),((3,2),4),((3,3),5)]
p2 = array ((1,1),(3,3)) [((1,1),1),((1,2),2),((1,3),3),
                          ((2,1),2),((2,2),1),((2,3),4),
                          ((3,1),3),((3,2),4),((3,3),5)]
     
matrizCreciente :: (Num a,Ord a) =>  Array (Int,Int) a -> Bool
matrizCreciente p = 
    and ([p!(i,j) <= p!(i,j+1)   | i <- [1..m],   j <- [1..n-1]] ++
         [p!(i,j) <= p!(i+1,j)   | i <- [1..m-1], j <- [1..n]] ++
         [p!(i,j) <= p!(i+1,j+1) | i <- [1..m-1], j <- [1..n-1]])
    where (m,n) = snd (bounds p)

-- ---------------------------------------------------------------------
-- Ejercicio 4. Partiremos de la siguiente definición para el tipo de
-- datos de árbol binario:
--    data Arbol = H
--               | N Int Arbol Arbol
--               deriving Show
--     
-- Diremos que un árbol está balanceado si para cada nodo v la
-- diferencia entre el número de nodos (con valor) de sus ramas
-- izquierda y derecha es menor o igual que uno. 
--
-- Definir la función 
--    balanceado :: Arbol -> Bool
-- tal que (balanceado a) se verifica si el árbol a está
-- balanceado. Por ejemplo,
--    balanceado (N 5 (N 1 (N 5 H H) H) (N 4 H (N 5 H H))) == True
--    balanceado (N 1 (N 2 (N 3 H H) H) H)                 == False
-- ---------------------------------------------------------------------

data Arbol = H
           | N Int Arbol Arbol
           deriving Show

balanceado :: Arbol -> Bool
balanceado H = True
balanceado (N _ i d) = abs (numeroNodos i - numeroNodos d) <= 1 

-- (numeroNodos a) es el número de nodos del árbol a. Por ejemplo,
--    numeroNodos (N 7 (N 1 (N 7 H H) H) (N 4 H (N 7 H H)))  ==  5
numeroNodos :: Arbol -> Int
numeroNodos H         = 0
numeroNodos (N _ i d) = 1 + numeroNodos i + numeroNodos d

-- ---------------------------------------------------------------------
-- Ejercicio 5. Hemos hecho un estudio en varias agencias de viajes
-- analizando las ciudades para las que se han comprado billetes de
-- avión en la última semana. Las siguientes listas muestran ejemplos de
-- dichos listados, donde es necesario tener en cuenta que en la misma
-- lista se puede repetir la misma ciudad en más de una ocasión, en cuyo
-- caso el valor total será la suma acumulada. A continuación se
-- muestran algunas de dichas listas:
--    lista1, lista2, lista3, lista4 :: [(String,Int)]
--    lista1 = [("Paris",17),("Londres",12),("Roma",21),("Atenas",16)]
--    lista2 = [("Roma",5),("Paris",4)]
--    lista3 = [("Atenas",2),("Paris",11),("Atenas",1),("Paris",5)]
--    lista4 = [("Paris",5),("Roma",5),("Atenas",4),("Londres",6)]
-- 
-- Definir la función
--    ciudadesOrdenadas :: [[(String,Int)]] -> [String]
-- tal que (ciudadesOrdenadas ls) es la lista de los nombres de ciudades
-- ordenadas según el número de visitas (de mayor a menor). Por ejemplo,
--    ghci> ciudadesOrdenadas [lista1]
--    ["Roma","Paris","Atenas","Londres"]
--    ghci> ciudadesOrdenadas [lista1,lista2,lista3,lista4]
--    ["Paris","Roma","Atenas","Londres"]
-- ---------------------------------------------------------------------

lista1, lista2, lista3, lista4 :: [(String,Int)]
lista1 = [("Paris",17),("Londres",12),("Roma",21),("Atenas",16)]
lista2 = [("Roma",5),("Paris",4)]
lista3 = [("Atenas",2),("Paris",11),("Atenas",1),("Paris",5)]
lista4 = [("Paris",5),("Roma",5),("Atenas",4),("Londres",6)]

ciudadesOrdenadas :: [[(String,Int)]] -> [String]
ciudadesOrdenadas ls = [c | (c,v) <- ordenaLista (uneListas ls)]

-- (uneListas ls) es la lista obtenida uniendo las listas ls y
-- acumulando los resultados. Por ejemplo,
--    ghci> uneListas [lista1,lista2]
--    [("Paris",21),("Londres",12),("Roma",26),("Atenas",16)]
uneListas :: [[(String,Int)]] -> [(String,Int)]
uneListas ls = acumulaLista (concat ls)

-- (acumulaLista cvs) es la lista obtenida acumulando el número de
-- visitas de la lista cvs. Por ejemplo,
--    acumulaLista lista3  ==  [("Atenas",3),("Paris",16)]
acumulaLista :: [(String,Int)] -> [(String,Int)] 
acumulaLista cvs =
    [(c,sum [t | (c',t) <- cvs, c' == c]) | c <- nub (map fst cvs)]

-- (ordenaLista cvs9 es la lista de los elementos de cvs ordenados por
-- el número de visitas (de mayor a menor). Por ejemplo,
--    ghci> ordenaLista lista1
--    [("Roma",21),("Paris",17),("Atenas",16),("Londres",12)]
ordenaLista :: [(String,Int)] -> [(String,Int)]
ordenaLista cvs =
    reverse [(c,v) | (v,c) <- sort [(v',c') | (c',v') <- cvs]]
